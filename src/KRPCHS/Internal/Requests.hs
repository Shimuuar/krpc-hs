{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module KRPCHS.Internal.Requests (
    -- * Monadic API for KRPC
    KRPC(..)
  , runKRPC
    -- ** Internals
  , Accum(..)
  , RespParser(..)
  , parseSingle
  , runRespParser
    -- ** Networking
  , RPCClient
  , withRPCClient
    -- * RPC API
  , RpcCall(..)
  , MonadRPC(..)
    -- * Serialization and deserialization
  , KRPCResponseExtractable(..)
  , checkError
  , makeArgument
  , makeRequest
    -- * Networking
  --   -- * Stream client & stream primitives
  -- -- , StreamClient
  -- -- , withStreamClient
  -- -- , KRPCStream(..)
  -- -- , KRPCStreamReq(..)
  -- -- , KRPCStreamMsg(..)
  -- -- , emptyKRPCStreamMsg
  -- -- , makeStream
  -- -- , requestStream
  -- -- , extractStreamMessage
  -- -- , extractStreamResponse
  -- -- , getStreamMessage
  ) where

import Control.Monad.Catch  (MonadThrow(..),MonadCatch,MonadMask,bracket,try)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Exception    (SomeException(..),AsyncException(..),Exception(..),throw)
import Control.Concurrent

import Network.Socket

import KRPCHS.Internal.ProtocolError
import KRPCHS.Internal.NetworkUtils
import KRPCHS.Internal.SerializeUtils

import qualified PB.KRPC.Argument        as KArg
import qualified PB.KRPC.Request         as KReq
import qualified PB.KRPC.ProcedureCall   as KReq
import qualified PB.KRPC.Response        as KRes
import qualified PB.KRPC.ProcedureResult as KPRes
import qualified PB.KRPC.ConnectionResponse        as KRPC
import qualified PB.KRPC.ConnectionResponse.Status as KRPC.Status

-- import qualified PB.KRPC.StreamMessage   as KStreamMsg
-- import qualified PB.KRPC.StreamResponse  as KStreamRes

import Data.Int
import Data.Word
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Map  as M
import qualified Data.Set      as Set
import qualified Data.Sequence as Seq
import           Data.Sequence   (Seq)

import qualified Data.ByteString.Lazy  as BL
import qualified Text.ProtocolBuffers  as P

----------------------------------------------------------------
-- Monadic API for KRPC
----------------------------------------------------------------

-- | Monad transformer for making RPC calls.
--
--   It makes attempt to use new feature in KRPC4 which allows to
--   bundle several call into single network request. Applicative
--   interface will accumulate requests and will send them in single
--   batch while monadic API will send them one by one. To make use of
--   batching of request enable @ApplicativeDo@ extension.
data KRPC m a = Immediate (RPCClient -> m a)
              | Batched   (RPCClient -> m (Accum a))
              deriving (Functor)

-- | Applicative builds batches and will evaluate them all at once
instance Applicative m => Applicative (KRPC m) where
  pure = Immediate . pure . pure
  Immediate fun <*> Immediate val = Immediate $ \c ->
    fun c <*> val c
  Immediate fun <*> Batched bat   = Batched   $ \c ->
    fmap <$> fun c <*> bat c
  Batched fun   <*> Immediate bat = Batched   $ \c ->
    (\acc a -> fmap ($ a) acc) <$> fun c <*> bat c
  Batched fun   <*> Batched bat   = Batched   $ \c ->
    (<*>) <$> fun c <*> bat c

-- | Monadic bind sequentialize RPC call which means call will be
--   performed
instance (MonadIO m, MonadThrow m) => Monad (KRPC m) where
  return  = pure
  m >>= f = Immediate $ \c -> do
    a <- forceRpcCall c m
    forceRpcCall c (f a)

-- | Perform all pending RPC calls
forceRpcCall :: (MonadIO m, MonadThrow m) => RPCClient -> KRPC m a -> m a
forceRpcCall c (Immediate m) = m c
forceRpcCall c (Batched   m) = do
  Accum xs p <- m c
  resp <- liftIO $ sendRequest c xs
  case KRes.error resp of
    Just  e -> throwM (KRPCError (show e))
    Nothing -> return ()
  case runRespParser p (KRes.results resp) of
    Right a -> return a
    Left  e -> throwM e

instance (MonadIO m, MonadThrow m) => MonadIO (KRPC m) where
  liftIO = Immediate . const . liftIO

instance MonadTrans KRPC where
  lift = Immediate . const

-- Accumulator for requests. Basically it's pair of list of requests
-- and parser for corresponding requests
data Accum a = Accum !(Seq KReq.ProcedureCall) (RespParser a)
               deriving (Functor)

instance Applicative Accum where
  pure x = Accum Seq.empty (pure x)
  Accum xs pX <*> Accum ys pY = Accum (xs <> ys) (pX <*> pY)

-- Very simple parser for requests
newtype RespParser a = RespParser
  { unRespParser :: StateT (Seq KPRes.ProcedureResult) (Either ProtocolError) a }
  deriving (Functor,Applicative,Monad)

-- Parse single field of responce
parseSingle :: KRPCResponseExtractable a => RespParser a
parseSingle = RespParser $ do
  s <- get
  case Seq.viewl s of
    Seq.EmptyL  -> throw (ResponseParserFailed "Not enough responces!")
    x Seq.:< xs -> put xs >> lift (extract x)

-- Evaluate response parser. It throws exception if there's not enough
-- input
runRespParser
  :: RespParser a -> Seq KPRes.ProcedureResult -> Either ProtocolError a
runRespParser fun xs =
  case runStateT (unRespParser fun) xs of
    Right (a,ys) | Seq.null ys -> Right a
                 | otherwise   -> throw (ResponseParserFailed "Too much responces")
    Left  e                    -> Left e

-- | Run KRPC program
runKRPC :: (MonadMask m, MonadIO m)
        => String               -- ^ Name to send to kRPC
        -> HostName             -- ^ Hostname to connect to
        -> ServiceName          -- ^ Port to connect to
        -> KRPC m a             -- ^ KRPC program
        -> m a
runKRPC name host port program = withRPCClient name host port $ \c -> do
  forceRpcCall c program



----------------------------------------------------------------
-- RPC API
----------------------------------------------------------------

-- |  Prepared  RPC call  it's  tagged  by  expected return  type.  To
--    actually perform call use 'call'.
newtype RpcCall a = RpcCall KReq.ProcedureCall
                    deriving (Show)

-- | Monad in which one could perform RPC calls
class (MonadIO m) => MonadRPC m where
  call :: KRPCResponseExtractable a => RpcCall a -> m a

instance (MonadIO m, MonadThrow m) => MonadRPC (KRPC m) where
  call (RpcCall req) = Batched $ \_ ->
    pure (Accum (Seq.singleton req) parseSingle)


----------------------------------------------------------------
-- Networking
----------------------------------------------------------------

-- | Connection to KRPC.
--
--   Note that to avoid possibility of message fragmentation network
--   IO is done from separate thread.
data RPCClient = RPCClient
  { rpcChan  :: Chan ChanRequest
  , clientId :: BL.ByteString
  }

-- | Connect to RPC server
withRPCClient
  :: (MonadMask m, MonadIO m)
  => String                     -- ^ Name to send to kRPC
  -> HostName                   -- ^ Hostname to connect to
  -> ServiceName                -- ^ Port to connect to
  -> (RPCClient -> m a)         -- ^ Action to perform
  -> m a
withRPCClient name host port action = withSocket host port $ \sock -> do
  -- Perform RPC handshake
  cid <- liftIO $ rpcHandshake sock name
  -- Fork off worker process for network IO (It's done inside bracket
  -- to ensure that we don't leak p
  ch <- liftIO newChan
  bracket
    (liftIO $ forkIOWithUnmask ($ networkIOWorker sock ch))
    (liftIO . killThread)
    (\_ -> action RPCClient { rpcChan  = ch
                            , clientId = cid
                            })

-- | Send request to RPC server and receive reply
sendRequest :: RPCClient -> Seq KReq.ProcedureCall -> IO KRes.Response
sendRequest c req = do
  let ch = rpcChan c
  var <- newEmptyMVar
  writeChan ch (req,var)
  takeMVar var >>= \case
    Left  e -> throwM e
    Right r -> return r

type ChanRequest = ( Seq KReq.ProcedureCall
                   , MVar (Either SomeException KRes.Response))

-- Worker process for network IO. To send request to RPC server we
-- push message to Chan alongside with empty MVar. Worker perform
-- query and puts query result in the MVar. If network exception
-- happens during IO it's sent back to parent thread
networkIOWorker :: Socket -> Chan ChanRequest -> IO ()
networkIOWorker sock ch = forever $ do
  (req,var) <- readChan ch
  rsp <- try $ do
    sendMsg sock (KReq.Request req)
    recvResponse sock
  -- We need to special-case ThreadKilled exception to make thread
  -- killable
  case rsp of
    Left e | Just ThreadKilled <- fromException e
           -> throwM ThreadKilled
    _      -> putMVar var rsp

-- Get response from RPC server
recvResponse :: (P.Wire a, P.ReflectDescriptor a) => Socket -> IO a
recvResponse sock = do
    msg <- recvMsg sock
    case msg of
      Left  e -> throwM e
      Right a -> return a
    -- either (throwM) (return) (messageGet (BL.fromStrict msg))

-- Perform IO operation using socket. It's closed after action whether
-- upon normal termination of because of exception
withSocket :: (MonadMask m, MonadIO m)
           => HostName -> ServiceName -> (Socket -> m a) -> m a
withSocket host port action
  = bracket initS fini body
  where
    initS = liftIO $do
      -- FIXME: do something more sensible!
      addr:_ <- getAddrInfo Nothing (Just host) (Just port)
      sock   <- socket AF_INET Stream defaultProtocol
      return (sock,addr)
    fini (sock,_)    = liftIO $ close sock
    body (sock,addr) = do
      liftIO $ connect sock (addrAddress addr)
      action sock

rpcHandshake :: Socket -> String -> IO BL.ByteString
rpcHandshake sock name = do
    sendMsg sock (connectRpcMsg name)
    resp <- recvMsg sock
    either (fail . show) (extractId) resp
  where
    extractId KRPC.ConnectionResponse{..} = case status of
        Just KRPC.Status.OK -> return $ fromJust client_identifier
        Just err            -> fail   $ show err ++ " - details: '" ++ show (unpackUtf8String <$> message) ++ "'"
        Nothing             -> return $ fromJust client_identifier



----------------------------------------------------------------
-- Streaming client
----------------------------------------------------------------
{-
-- | Client for stream request
data StreamClient = StreamClient { streamSocket :: Socket }

newtype KRPCStream a = KRPCStream { streamId :: Int }
    deriving (Show)

newtype KRPCStreamReq a = KRPCStreamReq { streamReq :: KReq.Request }
    deriving (Show)

newtype KRPCStreamMsg = KRPCStreamMsg { streamMsg :: M.Map Int KRes.Response }
    deriving (Show)

emptyKRPCStreamMsg :: KRPCStreamMsg
emptyKRPCStreamMsg = KRPCStreamMsg M.empty


-- | Perform IO action using stream client. Client will be shut down
--   after action is completed either normally of abnormally.
withStreamClient
  :: RPCClient                  -- ^ Connect client
  -> HostName                   -- ^ Host name
  -> ServiceName                -- ^ Port number
  -> (StreamClient -> IO a)     -- ^ Action to perform
  -> IO a
withStreamClient RPCClient{..} host port func =
    withSocket host port $ \sock -> do
        streamHandshake sock clientId
        func (StreamClient sock)

makeStream :: KReq.Request -> KRPCStreamReq a
makeStream r = KRPCStreamReq $
    makeRequest "KRPC" "AddStream" [KArg.Argument (Just 0) (Just $ messagePut r)]

requestStream :: (MonadRPC m, MonadIO m, MonadThrow m) => KRPCStreamReq a -> m (KRPCStream a)
requestStream KRPCStreamReq{..} = do
    res <- sendRequest streamReq
    sid <- processResponse res
    return (KRPCStream sid)

getStreamMessage :: MonadIO m => StreamClient -> m KRPCStreamMsg
getStreamMessage StreamClient{..} = unpackStreamMsg <$> liftIO (recvResponse streamSocket)
  where
    unpackStreamMsg res = KRPCStreamMsg $ M.fromList (extractStreamMessage res)

extractStreamResponse :: KStreamRes.StreamResponse -> Maybe (Int, KRes.Response)
extractStreamResponse streamRes = do
    sid <- KStreamRes.id       streamRes
    res <- KStreamRes.response streamRes
    return (fromIntegral sid, res)

extractStreamMessage :: KStreamMsg.StreamMessage -> [(Int, KRes.Response)]
extractStreamMessage msg = mapMaybe extractStreamResponse responseList
    where responseList = Data.Foldable.toList (KStreamMsg.responses msg)

-- Perform handshake with stream server
streamHandshake :: Socket -> BS.ByteString -> IO ()
streamHandshake sock clientId = do
    sendAll sock helloStreamMsg
    sendAll sock clientId
    res <- recvN sock 2
    case res of
        "OK" -> return ()
        _    -> fail "Could not handshake with stream server"
-}


----------------------------------------------------------------
-- Monad API
----------------------------------------------------------------

-- -- | Reader monad which uses RPCClient as context.
-- newtype RPCContextT m a = RPCContextT { runRPCContextT :: ReaderT RPCClient m a }
--   deriving ( Functor, Applicative, Monad, MonadIO
--            , MonadReader RPCClient, MonadThrow, MonadCatch, MonadMask)

-- runRPCProg :: Monad m => RPCClient -> RPCContextT m a -> m a
-- runRPCProg client ctx = runReaderT (runRPCContextT ctx) client

-- instance Monad m => MonadRPC (RPCContextT m) where
  -- askClient = ask



----------------------------------------------------------------
-- Decoding of messages
----------------------------------------------------------------

class (PbSerializable a) => KRPCResponseExtractable a where
    extract :: KPRes.ProcedureResult -> Either ProtocolError a
    extract r = do
        checkError r
        maybe (Left ResponseEmpty) decodePb (KPRes.value r)

instance KRPCResponseExtractable Bool
instance KRPCResponseExtractable Float
instance KRPCResponseExtractable Double
instance KRPCResponseExtractable Int
instance KRPCResponseExtractable Int32
instance KRPCResponseExtractable Int64
instance KRPCResponseExtractable Word32
instance KRPCResponseExtractable Word64
instance KRPCResponseExtractable T.Text
instance (PbSerializable a, PbSerializable b)
         => KRPCResponseExtractable (a, b)
instance (PbSerializable a, PbSerializable b, PbSerializable c)
         => KRPCResponseExtractable (a, b, c)
instance (PbSerializable a, PbSerializable b, PbSerializable c, PbSerializable d)
         => KRPCResponseExtractable (a, b, c, d)

instance KRPCResponseExtractable () where
    extract = checkError

instance (PbSerializable a) => KRPCResponseExtractable [a] where
    extract r = do
        checkError r
        case KPRes.value r of
            Nothing    -> Right []
            Just bytes -> decodePb bytes

instance (Ord a, PbSerializable a) => KRPCResponseExtractable (Set.Set a) where
    extract r = do
        checkError r
        case KPRes.value r of
            Nothing    -> Right Set.empty
            Just bytes -> decodePb bytes


instance (Ord k, PbSerializable k, PbSerializable v
         ) => KRPCResponseExtractable (M.Map k v) where
    extract r = do
        checkError r
        case KPRes.value r of
            Nothing    -> Right M.empty
            Just bytes -> decodePb bytes

checkError :: KPRes.ProcedureResult -> Either ProtocolError ()
checkError r = case KPRes.error r of
  Nothing -> return ()
  Just e  -> Left $ KRPCError $ show e


makeArgument :: (PbSerializable a) => Word32 -> a -> KArg.Argument
makeArgument position arg = KArg.Argument (Just position) (Just $ encodePb arg)

makeRequest :: String -> Word32 -> [KArg.Argument] -> KReq.ProcedureCall
makeRequest serviceId procId params =
  KReq.ProcedureCall
    { service_id   = Nothing
    , procedure_id = Just procId
    , service      = Just (packUtf8String serviceId)
    , procedure    = Nothing
    , arguments    = Seq.fromList params
    }
