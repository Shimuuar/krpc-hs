{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module KRPCHS.Internal.Requests
( RPCClient(..)
, StreamClient(..)
, RPCContext(..)

, KRPCStream(..)
, KRPCStreamReq(..)
, KRPCStreamMsg(..)
, emptyKRPCStreamMsg

, KRPCResponseExtractable
, extract

, makeArgument
, processResponse
, makeRequest
, sendRequest
, recvResponse
, makeStream
, requestStream
, extractStreamMessage
, extractStreamResponse
) where


import Network.Socket
import Control.Monad.Catch
import Control.Monad.Reader

import KRPCHS.Internal.ProtocolError
import KRPCHS.Internal.NetworkUtils
import KRPCHS.Internal.SerializeUtils

import qualified PB.KRPC.Argument        as KArg
import qualified PB.KRPC.Request         as KReq
import qualified PB.KRPC.Response        as KRes
import qualified PB.KRPC.StreamMessage   as KStreamMsg
import qualified PB.KRPC.StreamResponse  as KStreamRes

import Data.Int
import Data.Word
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map  as M
import qualified Data.Foldable
import qualified Data.Set      as Set
import qualified Data.Sequence as Seq

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL
import qualified Text.ProtocolBuffers  as P


data RPCClient    = RPCClient    { rpcSocket    :: Socket, clientId :: BS.ByteString }
data StreamClient = StreamClient { streamSocket :: Socket }


newtype RPCContext a = RPCContext { runRPCContext :: ReaderT RPCClient IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader RPCClient, MonadThrow, MonadCatch, MonadMask)

newtype KRPCStream a = KRPCStream { streamId :: Int }
    deriving (Show)

newtype KRPCStreamReq a = KRPCStreamReq { streamReq :: KReq.Request }
    deriving (Show)

newtype KRPCStreamMsg = KRPCStreamMsg { streamMsg :: M.Map Int KRes.Response }
    deriving (Show)

emptyKRPCStreamMsg :: KRPCStreamMsg
emptyKRPCStreamMsg = KRPCStreamMsg M.empty


class (PbSerializable a) => KRPCResponseExtractable a where
    extract :: KRes.Response -> Either ProtocolError a
    extract r = do
        checkError r
        maybe (Left ResponseEmpty) (decodePb) (KRes.return_value r)

instance KRPCResponseExtractable Bool
instance KRPCResponseExtractable Float
instance KRPCResponseExtractable Double
instance KRPCResponseExtractable Int
instance KRPCResponseExtractable Int32
instance KRPCResponseExtractable Int64
instance KRPCResponseExtractable Word32
instance KRPCResponseExtractable Word64
instance KRPCResponseExtractable T.Text
instance (PbSerializable a, PbSerializable b)                                     => KRPCResponseExtractable (a, b)
instance (PbSerializable a, PbSerializable b, PbSerializable c)                   => KRPCResponseExtractable (a, b, c)
instance (PbSerializable a, PbSerializable b, PbSerializable c, PbSerializable d) => KRPCResponseExtractable (a, b, c, d)


instance KRPCResponseExtractable () where
    extract = checkError


instance (PbSerializable a) => KRPCResponseExtractable [a] where
    extract r = do
        checkError r
        case (KRes.return_value r) of
            Nothing    -> Right []
            Just bytes -> decodePb bytes


instance (Ord a, PbSerializable a) => KRPCResponseExtractable (Set.Set a) where
    extract r = do
        checkError r
        case (KRes.return_value r) of
            Nothing    -> Right Set.empty
            Just bytes -> decodePb bytes


instance (Ord k, PbSerializable k, PbSerializable v) => KRPCResponseExtractable (M.Map k v) where
    extract r = do
        checkError r
        case (KRes.return_value r) of
            Nothing    -> Right M.empty
            Just bytes -> decodePb bytes


checkError :: KRes.Response -> Either ProtocolError ()
checkError r = case (KRes.has_error r) of
    Just True -> Left (maybe (DecodeFailure "unknown reason") (KRPCError . P.toString) (KRes.error r))
    _         -> return ()


processResponse :: (KRPCResponseExtractable a) => KRes.Response -> RPCContext a
processResponse res = either (throwM) (return) (extract res)


sendRequest :: KReq.Request -> RPCContext KRes.Response
sendRequest r = do
    sock <- asks rpcSocket
    liftIO $ sendMsg sock r
    liftIO $ recvResponse sock


recvResponse :: (P.Wire a, P.ReflectDescriptor a) => Socket -> IO a
recvResponse sock = do
    msg <- recvMsg sock
    either (throwM) (return) (messageGet (BL.fromStrict msg))


makeArgument :: (PbSerializable a) => P.Word32 -> a -> KArg.Argument
makeArgument position arg = KArg.Argument (Just position) (Just $ encodePb arg)


makeRequest :: String -> String -> [KArg.Argument] -> KReq.Request
makeRequest serviceName procName params =
    KReq.Request
    { service   = Just $ P.fromString serviceName
    , procedure = Just $ P.fromString procName
    , arguments = Seq.fromList params }


makeStream :: KReq.Request -> KRPCStreamReq a
makeStream r = KRPCStreamReq $
    makeRequest "KRPC" "AddStream" [KArg.Argument (Just 0) (Just $ messagePut r)]


requestStream :: KRPCStreamReq a -> RPCContext (KRPCStream a)
requestStream KRPCStreamReq{..} = do
    res <- sendRequest streamReq
    sid <- processResponse res
    return (KRPCStream sid)


extractStreamResponse :: KStreamRes.StreamResponse -> Maybe (Int, KRes.Response)
extractStreamResponse streamRes = do
    sid <- KStreamRes.id       streamRes
    res <- KStreamRes.response streamRes
    return (fromIntegral sid, res)


extractStreamMessage :: KStreamMsg.StreamMessage -> [(Int, KRes.Response)]
extractStreamMessage msg = mapMaybe extractStreamResponse responseList
    where responseList = Data.Foldable.toList (KStreamMsg.responses msg)
