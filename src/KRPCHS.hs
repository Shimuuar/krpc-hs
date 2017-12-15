{-# LANGUAGE RecordWildCards #-}
module KRPCHS (
    -- * Monadic API for KRPC
    KRPC
  , runKRPC
    -- * RPC API
  , RpcCall
  , MonadRPC(..)
    -- * Serialization and deserialization
  , KRPCResponseExtractable(..)
  ) where



import KRPCHS.Internal.Requests
import KRPCHS.Internal.ProtocolError

import qualified PB.KRPC.Status   as KRPC
import qualified PB.KRPC.Service  as KRPC
import qualified PB.KRPC.Services as KRPC

import Control.Monad.Catch
import Control.Monad.Reader

import qualified Data.Map as M



-- messageResultsCount :: KRPCStreamMsg -> Int
-- messageResultsCount = M.size . streamMsg


-- messageHasResultFor :: KRPCStream a -> KRPCStreamMsg -> Bool
-- messageHasResultFor KRPCStream{..} KRPCStreamMsg{..} =
--     M.member streamId streamMsg


-- getStreamResult :: (MonadThrow m, KRPCResponseExtractable a) => KRPCStream a -> KRPCStreamMsg -> m a
-- getStreamResult KRPCStream{..} KRPCStreamMsg{..} =
--     maybe (throwM NoSuchStream)
--           (processResponse)
--           (M.lookup streamId streamMsg)


-- addStream :: (MonadRPC m, MonadThrow m, MonadIO m) => KRPCStreamReq a -> m (KRPCStream a)
-- addStream = requestStream


-- removeStream :: (MonadRPC m, MonadThrow m, MonadIO m) => KRPCStream a -> m ()
-- removeStream KRPCStream{..} = do
--     resp <- sendRequest (makeRequest "KRPC" "RemoveStream" [ makeArgument 0 streamId ])
--     processResponse resp


-- withStream :: (MonadMask m, MonadRPC m, MonadIO m) => KRPCStreamReq a -> (KRPCStream a -> m b) -> m b
-- withStream r f = mask $ \restore -> do
--     s <- addStream r
--     restore (f s) `finally` (removeStream s)
