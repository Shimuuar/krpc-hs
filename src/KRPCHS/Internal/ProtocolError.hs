module KRPCHS.Internal.ProtocolError
  ( ProtocolError(..)
  , InternalError(..)
  ) where

import Data.Typeable
import Control.Monad.Catch
import PB.KRPC.Error (Error(..))

data ProtocolError
    = UnknownError
    | ResponseEmpty
    | NoSuchStream
    | KRPCError Error
    | DecodeFailure String
    deriving (Typeable, Eq)
         
instance Show ProtocolError where
    show UnknownError      = "Unknown"
    show ResponseEmpty     = "Empty response"
    show NoSuchStream      = "No such stream"
    show (KRPCError e)     = unlines
      [ "KRPC Error: "
      , "  service:     " ++ show (service e)
      , "  name:        " ++ show (name e)
      , "  description: " ++ show (description e)
      ]
    show (DecodeFailure e) = "Decoding failure: " ++ e

instance Exception ProtocolError


data InternalError
  = ResponseParserFailed String
  deriving (Typeable,Show,Eq)

instance Exception InternalError
