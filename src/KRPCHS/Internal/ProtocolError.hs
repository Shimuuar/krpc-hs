module KRPCHS.Internal.ProtocolError
  ( ProtocolError(..)
  , InternalError(..)
  ) where

import Data.Typeable
import Control.Monad.Catch

data ProtocolError
    = UnknownError
    | ResponseEmpty
    | NoSuchStream
    | KRPCError String
    | DecodeFailure String
    deriving (Typeable, Eq)
         
instance Show ProtocolError where
    show UnknownError      = "Unknown"
    show ResponseEmpty     = "Empty response"
    show NoSuchStream      = "No such stream"
    show (KRPCError e)     = "KRPC Error: '" ++ e ++ "'"
    show (DecodeFailure e) = "Decoding failure: " ++ e

instance Exception ProtocolError


data InternalError
  = ResponseParserFailed String
  deriving (Typeable,Show,Eq)

instance Exception InternalError
