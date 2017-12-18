{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Text (Text)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           Data.Monoid
import           Data.Maybe
import           Data.Foldable
import           Data.String.Interpolate
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Text as T
import System.Environment (getArgs)
import TH

----------------------------------------------------------------
data Module = Module
  { modId            :: Int
  , modDocumentation :: Text
  , modProcedures    :: HM.HashMap Text Procedure
  , modClasses       :: HM.HashMap Text Class
  , modEnumerations  :: HM.HashMap Text Enumeration
  , modExceptions    :: Value
  }
  deriving (Show,Eq)

data Enumeration = Enumeration
  { enumDocumentation :: Text
  , enumValues        :: [EnumVal]
  }
  deriving (Show,Eq)

data EnumVal = EnumVal
  { evName          :: Text
  , evValue         :: Int
  , evDocumentation :: Text
  }
  deriving (Show,Eq)

data Class = Class
  { clsDocumentation :: Text
  }
  deriving (Show,Eq)  

data Procedure = Procedure
  { procId                 :: Int
  , procParameters         :: [Param]
  , procReturn_type        :: Maybe Type
  , procReturn_is_nullable :: Maybe Bool
  , procDocumentation      :: Text
  }
  deriving (Show,Eq)

data Type = Type
  { tyCode    :: Text
  , tyClass   :: Maybe Text
  , tyService :: Maybe Text
  , tyTypes   :: Maybe [Type]
  , tyName    :: Maybe Text
  }
  deriving (Show,Eq)  

data Param = Param
  { parName :: Text
  , parType :: Type
  }
  deriving (Show,Eq)
           
deriveJSONpref "mod"  'Module
deriveJSONpref "enum" 'Enumeration
deriveJSONpref "ev"   'EnumVal
deriveJSONpref "cls"  'Class
deriveJSONpref "proc" 'Procedure
deriveJSONpref "ty"   'Type
deriveJSONpref "par"  'Param

----------------------------------------------------------------

-- Pretty print type information
pprType :: Bool -> Type -> Text
pprType False Type{tyCode="DOUBLE"} = "Double"
pprType False Type{tyCode="FLOAT"}  = "Float"
pprType False Type{tyCode="SINT32"} = "Int32"
pprType False Type{tyCode="SINT64"} = "Int64"
pprType False Type{tyCode="UINT32"} = "Word32"
pprType False Type{tyCode="UINT64"} = "Word64"
pprType False Type{tyCode="BOOL"}   = "Bool"
pprType False Type{tyCode="STRING"} = "Data.Text.Text"
pprType False Type{tyCode="SET", tyTypes=Just [ty]}
  = "(Set " <> pprType False ty <> ")"
pprType False Type{tyCode="LIST", tyTypes=Just [ty]}
  = "[" <> pprType False ty <> "]"
pprType False Type{tyCode="DICTIONARY", tyTypes=Just [k,v]}
  = "(Map " <> pprType False k <> " " <> pprType False v <> ")"
pprType False Type{tyCode="CLASS", tyService=Just srv, tyName=Just nm}
  = "KRPCHS.Service."<>srv<>"."<>nm
pprType True  Type{tyCode="CLASS", tyService=Just srv, tyName=Just nm}
  = "(Maybe KRPCHS.Service."<>srv<>"."<>nm<>")"
pprType False Type{tyCode="ENUMERATION", tyService=Just srv, tyName=Just nm}
  = "KRPCHS.Service."<>srv<>"."<>nm
pprType False Type{tyCode="TUPLE", tyTypes=Just xs}
  = "(" <> T.intercalate "," (map (pprType False) xs) <> ")"
pprType n ty = error (show (n,ty))

-- Pretty-print class declaration
pprClass :: (Text,Class) -> String
pprClass (nm,c) = [i|
{-| #{clsDocumentation c} -}
newtype #{nm} = #{nm} Int
  deriving (Show,Eq,Ord,PbSerializable)
instance KRPCResponseExtractable #{nm}
instance KRPCObject #{nm} where
  isKrpcNull (#{nm} i) = i == 0|]

pprEnum :: (Text,Enumeration) -> String
pprEnum (nm,e)
  = [i|
{-| #{enumDocumentation e} -}
data #{nm}
  = #{T.intercalate "\n  | " [ nm<>"'"<>evName c | c <- evals]}
  deriving (Show,Eq,Ord,Enum)

instance PbSerializable #{nm} where
  encodePb   = encodePb . (fromIntegral :: Int -> Int32) . fromEnum
  decodePb b = toEnum . (fromIntegral :: Int32 -> Int) <$> decodePb b
instance KRPCResponseExtractable #{nm}|]
  where
    evals = sortOn evValue (enumValues e)

-- Generate name of procedure
mangleProcName :: Text -> Text
mangleProcName s = T.pack $ case splitOn "_" (T.unpack s) of
  [nm1,"get",nm2] -> "get" <> uc nm1 <> uc nm2
  [nm1,"set",nm2] -> "set" <> uc nm1 <> uc nm2
  [nm1,"static",nm2] -> "static" <> uc nm1 <> uc nm2 
  [nm1,nm2]       -> lc nm1 <> nm2
  [nm1]           -> lc nm1
  _               -> error (T.unpack s)
  where
    lc (c:cs) = toLower c : cs
    uc (c:cs) = toUpper c : cs

-- Pretty-print procedure
--
-- FIXME: insert doc
pprProc :: Text -> (Text,Procedure) -> String
pprProc srv (nm,prc)
  = [i|
#{name} :: #{T.intercalate " -> " types}
#{name} #{T.intercalate " " parNms} =
  RpcCall (makeRequest "#{srv}" #{procId prc} [#{intercalate "," args}]) extract|]
  where
    name   = case procReturn_is_nullable prc of
               -- Just True -> error "A"
               _ -> mangleProcName nm
    retT   = case procReturn_type prc of
               Nothing -> "()"
               Just t  -> case procReturn_is_nullable prc of
                 Just True  -> pprType True t
                 Just False -> pprType False t
                 Nothing    -> pprType False t
    types  = map (pprType False . parType) (procParameters prc)
          ++ ["RpcCall " <> retT]
    parNms = [ case nm of "type" -> "ty"
                          _      -> nm
             | nm <- map parName (procParameters prc)
             ]
    args   = [ [i|makeArgument #{n} #{arg}|] 
             | (n,arg) <- [0..] `zip` parNms
             ]

moduleImports :: Text -> Module -> [Text]
moduleImports srv
  = id
  . map ("import qualified KRPCHS.Service."<>)
  . filter (/= srv)
  . HS.toList
  . foldMap procServices
  . modProcedures
  where
    procServices p = HS.fromList
      [ srv | Type{tyService=Just srv} <- maybeToList (procReturn_type p)
                                       ++ map parType (procParameters p)
            ]


-- Generate full haskell module
generateModule :: FilePath -> IO (String,FilePath)
generateModule fname = do
  bs <- BL.readFile fname
  let srv :: Text
      md  :: Module
      (srv, md) = case HM.toList <$> eitherDecode bs of
        Right [x] -> x
        Right _   -> error "Multiple values"
        Left e    -> error e
  return ([i|
-- ! *** DO NOT EDIT ***
-- ! 
-- ! This file is automatically generated from JSON description of RPC
-- ! interface
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module KRPCHS.Service.#{srv} where

import           Data.Int
import           Data.Map  (Map)
import           Data.Set  (Set)
import qualified Data.Text (Text)
import           Data.Word

import Control.Monad.Catch
import Control.Monad.IO.Class

import KRPCHS.Internal.Requests
import KRPCHS.Internal.SerializeUtils

#{T.unlines $ moduleImports srv md}

#{unlines $ map pprClass      $ HM.toList $ modClasses md }
#{unlines $ map pprEnum       $ HM.toList $ modEnumerations md }
#{unlines $ map (pprProc srv) $ HM.toList $ modProcedures md }
|], [i|KRPCHS/Service/#{srv}.hs|])
  
main :: IO ()
main = do
  [srcDir] <- getArgs
  let src = [ srcDir ++ "/" ++ nm
            | nm <- [ "KRPC.Drawing.json"
                    , "KRPC.InfernalRobotics.json"
--                    , "KRPC.json"
                    , "KRPC.KerbalAlarmClock.json"
                    , "KRPC.RemoteTech.json"
                    , "KRPC.SpaceCenter.json"
                    , "KRPC.UI.json"
                    ]]
  forM_ src $ \s -> do
    (hs,dst) <- generateModule s
    writeFile ("../../src/"++dst) hs
  return ()
