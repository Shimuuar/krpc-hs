-- |
module TH where

import           Data.List (isPrefixOf)
import           Data.Char
import           Data.Aeson.TH
import Language.Haskell.TH (Name,Q,Dec)

dropP :: String -> String -> String
dropP p s | p `isPrefixOf` s = let c:cs = drop (length p) s
                               in  toLower c : cs
          | otherwise        = error "Invalid prefix"

deriveJSONpref :: String -> Name -> Q [Dec]
deriveJSONpref pref 
  = deriveJSON defaultOptions{fieldLabelModifier = dropP pref}
