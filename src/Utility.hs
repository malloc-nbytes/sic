module Utility where

import qualified Data.Map as Map

data Global = Global
  { wildcardLimit :: Int
  , iota :: Int
  , vars :: Map.Map String String
  } deriving Show

writeFuncName :: String
writeFuncName = "write"

limitWildCardName :: String
limitWildCardName = "limitWildCard"

newlineFuncName :: String
newlineFuncName = "n"

wildcardFuncName :: String
wildcardFuncName = "w"

varFuncName :: String
varFuncName = "var"

varIotaFuncName :: String
varIotaFuncName = "iota"
