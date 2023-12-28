module Utility where

import qualified Data.Map as Map

data Global = Global
  { wildcardLimit :: Int
  , vars :: Map.Map String String
  } deriving Show
