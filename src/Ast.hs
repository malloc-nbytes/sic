module Ast where

data NodeFuncCall = NodeFuncCall
  { nodeFuncCallId :: String
  , nodeFuncCallArgs :: [NodeExpr]
  } deriving Show

data NodeExpr = NodeExpr
  | NodeFuncCallExpr NodeFuncCall
  | NodeStringLiteral String
  | NodeIntegerLiteral Int
  deriving Show

newtype NodeProg = NodeProg [NodeFuncCall] deriving Show
