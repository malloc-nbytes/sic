module Ast where

data NodeFuncCall = NodeFuncCall
  { nodeFuncCallId :: String
  , nodeFuncCallArgs :: [NodeExpr]
  } deriving Show

data NodeExpr = NodeExpr
  | NodeFuncCallExpr NodeFuncCall
  | NodeStringLiteral String
  | NodeIntegerLiteral Int
  | NodeVariable String
  deriving Show
