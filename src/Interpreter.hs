module Interpreter where

import Token ()
import Ast
import Utility

-- data TokenType = TokenType
--   | Comment
--   | LParen
--   | RParen
--   | FuncCall
--   | IntegerLiteral
--   | StringLiteral
--   | EOF
--   deriving (Show, Eq)

-- data Token = Token
--   { tokenValue :: String
--   , tokenType :: TokenType
--   } deriving (Show)

-- data NodeFuncCall = NodeFuncCall
--   { nodeFuncCallId :: String
--   , nodeFuncCallArgs :: [NodeExpr]
--   } deriving Show

-- data NodeExpr = NodeExpr
--   | NodeFuncCallExpr NodeFuncCall
--   | NodeStringLiteral String
--   | NodeIntegerLiteral Int
--   deriving Show

-- newtype NodeProg = NodeProg [NodeFuncCall] deriving Show

repeatFunc :: Int -> [Ast.NodeExpr] -> String
repeatFunc _ [] = ""
repeatFunc 0 _ = ""
repeatFunc n lst = writeFunc lst ++ repeatFunc (n - 1) lst

newlineFunc :: [Ast.NodeExpr] -> String
newlineFunc [] = "\n"
newlineFunc (x:xs) =
  case x of
    Ast.NodeIntegerLiteral i -> replicate i '\n' ++ newlineFunc xs
    Ast.NodeStringLiteral s -> s ++ newlineFunc xs
    _ -> error "unsupported expression"

writeFunc :: [Ast.NodeExpr] -> String
writeFunc [] = ""
writeFunc (x:xs) =
  case x of
    Ast.NodeStringLiteral s -> s ++ writeFunc xs
    Ast.NodeIntegerLiteral i -> show i ++ writeFunc xs
    Ast.NodeFuncCallExpr f -> callFunc f ++ writeFunc xs
    _ -> error "unsupported expression"
  where
    callFunc :: Ast.NodeFuncCall -> String
    callFunc f
      | Ast.nodeFuncCallId f == "r" =
        case Ast.nodeFuncCallArgs f of
          (Ast.NodeIntegerLiteral n : xs) -> repeatFunc n xs
          _ -> error "invalid arguments for function r, needed `n`"
      | Ast.nodeFuncCallId f == "n" =
        case Ast.nodeFuncCallArgs f of
          [] -> newlineFunc []
          [Ast.NodeIntegerLiteral n] -> newlineFunc [Ast.NodeIntegerLiteral n]
          _ -> error "invalid arguments for function n"
      | Ast.nodeFuncCallId f == "w" =
        case Ast.nodeFuncCallArgs f of
          [] -> undefined
          _ -> error "invalid arguments for function w, should take no arguments"
      | otherwise = error $ "unsupported function call: " ++ show (Ast.nodeFuncCallId f)

interpret :: [Ast.NodeFuncCall] -> Utility.Global -> String
interpret [] _ = ""
interpret (x:xs) gl
  | nodeFuncCallId x == "write" = writeFunc $ nodeFuncCallArgs x
  | otherwise = error $ "unsupported function call: " ++ show (nodeFuncCallId x)