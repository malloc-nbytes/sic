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

wildcardLimitFunc :: Int -> Utility.Global -> Utility.Global
wildcardLimitFunc n gl = gl {wildcardLimit = n}

determineTypesMatch :: [Ast.NodeExpr] -> [Ast.NodeExpr] -> Bool
determineTypesMatch [] [] = True
determineTypesMatch (x:xs) (y:ys) =
  case (x, y) of
    (Ast.NodeIntegerLiteral _, Ast.NodeIntegerLiteral _) -> determineTypesMatch xs ys
    (Ast.NodeStringLiteral _, Ast.NodeStringLiteral _) -> determineTypesMatch xs ys
    (Ast.NodeFuncCallExpr _, Ast.NodeFuncCallExpr _) -> determineTypesMatch xs ys
    _ -> False

repeatFunc :: Int -> [Ast.NodeExpr] -> Utility.Global -> String
repeatFunc _ [] _ = ""
repeatFunc 0 _ _ = ""
repeatFunc n lst gl = writeFunc lst gl ++ repeatFunc (n - 1) lst gl

newlineFunc :: [Ast.NodeExpr] -> String
newlineFunc [] = "\n"
newlineFunc (x:xs) =
  case x of
    Ast.NodeIntegerLiteral i -> replicate i '\n' ++ newlineFunc xs
    Ast.NodeStringLiteral s -> s ++ newlineFunc xs
    _ -> error "unsupported expression"

-- Generates random chars from a-z with length wildcardLimit
wildcardFunc :: Utility.Global -> String
wildcardFunc gl = take (wildcardLimit gl) ['a' .. 'z']

writeFunc :: [Ast.NodeExpr] -> Utility.Global -> String
writeFunc [] _ = ""
writeFunc (x:xs) gl =
  case x of
    Ast.NodeStringLiteral s -> s ++ writeFunc xs gl
    Ast.NodeIntegerLiteral i -> show i ++ writeFunc xs gl
    Ast.NodeFuncCallExpr f -> callFunc f ++ writeFunc xs gl
    _ -> error "unsupported expression"
  where
    callFunc :: Ast.NodeFuncCall -> String
    callFunc f
      | Ast.nodeFuncCallId f == "r" =
        case Ast.nodeFuncCallArgs f of
          (Ast.NodeIntegerLiteral n : xs) -> repeatFunc n xs gl
          _ -> error "invalid arguments for function r, needed `n`"
      | Ast.nodeFuncCallId f == "n" =
        case Ast.nodeFuncCallArgs f of
          [] -> newlineFunc []
          [Ast.NodeIntegerLiteral n] -> newlineFunc [Ast.NodeIntegerLiteral n]
          _ -> error "invalid arguments for function n"
      | Ast.nodeFuncCallId f == "w" =
        case Ast.nodeFuncCallArgs f of
          [] -> wildcardFunc gl
          _ -> error "invalid arguments for function w, should take no arguments"
      | otherwise = error $ "unsupported function call: " ++ show (Ast.nodeFuncCallId f)

interpret :: [Ast.NodeFuncCall] -> Utility.Global -> String
interpret [] _ = ""
interpret (x:xs) gl
  | nodeFuncCallId x == "write" = writeFunc (nodeFuncCallArgs x) gl ++ interpret xs gl
  | nodeFuncCallId x == "limitWildcard" =
    case nodeFuncCallArgs x of
      [Ast.NodeIntegerLiteral n] -> interpret xs (wildcardLimitFunc n gl)
      _ -> error "invalid arguments for function limitWildcard, needed `n`"
  | otherwise = error $ "unsupported function call: " ++ show (nodeFuncCallId x)
