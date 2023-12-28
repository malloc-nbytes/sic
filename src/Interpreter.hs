module Interpreter where

import qualified Data.Map as Map

import Token
import Ast
import Utility

-- data Global = Global
--   { wildcardLimit :: Int
--   , vars :: Map.Map String String
--   } deriving Show

-- data TokenType = TokenType
--   | Comment
--   | LParen
--   | RParen
--   | FuncCall
--   | IntegerLiteral
--   | StringLiteral
--   | NodeVariable
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
--   | NodeVariable String
--   deriving Show

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

wildcardFunc :: Int -> String
wildcardFunc n = take n ['a' .. 'z']

setVarFunc :: String -> [Ast.NodeExpr] -> Utility.Global -> Utility.Global
setVarFunc id args gl = gl {vars = Map.insert id (writeFunc args gl) (vars gl)}

writeFunc :: [Ast.NodeExpr] -> Utility.Global -> String
writeFunc [] _ = ""
writeFunc (x:xs) gl =
  case x of
    Ast.NodeStringLiteral s -> s ++ writeFunc xs gl
    Ast.NodeIntegerLiteral i -> show i ++ writeFunc xs gl
    Ast.NodeFuncCallExpr f -> callFunc f ++ writeFunc xs gl
    Ast.NodeVariable v -> case Map.lookup v (vars gl) of
      Just s -> s ++ writeFunc xs gl
      Nothing -> error $ "variable `" ++ v ++ "` has not been set"
    _ -> error "unsupported expression"
  where
    callFunc :: Ast.NodeFuncCall -> String
    callFunc f
      | Ast.nodeFuncCallId f == "r" =
        case Ast.nodeFuncCallArgs f of
          (Ast.NodeIntegerLiteral n:args) -> repeatFunc n args gl
          _ -> error "invalid arguments for function r, needed `n`"
      | Ast.nodeFuncCallId f == Utility.newlineFuncName =
        case Ast.nodeFuncCallArgs f of
          [] -> newlineFunc []
          [Ast.NodeIntegerLiteral n] -> newlineFunc [Ast.NodeIntegerLiteral n]
          _ -> error "invalid arguments for function n"
      | Ast.nodeFuncCallId f == Utility.wildcardFuncName =
        case Ast.nodeFuncCallArgs f of
          [] -> wildcardFunc (wildcardLimit gl)
          [Ast.NodeIntegerLiteral n] -> wildcardFunc n
          _ -> error "invalid arguments for function w, requires 1 argument"
      | otherwise = error $ "unsupported function call: " ++ show (Ast.nodeFuncCallId f)

interpret :: [Ast.NodeFuncCall] -> Utility.Global -> String
interpret [] _ = ""
interpret (x:xs) gl
  | nodeFuncCallId x == Utility.writeFuncName = writeFunc (nodeFuncCallArgs x) gl ++ interpret xs gl
  | nodeFuncCallId x == Utility.limitWildCardName =
    case nodeFuncCallArgs x of
      [Ast.NodeIntegerLiteral n] -> interpret xs (wildcardLimitFunc n gl)
      _ -> error "invalid arguments for function limitWildcard, needed `n`"
  | nodeFuncCallId x == Utility.varFuncName =
    case Ast.nodeFuncCallArgs x of
      (Ast.NodeVariable id:args) -> interpret xs (setVarFunc id args gl)
      _ -> error ("invalid args of set()" ++ show (Ast.nodeFuncCallArgs x))
  | otherwise = error $ "unsupported function call: " ++ show (nodeFuncCallId x)
