module Interpreter where

-- [6, 23, 2, 20, 17, 14, 3, 16, 0, 7, 12, 6, 20, 21, 21, 15, 0, 6, 6, 19, 10, 21, 11, 4, 13, 8, 14, 16, 6, 6, 10, 17, 10, 0, 11, 24, 15, 22, 4, 22, 16, 2, 23, 10, 17, 17, 9, 16, 6, 17, 7, 1, 9, 25, 8, 8, 8, 2, 22, 20, 20, 7, 25, 9, 10, 19, 14, 7, 8, 12, 11, 19, 13, 6, 6, 0, 11, 5, 1, 4, 10, 6, 19, 1, 0, 7, 14, 17, 19, 14, 12, 11, 21, 9, 7, 8, 5, 9, 19, 0]

import Token
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

-- let lst = [1,2,3,4,5]
-- pair(1, pair(2, pair(3, pair(4, pair(5, _)))))
-- (x:xs)
--   x = 1
--   xs = pair(2, pair(3, pair(4, pair(5, _))))

-- getRandomNum :: Utility.Global -> (Int, Utility.Global)
-- getRandomNum gl =
--   let lst = [6, 23, 2,
--              20, 17, 14,
--              3, 16, 0,
--              7, 12, 6,
--              20, 21, 21,
--              15, 0, 6,
--              6, 19, 10,
--              21, 11, 4,
--              13, 8, 14,
--              16, 6, 6,
--              10, 17, 10,
--              0, 11, 24,
--              15, 22, 4,
--              22, 16, 2,
--              23, 10, 17,
--              17, 9, 16,
--              6, 17, 7,
--              1, 9, 25,
--              8, 8, 8,
--              2, 22, 20,
--              20, 7, 25,
--              9, 10, 19,
--              14, 7, 8,
--              12, 11, 19,
--              13, 6, 6,
--              0, 11, 5,
--              1, 4, 10,
--              6, 19, 1,
--              0, 7, 14,
--              17, 19, 14,
--              12, 11, 21,
--              9, 7, 8,
--              5, 9, 19, 0]
--   in
--     (lst !! seed gl, Utility.Global (wildcardLimit gl) (seed gl + 1))

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
          [] -> wildcardFunc (wildcardLimit gl)
          [Ast.NodeIntegerLiteral n] -> wildcardFunc n
          _ -> error "invalid arguments for function w, requires 1 argument"
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
