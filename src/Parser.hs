module Parser where

import Token
import Ast

pExpect :: [Token.Token] -> Token.TokenType -> (Token.Token, [Token.Token])
pExpect [] _ = error "no tokens in call to `expect ()`."
pExpect (x:xs) t
  | tokenType x == t = (x, xs)
  | otherwise = error $ "call to pExpect with differing token types, namely: expected: " ++
                show t ++
                " /= actual: " ++
                show (tokenType x)

pPeak :: [Token.Token] -> Maybe Token.Token
pPeak [] = Nothing
pPeak tokens = Just $ head tokens

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

tokenValueToStr :: Token.Token -> Int
tokenValueToStr x = read (tokenValue x) :: Int

parseFuncCall :: Token -> [Token] -> (Ast.NodeFuncCall, [Token.Token])
parseFuncCall id [] = error ("could not parse" ++ show (tokenValue id))
parseFuncCall id lst =
  let (args, rest) = f lst []
  in (Ast.NodeFuncCall (tokenValue id) args, rest)
  where
    f :: [Token.Token] -> [Ast.NodeExpr] -> ([Ast.NodeExpr], [Token.Token])
    f [] _ = error "unterminated function call"
    f (x:xs) acc
      | tokenType x == Token.RParen = (acc, xs)
      | tokenType x == Token.StringLiteral = f xs (acc ++ [Ast.NodeStringLiteral (tokenValue x)])
      | tokenType x == Token.IntegerLiteral = f xs (acc ++ [Ast.NodeIntegerLiteral (tokenValueToStr x)])
      | tokenType x == Token.FuncCall =
        let (fc, rest) = parsePrimaryFuncCall x xs
        in f rest (acc ++ [Ast.NodeFuncCallExpr fc])
      | otherwise = error ("unsupported token type in function call: " ++ show (tokenType x))

parsePrimaryFuncCall :: Token -> [Token.Token] -> (Ast.NodeFuncCall, [Token.Token])
parsePrimaryFuncCall id lst =
  let (_, rest) = pExpect lst Token.LParen
  in let (fc, rest') = parseFuncCall id rest
    in (fc, rest')

parseTokens :: [Token.Token] -> [Ast.NodeFuncCall]
parseTokens [] = undefined
parseTokens (x:xs) =
  case tokenType x of
    Token.EOF -> []
    Token.Comment -> parseTokens xs
    Token.FuncCall -> let (fc, rest) = parsePrimaryFuncCall x xs in fc : parseTokens rest
    Token.Macro -> error ("macros are unsupported " ++ show (tokenValue x))
    Token.StringLiteral -> error $ "string literal not in function call " ++ show (tokenValue x)
    Token.Wildcard -> error ("wildcard is not supported " ++ show (tokenType x))
    _ -> error ("unsupported TokenType " ++ show (tokenType x))

produceProgram :: [Token.Token] -> Ast.NodeProg
produceProgram [] = Ast.NodeProg []
produceProgram tokens = Ast.NodeProg $ parseTokens tokens