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

tokenValueToStr :: Token.Token -> Int
tokenValueToStr x = read (tokenValue x) :: Int

parseFuncArgs :: Token -> [Token] -> (Ast.NodeFuncCall, [Token.Token])
parseFuncArgs id [] = error ("could not parse" ++ show (tokenValue id))
parseFuncArgs id lst =
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
        let (fc, rest) = parseFuncCall x xs
        in f rest (acc ++ [Ast.NodeFuncCallExpr fc])
      | otherwise = error ("unsupported token type in function call: " ++ show (tokenType x))

parseFuncCall :: Token -> [Token.Token] -> (Ast.NodeFuncCall, [Token.Token])
parseFuncCall id lst =
  let (_, rest) = pExpect lst Token.LParen
  in let (fc, rest') = parseFuncArgs id rest
    in (fc, rest')

parseTokens :: [Token.Token] -> [Ast.NodeFuncCall]
parseTokens (x:xs) =
  case tokenType x of
    Token.EOF -> []
    Token.Comment -> parseTokens xs
    Token.FuncCall -> let (fc, rest) = parseFuncCall x xs in fc : parseTokens rest
    Token.StringLiteral -> error $ "string literal not in function call " ++ show (tokenValue x)
    _ -> error $ "unsupported TokenType " ++ show (tokenType x)

produceProgram :: [Token.Token] -> [Ast.NodeFuncCall]
produceProgram [] = []
produceProgram tokens = parseTokens tokens
