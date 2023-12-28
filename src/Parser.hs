module Parser where
import Token
import Ast

pExpect :: [Token.Token] -> Token.TokenType -> (Token.Token, [Token.Token])
pExpect [] _ = error "no tokens in call to `expect ()`."
pExpect (x:xs) t
  | tokenType x == t = (x, xs)
  | otherwise = error $ "Call to pExpect () with differing token types, namely:\n  (expected: " ++
                show t ++
                ") =/= (actual: " ++
                show (tokenType x) ++
                ")" ++
                " (value: " ++ show (tokenValue x) ++ ")\n" ++
                "Failed on: " ++ show x ++ "\n" ++
                "Remaining: " ++ show xs

pPeak :: [Token.Token] -> Maybe Token.Token
pPeak [] = Nothing
pPeak tokens = Just $ head tokens

tokenValueToStr :: Token.Token -> Int
tokenValueToStr x = read (tokenValue x) :: Int

parseFuncArgs :: Token -> [Token] -> (Ast.NodeFuncCall, [Token.Token])
parseFuncArgs id [] = error ("Could not parse" ++ show (tokenValue id))
parseFuncArgs id lst =
  let (args, rest) = f lst []
  in (Ast.NodeFuncCall (tokenValue id) args, rest)
  where
    f :: [Token.Token] -> [Ast.NodeExpr] -> ([Ast.NodeExpr], [Token.Token])
    f [] _ = error "Unterminated function call"
    f (x:xs) acc
      | tokenType x == Token.RParen = (acc, xs)
      | tokenType x == Token.StringLiteral = f xs (acc ++ [Ast.NodeStringLiteral (tokenValue x)])
      | tokenType x == Token.IntegerLiteral = f xs (acc ++ [Ast.NodeIntegerLiteral (tokenValueToStr x)])
      | tokenType x == Token.Variable = f xs (acc ++ [Ast.NodeVariable (tokenValue x)])
      | tokenType x == Token.FuncCall =
        let (fc, rest) = parseFuncCall x xs
        in f rest (acc ++ [Ast.NodeFuncCallExpr fc])
      | tokenType x == Token.EOF = error ("Unterminated function call in " ++ show (tokenValue id) ++ " at EOF")
      | otherwise = error ("parseFuncArgs: Unsupported token type in function call: " ++ show (tokenType x) ++ show (tokenValue x))

parseFuncCall :: Token -> [Token.Token] -> (Ast.NodeFuncCall, [Token.Token])
parseFuncCall id lst =
  let (_, rest) = pExpect lst Token.LParen
  in let (fc, rest') = parseFuncArgs id rest
    in (fc, rest')

parseTokens :: [Token.Token] -> [Ast.NodeFuncCall]
parseTokens [] = []
parseTokens (x:xs) =
  case tokenType x of
    Token.EOF -> []
    Token.Comment -> parseTokens xs
    Token.FuncCall -> let (fc, rest) = parseFuncCall x xs in fc : parseTokens rest
    Token.StringLiteral -> error $ "string literal not in function call " ++ show (tokenValue x)
    Token.IntegerLiteral -> error $ "integer literal not in function call " ++ show (tokenValue x)
    Token.Variable -> error $ "variable not in function call " ++ show (tokenValue x)
    _ -> error $ "unsupported TokenType " ++ show (tokenType x) ++ show (tokenValue x)

produceProgram :: [Token.Token] -> [Ast.NodeFuncCall]
produceProgram [] = []
produceProgram tokens = parseTokens tokens
