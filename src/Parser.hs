module Parser where

import Token

--    LParen -> undefined
--    RParen -> undefined
--    IntegerLiteral -> undefined

pExpect :: [Token] -> TokenType -> (Token, [Token])
pExpect [] _ = error "no tokens in call to `expect ()`."
pExpect (x:xs) t
  | tokenType x == t = (x, xs)
  | otherwise = error $ "call to pExpect with differing token types, namely: expected: " ++
                show t ++
                " /= actual: " ++
                show (tokenType x)

pPeak :: [Token] -> Maybe Token
pPeak [] = Nothing
pPeak tokens = Just $ head tokens

-- parseFuncCall [Token] -> String
-- parseFuncCall (x:xs) =
--   let body =

parseTokens :: [Token] -> String
parseTokens [] = ""
parseTokens (x:xs) =
  case tokenType x of
    EOF -> ""
    Comment -> parseTokens xs
    Wildcard -> "WILDCARD" ++ parseTokens xs
    FuncCall -> undefined
    Macro -> undefined
    StringLiteral -> tokenValue x ++ parseTokens xs
    _ -> error "unsupported TokenType"

