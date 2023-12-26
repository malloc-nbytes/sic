module Parser where

import Token

parseTokens :: [Token] -> String
parseTokens [] = ""
parseTokens (x:xs) =
  case tokenType x of
    EOF -> ""
    Comment -> parseTokens xs
    LParen -> undefined
    RParen -> undefined
    Wildcard -> undefined
    FuncCall -> undefined
    IntegerLiteral -> undefined
    Macro -> undefined
    StringLiteral -> undefined
    _ -> error "unsupported TokenType"

