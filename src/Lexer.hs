module Lexer where

import Data.Char (isDigit, isAlpha)

data TokenType = TokenType
  | Comment
  | LParen
  | RParen
  | Wildcard
  | FuncCall
  | IntegerLiteral
  | Macro
  | StringLiteral
  | EOF
  deriving Show

data Token = Token
  { tokenValue :: String,
    tokenType :: TokenType
  } deriving Show

isIgnorable :: Char -> Bool
isIgnorable c = c == '\t' || c == '\n' || c == ' '

consumeUntil :: [Char] -> (Char -> Bool) -> ([Char], [Char])
consumeUntil [] _ = ([], [])
consumeUntil lst predicate = f lst []
  where
    f :: [Char] -> [Char] -> ([Char], [Char])
    f [] _ = ([], [])
    f (x:xs) acc
      | x == '\\' = f (tail xs) (acc ++ [head xs])
      | predicate x = (acc, x : xs)
      | otherwise = f xs (acc ++ [x])

peek :: [Char] -> Int -> Maybe Char
peek [] _ = Nothing
peek (x:xs) ahead
  | ahead < 1 = error "tried to peak () when `ahead` < 1"
  | ahead /= 1 = peek xs (ahead-1)
  | otherwise = Just x

lexFile :: [Char] -> [Token]
lexFile [] = [Token "EOF" EOF]
lexFile (x:xs)
  | isIgnorable x = lexFile xs
  | isDigit x =
    let (value, rest) = consumeUntil (x : xs) (not . isDigit)
    in Token value IntegerLiteral : lexFile rest
  | x == '/' && peek xs 1 == Just '/' =
    let (value, rest) = consumeUntil (tail xs) (== '\n')
    in Token value Comment : lexFile rest
  | x == '/' = error "unsupported char `/` without following `/`"
  | x == '"' =
    let (value, rest) = consumeUntil xs (== '"')
    in Token value StringLiteral : lexFile (tail rest)
  | x == '*' = Token "*" Wildcard : lexFile xs
  | x == '(' = Token "(" LParen : lexFile xs
  | x == ')' = Token ")" RParen : lexFile xs
  | otherwise =
    let (value, rest) = consumeUntil (x : xs) (not . isAlpha)
    in Token value FuncCall : lexFile rest
