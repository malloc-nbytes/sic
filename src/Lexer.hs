module Lexer where

import Token
import Data.Char (isDigit, isAlpha, isAlphaNum)

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

lPeek :: [Char] -> Int -> Maybe Char
lPeek [] _ = Nothing
lPeek (x:xs) ahead
  | ahead < 1 = error "tried to peak () when `ahead` < 1"
  | ahead /= 1 = lPeek xs (ahead-1)
  | otherwise = Just x

lexFile :: [Char] -> [Token]
lexFile [] = [Token "EOF" EOF]
lexFile (x:xs)
  | isIgnorable x = lexFile xs
  | isDigit x =
    let (value, rest) = consumeUntil (x : xs) (not . isDigit)
    in Token value IntegerLiteral : lexFile rest
  | x == '/' && lPeek xs 1 == Just '/' =
    let (value, rest) = consumeUntil (tail xs) (== '\n')
    in Token value Comment : lexFile rest
  | x == '/' = error "unsupported char `/` without following `/`"
  | x == '"' =
    let (value, rest) = consumeUntil xs (== '"')
    in Token value StringLiteral : lexFile (tail rest)
  | x == '(' = Token "(" LParen : lexFile xs
  | x == ')' = Token ")" RParen : lexFile xs
  | x == '#' =
    let (value, rest) = consumeUntil xs (not . isAlphaNum)
    in Token value Variable : lexFile (tail rest)
  | otherwise =
    let (value, rest) = consumeUntil (x : xs) (not . isAlpha)
    in Token value FuncCall : lexFile rest
