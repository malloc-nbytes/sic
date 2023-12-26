module Main where
import Lexer
import Parser

filepath :: String
filepath = "./input.txt"

main :: IO ()
main = do
  src <- readFile filepath
  let tokens = Lexer.lexFile src
  let result = Parser.parseTokens tokens
  print result
