module Main where
import Lexer

filepath :: String
filepath = "./input.txt"

main :: IO ()
main = do
  src <- readFile filepath
  let tokens = Lexer.lexFile src
  print tokens
