module Main where
import Lexer
import Parser
import Interpreter
import Utility

filepath :: String
filepath = "./input.txt"

main :: IO ()
main = do
  src <- readFile filepath
  let tokens = Lexer.lexFile src
  let nodeProg = Parser.produceProgram tokens
  let result = Interpreter.interpret nodeProg (Global 5)
  print result
  --  Interpreter.interpret . Parser.produceProgram . Lexer.lexFile =<< readFile filepath
