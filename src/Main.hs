module Main where
import Lexer
import Parser
import Interpreter
import Utility

filepath :: String
filepath = "./input.txt"

outputFilepath :: String
outputFilepath = "./output.txt"

main :: IO ()
main = do
  src <- readFile filepath
  let tokens = Lexer.lexFile src
  let nodeProg = Parser.produceProgram tokens
  let result = Interpreter.interpret nodeProg (Global 5 0)
  writeFile outputFilepath result
  --  Interpreter.interpret . Parser.produceProgram . Lexer.lexFile =<< readFile filepath
