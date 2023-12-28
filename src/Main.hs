module Main where

import qualified Data.Map as Map

import Lexer
import Parser
import Interpreter
import Utility
import Token

filepath :: String
filepath = "./input.txt"

outputFilepath :: String
outputFilepath = "./output.txt"

debugTokenPrintValues :: [Token] -> IO ()
debugTokenPrintValues [] = return ()
debugTokenPrintValues lst = mapM_ (putStr . tokenValue) lst

main :: IO ()
main = do
  src <- readFile filepath
  let tokens = Lexer.lexFile src
  -- debugTokenPrintValues tokens
  let nodeProg = Parser.produceProgram tokens
  let result = Interpreter.interpret nodeProg (Global 5 Map.empty)
  writeFile outputFilepath result
  --  Interpreter.interpret . Parser.produceProgram . Lexer.lexFile =<< readFile filepath
