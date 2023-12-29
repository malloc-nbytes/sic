module Main where

import qualified Data.Map as Map
import System.Environment (getArgs)

import Lexer
import Parser
import Interpreter
import Utility
import Token

usage :: IO ()
usage = do
  putStrLn "tfc usage:"
  putStrLn "  ./tfc <input-file> <output-file>"

run :: [String] -> IO ()
run [x] = undefined
run (x:y:_) =
  let (inputFp, outFp) = (x, y)
  in do
    src <- readFile inputFp
    let tokens = Lexer.lexFile src
    putStrLn "[tfc Lexer]:"
    print tokens >> putStrLn ""
    let nodeProg = Parser.produceProgram tokens
    putStrLn "[tfc Parser]:"
    print nodeProg >> putStrLn ""
    let (result, outputFilepath) = Interpreter.interpret nodeProg (Global "./out.txt" 5 0 Map.empty)
    writeFile outputFilepath result
    -- putStrLn "[tfc Interpreter]:"
    -- print result >> putStrLn ""
    -- writeFile outFp result
    putStrLn "[tfc] Wrote to:"
    putStrLn outputFilepath

main :: IO ()
main = do
  run ["./input.txt", "output.txt"]
  -- args <- getArgs
  -- case args of
  --   [] -> usage
  --   _ -> run args
