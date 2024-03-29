module Interpreter where

import qualified Data.Map as Map

import Token
import Ast
import Utility

wildcardLimitFunc :: Int -> Utility.Global -> Utility.Global
wildcardLimitFunc n gl = gl {wildcardLimit = n}

repeatFunc :: Int -> [Ast.NodeExpr] -> Utility.Global -> String
repeatFunc _ [] _ = ""
repeatFunc 0 _ _ = ""
repeatFunc n lst gl =
  let (s, gl') = writeFunc lst gl
  in s ++ repeatFunc (n - 1) lst gl'

newlineFunc :: [Ast.NodeExpr] -> String
newlineFunc [] = "\n"
newlineFunc (x:xs) =
  case x of
    Ast.NodeIntegerLiteral i -> replicate i '\n' ++ newlineFunc xs
    Ast.NodeStringLiteral s -> s ++ newlineFunc xs
    _ -> error "unsupported expression"

wildcardFunc :: Int -> String
wildcardFunc n = take n ['a' .. 'z']

incrementIota :: Maybe Int -> Utility.Global -> Utility.Global
incrementIota n gl =
  case n of
    Just n' -> gl {iota = iota gl + n'}
    Nothing -> gl {iota = iota gl + 1}

setVarFunc :: String -> [Ast.NodeExpr] -> Utility.Global -> Utility.Global
setVarFunc id args gl =
  let (s, gl') = writeFunc args gl
  in gl' {vars = Map.insert id s (vars gl')}

writeFunc :: [Ast.NodeExpr] -> Utility.Global -> (String, Utility.Global)
writeFunc [] gl = ("", gl)
writeFunc (x:xs) gl =
  case x of
    Ast.NodeStringLiteral s ->
      let (s', gl') = writeFunc xs gl
      in (s ++ s', gl')
    Ast.NodeIntegerLiteral i ->
      let (s, gl') = writeFunc xs gl
      in (show i ++ s, gl')
    Ast.NodeFuncCallExpr fc ->
      let (s, gl') = callFunc fc gl
      in let (s', gl'') = writeFunc xs gl'
         in (s ++ s', gl'')
    Ast.NodeVariable v -> case Map.lookup v (vars gl) of
      Just s ->
        let (s', gl') = writeFunc xs gl
        in (s ++ s', gl')
      Nothing -> error $ "variable `" ++ v ++ "` has not been set"
    _ -> error "unsupported expression"
  where
    callFunc :: Ast.NodeFuncCall -> Utility.Global -> (String, Utility.Global)
    callFunc f gl'
      | Ast.nodeFuncCallId f == "r" =
        case Ast.nodeFuncCallArgs f of
          (Ast.NodeIntegerLiteral n:args) -> (repeatFunc n args gl, gl')
          _ -> error "invalid arguments for function r, needed `Integer`, for the number of times to repeat"
      | Ast.nodeFuncCallId f == Utility.newlineFuncName =
        case Ast.nodeFuncCallArgs f of
          [] -> (newlineFunc [], gl')
          [Ast.NodeIntegerLiteral n] -> (newlineFunc [Ast.NodeIntegerLiteral n], gl')
          _ -> error "invalid arguments for function n"
      | Ast.nodeFuncCallId f == Utility.wildcardFuncName =
        case Ast.nodeFuncCallArgs f of
          [] -> (wildcardFunc (wildcardLimit gl), gl')
          [Ast.NodeIntegerLiteral n] -> (wildcardFunc n, gl')
          _ -> error "invalid arguments for function w, requires 1 argument"
      | Ast.nodeFuncCallId f == Utility.varIotaFuncName =
        case Ast.nodeFuncCallArgs f of
          [] -> (show (iota gl), incrementIota Nothing gl)
          [Ast.NodeIntegerLiteral n] -> (show (iota gl), incrementIota (Just n) gl)
          _ -> error "invalid arguments for function iota, requires 0 or 1 arguments"
      | otherwise =
        error $ "unsupported function call:\n  " ++
        show (Ast.nodeFuncCallId f) ++ show (Ast.nodeFuncCallArgs f) ++ ".\n" ++
        "This most likely happend because of calling a function that is not supported by write().\n" ++
        "Try moving the function call outside of write(). Otherwise, the function does not exist."

changeOutputFilepath :: String -> Utility.Global -> Utility.Global
changeOutputFilepath [] _ = error "empty name to changeOutputFilepath ()"
changeOutputFilepath s gl = gl {outputFilepath = s}

interpret :: [Ast.NodeFuncCall] -> Utility.Global -> (String, Utility.Global)
interpret [] gl = ("", gl)
interpret (x:xs) gl
  | nodeFuncCallId x == Utility.writeFuncName =
    let (writeResult, gl') = writeFunc (nodeFuncCallArgs x) gl
    in let (rest, gl'') = interpret xs gl'
       in (writeResult ++ rest, gl'')
  | nodeFuncCallId x == Utility.limitWildCardName =
    case nodeFuncCallArgs x of
      [Ast.NodeIntegerLiteral n] -> interpret xs (wildcardLimitFunc n gl)
      _ -> error "invalid arguments for function limitWildcard, needed `n`"
  | nodeFuncCallId x == Utility.varFuncName =
    case Ast.nodeFuncCallArgs x of
      (Ast.NodeVariable id:args) -> interpret xs (setVarFunc id args gl)
      _ -> error ("invalid args of var()" ++ show (Ast.nodeFuncCallArgs x))
  | nodeFuncCallId x == Utility.outputFuncName =
    case nodeFuncCallArgs x of
      [] -> error ("invalid args to " ++ Utility.outputFuncName)
      lst ->
        let (s, gl') = writeFunc lst gl
        in let gl'' = changeOutputFilepath s gl'
           in interpret xs gl''
  | otherwise =
    error $ "unsupported function call: " ++ show (nodeFuncCallId x) ++ show (nodeFuncCallArgs x) ++
    "\nA possible cause of this is calling a function that is only available inside of write().\n" ++
    "Try moving the function call inside of write(). Otherwise, the function does not exist."
