module Interpreter where

import Token
import Ast

interpret :: Ast.NodeProg -> IO ()
interpret (NodeProg []) = return ()
interpret (NodeProg (x:xs)) = undefined