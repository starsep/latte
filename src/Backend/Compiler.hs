module Compiler (compiler) where

import AbsLatte
import AsmStmt
import AsmStandard
import CompilerState
import CStatement
import Control.Monad
import Control.Monad.RWS (runRWS, tell)
import Optimize

compiler :: Bool -> String -> Program -> String
compiler optimizeOn basename prog =
  let (_, _, output) = runRWS (compile prog) () ""
      output' = if optimizeOn then optimize output else output in
  printAsm output'

compile :: Program -> CMonad ()
compile prog = do
  emitHeader
  emitProgram prog

emitProgram :: Program -> CMonad ()
emitProgram (Program topdefs) = do
  forM_ topdefs emitTopDef

emitTopDef :: TopDef -> CMonad ()
emitTopDef (FnDef _ (Ident name) args block) = do
  putName name
  funHeader
  emitBlock block
  funFooter

emitTopDef _ = return ()
