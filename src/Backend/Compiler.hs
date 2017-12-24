module Compiler (compiler) where

import AbsLatte
import AsmStmt
import AsmStandard
import CompilerState
import CStatement
import Control.Monad
import Control.Monad.RWS (runRWS, tell)

compiler :: Bool -> String -> Program -> String
compiler optimizeOn basename prog =
  let (_, _, output) = runRWS (compile prog) () () in
  printAsm output

compile :: Program -> CMonad ()
compile prog = do
  emitHeader
  emitStandardImpl
  emitProgram prog

emitProgram :: Program -> CMonad ()
emitProgram (Program topdefs) = do
  funImpl "main" [
    Push "rbp",
    Mov "rbp" "rsp",
    Call "functionMain",
    Xor "rax" "rax",
    Leave]
  forM_ topdefs emitTopDef

emitTopDef :: TopDef -> CMonad ()
emitTopDef (FnDef Int (Ident "main") [] block) = do
  tell [Label "functionMain"]
  emitBlock block
  tell [AsmStmt.Ret]

emitTopDef _ = return ()

emitBlock :: Block -> CMonad ()
emitBlock (Block stmts) = forM_ stmts emitStatement
