module Compiler where

import AbsLatte
import Assembly
import AsmStmt
import AsmStandard

compiler :: Bool -> String -> Program -> String
compiler optimizeOn basename prog =
  printAsm $
    emitHeader ++
    standardImpl ++
    emitProgram prog

emitProgram :: Program -> AsmStmts
emitProgram prog = funImpl "main" [
  Push "rbp",
  Mov "rbp" "rsp",
  Call "printInt",
  Xor "rax" "rax",
  Leave]
