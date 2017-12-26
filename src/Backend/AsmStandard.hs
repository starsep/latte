module AsmStandard where

import AsmStmt
import CompilerState
import Control.Monad.RWS (tell)
import TypecheckerPure (standardFunctionsNames)

emitHeader :: CMonad ()
emitHeader = do
  tell [Global "main", EmptyLine]
  emitExterns
  tell [EmptyLine]

emitExterns :: CMonad ()
emitExterns =
  tell $ map Extern $ "__new" : standardFunctionsNames

funHeader :: CMonad ()
funHeader = do
  name <- getName
  tell [
    Label name,
    Push "rbp",
    Mov "rbp" "rsp"]

funFooter :: CMonad ()
funFooter = do
  name <- getName
  tell [
    Label $ name ++ "$end",
    Pop "rbp",
    Custom "ret" []]

argRegisters :: [String]
argRegisters = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

preserveRegisters :: [String]
preserveRegisters =
  ["rbx", "rsp", "rbp", "r12", "r13", "r14", "r15"]

scratchRegisters :: [String]
scratchRegisters =
  ["rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9", "r10", "r11"]

registers :: [String]
registers = preserveRegisters ++ scratchRegisters
