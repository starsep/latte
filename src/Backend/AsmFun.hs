module AsmFun where

import AsmStmt
import CompilerState
import Control.Monad.RWS (tell)
import TypecheckerPure (standardFunctionsNames)

emitHeader :: CMonad ()
emitHeader = do
  tell [Global "main", EmptyLine]
  emitExterns
  tell [SectionText]

internalFunctions :: [String]
internalFunctions = ["_new", "_copyStr"]

emitExterns :: CMonad ()
emitExterns =
  tell $ map Extern $ internalFunctions ++ standardFunctionsNames

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
