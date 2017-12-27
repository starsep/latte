module AsmFun where

import AsmStmt
import CompilerState
import Control.Monad.RWS (tell)
import TypecheckerPure (standardFunctionsNames)

emitHeader :: CMonad ()
emitHeader = do
  tell [Global "main", EmptyLine]
  tell $ map Extern standardFunctionsNames
  tell [SectionText]

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
