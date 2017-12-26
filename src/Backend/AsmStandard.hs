module AsmStandard where

import AsmStmt
import CompilerState
import Control.Monad.RWS (tell)
import TypecheckerPure (standardFunctionsNames)

emitHeader :: CMonad ()
emitHeader = do
  tell [Global "main", Empty]
  emitExterns
  tell [Empty]

emitExterns :: CMonad ()
emitExterns =
  tell $ map Extern standardFunctionsNames

funHeader :: String -> CMonad ()
funHeader name =
  tell [
    Label name,
    Push "rbp",
    Mov "rbp" "rsp"]

funFooter :: String -> CMonad ()
funFooter name =
  tell [
    Label $ name ++ "$end",
    Pop "rbp",
    Ret]

funImpl :: String -> AsmStmts -> CMonad ()
funImpl name body = do
  funHeader name
  tell body
  funFooter name
