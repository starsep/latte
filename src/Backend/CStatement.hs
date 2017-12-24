module CStatement where

import AbsLatte
import qualified AsmStmt as Asm
import CExpression
import CompilerState
import Control.Monad.RWS (tell)

emitStatement :: Stmt -> CMonad ()
emitStatement stmt = case stmt of
  Empty -> return ()
  SExp expr -> emitExpression expr
  Ret expr -> do
    emitExpression expr
    tell [Asm.Pop "rax"]
