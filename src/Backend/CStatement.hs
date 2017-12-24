module CStatement where

import AbsLatte
import qualified AsmStmt as Asm
import CExpression
import CompilerState
import Control.Monad
import Control.Monad.RWS (tell)

emitStatement :: Stmt -> CMonad ()
emitStatement stmt = case stmt of
  Empty -> return ()
  BStmt block -> emitBlock block
  Ret expr -> do
    emitExpression expr
    tell [Asm.Pop "rax", Asm.Jmp "main$end"] -- TODO: change label
  VRet -> tell [Asm.Jmp "main$end"] -- TODO: change label
  SExp expr -> emitExpression expr
  _ -> return () -- TODO

emitBlock :: Block -> CMonad ()
emitBlock (Block stmts) = forM_ stmts emitStatement
