module CStatement where

import AbsLatte
import AsmStmt
import CExpression
import CompilerState
import Control.Monad
import Control.Monad.RWS (tell)

jumpToEnd :: CMonad ()
jumpToEnd = do
  name <- getName
  tell [Jmp $ name ++ "$end"]

emitStatement :: Stmt -> CMonad ()
emitStatement stmt = case stmt of
  Empty -> return ()
  BStmt block -> emitBlock block
  Decl _ items -> forM_ items emitDecl
  Ass lvalue expr -> do
    emitExpression expr
    tell [Pop "rax"] -- TODO: save to lvalue
  Ret expr -> do
    emitExpression expr
    tell [Pop "rax"]
    jumpToEnd
  VRet -> jumpToEnd
  SExp expr -> do
    emitExpression expr
    tell [Pop "rax"]
  _ -> return () -- TODO

emitBlock :: Block -> CMonad ()
emitBlock (Block stmts) = forM_ stmts emitStatement

emitDecl :: Item -> CMonad ()
emitDecl (Init ident expr) = do
  emitDecl $ NoInit ident
  emitStatement $ Ass (EVar ident) expr
emitDecl (NoInit (Ident name)) = return () -- TODO
