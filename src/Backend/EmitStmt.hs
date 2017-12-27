module EmitStmt where

import AbsLatte
import AsmStmt
import CompilerState
import Control.Monad
import Control.Monad.RWS (tell)
import EmitExpr
import Label

emitStmt :: Stmt -> CMonad ()
emitStmt q = case q of
  Empty -> return ()
  BStmt block -> emitBlock block
  Decl _ items -> forM_ items emitDecl
  Ass lvalue expr -> do
    _ <- emitExpr expr
    tell [Pop "rax"] -- TODO: save to lvalue
  Incr lv -> emitStmt $ Ass (EVar lv) (EAdd (EVar lv) Plus (ELitInt 1))
  Decr lv -> emitStmt $ Ass (EVar lv) (EAdd (EVar lv) Minus (ELitInt 1))
  Ret expr -> do
    _ <- emitExpr expr
    tell [Pop "rax"]
    jumpEndLabel
  VRet -> jumpEndLabel
  Cond expr stmt -> do
    _ <- emitExpr expr
    afterLabel <- ifLabel
    tell [
      Pop "rax",
      Cmp "rax" "0",
      Custom "je" [afterLabel]]
    emitStmt stmt
    tell [Label afterLabel]
  CondElse expr stmt stmt' -> do
    _ <- emitExpr expr
    (elseLabel, afterLabel) <- ifElseLabels
    tell [
      Pop "rax",
      Cmp "rax" "0",
      Custom "je" [elseLabel]]
    emitStmt stmt
    tell [
      Jmp afterLabel,
      Label elseLabel]
    emitStmt stmt'
    tell [Label afterLabel]
  While expr stmt -> do
    (beginLabel, afterLabel) <- whileLabels
    tell [Label beginLabel]
    _ <- emitExpr expr
    tell [
      Pop "rax",
      Cmp "rax" "0",
      Custom "je" [afterLabel]]
    emitStmt stmt
    tell [
      Jmp beginLabel,
      Label afterLabel]
  For _ _ _ _ -> return () -- TODO: implement for
  SExp expr -> do
    t <- emitExpr expr
    unless (t == Void) $
      tell [Pop "rax"]

emitBlock :: Block -> CMonad ()
emitBlock (Block stmts) = forM_ stmts emitStmt

emitDecl :: Item -> CMonad ()
emitDecl (Init ident expr) = do
  emitDecl $ NoInit ident
  emitStmt $ Ass (EVar ident) expr
emitDecl (NoInit (Ident name)) = return () -- TODO
