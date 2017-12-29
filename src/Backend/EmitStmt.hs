module EmitStmt where

import AbsLatte
import Asm
import CompilerState
import Control.Monad
import Control.Monad.RWS (tell)
import qualified Data.Map as Map
import EmitExpr
import Label

emitStmt :: Stmt -> CMonad ()
emitStmt q = case q of
  Empty -> return ()
  BStmt block -> emitBlock block
  Decl t items -> forM_ items $ emitDecl t
  Ass lv expr -> emitAss lv expr
  Incr lv -> emitStmt $ Ass (EVar lv) (EAdd (EVar lv) Plus (ELitInt 1))
  Decr lv -> emitStmt $ Ass (EVar lv) (EAdd (EVar lv) Minus (ELitInt 1))
  Ret expr -> do
    _ <- emitExpr expr
    tell [Pop resultReg]
    jumpEndLabel
  VRet -> jumpEndLabel
  Cond expr stmt -> do
    _ <- emitExpr expr
    afterLabel <- ifLabel
    localReserve 1 $ \[r] ->
      tell [
        Pop r,
        Cmp r "0",
        Je afterLabel]
    emitStmt stmt
    tell [Label afterLabel]
  CondElse expr stmt stmt' -> do
    _ <- emitExpr expr
    (elseLabel, afterLabel) <- ifElseLabels
    localReserve 1 $ \[r] ->
      tell [
        Pop r,
        Cmp r "0",
        Je elseLabel]
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
    localReserve 1 $ \[r] ->
      tell [
        Pop r,
        Cmp r "0",
        Je afterLabel]
    emitStmt stmt
    tell [
      Jmp beginLabel,
      Label afterLabel]
  For t ident e s -> emitFor t ident e s
  SExp expr -> do
    t <- emitExpr expr
    unless (t == Void) $
      tell [Add stackPointer "8"]

emitBlock :: Block -> CMonad ()
emitBlock (Block stmts) = do
  (vars, nextVar) <- getVars
  putVars (Map.empty : vars, nextVar)
  forM_ stmts emitStmt
  (vars', nextVar') <- getVars
  putVars (tail vars', nextVar')

emitFor :: Type -> Ident -> Expr -> Stmt -> CMonad ()
emitFor t ident e stmt = do
  let iIdent = Ident "_i"
      aIdent = Ident "_a"
      lenIdent = Ident "length"
      iVar = EVar iIdent
      aVar = EVar aIdent
      index = EIndex $ Index aVar iVar
  emitBlock $ Block [
    Decl Int [Init iIdent (ELitInt 0)],
    Decl (Array t) [Init aIdent e],
    While (ERel iVar LTH (EField aVar lenIdent)) $ BStmt $
      Block [
        Decl t [Init ident index],
        stmt
      ]
    ]

emitDecl :: Type -> Item -> CMonad ()
emitDecl t (Init ident expr) = do
  emitDecl t $ NoInit ident
  emitStmt $ Ass (EVar ident) expr
emitDecl t (NoInit (Ident name)) = do
  (scopeVars : vars, addr) <- getVars
  let scopeVars' = Map.insert name (addr, t) scopeVars
      (Address n) = addr
  putVars (scopeVars': vars, Address $ n + 8)
  tell [Mov (show addr) "0"]

emitAss :: Expr -> Expr -> CMonad ()
emitAss lv expr = do
  _ <- emitExpr expr
  emitLValue lv
  localReserve 2 $ \[l, e] ->
    tell [
      Pop l,
      Pop e,
      Mov ("qword[" ++ l ++ "]") e]
