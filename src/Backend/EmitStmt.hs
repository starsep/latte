module EmitStmt where

import AbsLatte
import Asm
import Control.Monad
import Control.Monad.RWS (tell)
import qualified Data.Map as Map
import EmitExpr
import GC
import Label
import State

emitStmt :: Stmt -> CMonad ()
emitStmt q = case q of
  Empty -> return ()
  BStmt block -> emitBlock block
  Decl t items -> forM_ items $ emitDecl t
  Ass left expr -> emitAss left expr
  Incr lv -> emitStmt $ Ass lv (EAdd lv Plus (ELitInt 1))
  Decr lv -> emitStmt $ Ass lv (EAdd lv Minus (ELitInt 1))
  Ret expr -> do
    _ <- emitExpr expr
    tell [Pop resultReg]
    emitReturn
  VRet -> emitReturn
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

emitReturn :: CMonad ()
emitReturn = do
  gcScopeVars
  jumpEndLabel

emitBlock :: Block -> CMonad ()
emitBlock (Block stmts) = do
  (vars, nextVar) <- getVars
  putVars (Map.empty : vars, nextVar)
  forM_ stmts emitStmt
  (vars', nextVar') <- getVars
  gcScopeVars
  putVars (tail vars', nextVar')

emitFor :: Type -> Ident -> Expr -> Stmt -> CMonad ()
emitFor t ident e stmt = do
  let iIdent = Ident "_i"
      aIdent = Ident "_a"
      lenIdent = Ident "length"
      iVar = EVar iIdent
      aVar = EVar aIdent
      subs = ESubs $ Subs aVar iVar
  emitBlock $ Block [
    Decl Int [Init iIdent (ELitInt 0)],
    Decl (Array t) [Init aIdent e],
    While (ERel iVar LTH (EField aVar lenIdent)) $ BStmt $
      Block [
        Decl t [Init ident subs],
        Incr (EVar iIdent),
        stmt
      ]
    ]

emitDecl :: Type -> Item -> CMonad ()
emitDecl t (Init ident expr) = do
  void $ emitExpr expr
  emitDecl t $ NoInit ident
  assign t $ LVar ident
emitDecl t (NoInit (Ident name)) = do
  (scopeVars : vars, addr) <- getVars
  let scopeVars' = Map.insert name (addr, t) scopeVars
      (Address n) = addr
  putVars (scopeVars': vars, Address $ n + 8)
  tell [Mov (show addr) "0"]

exprToLValue :: Expr -> LValue
exprToLValue q = case q of
  EVar ident -> LVar ident
  ESubs subs -> LSubs subs
  EField obj name -> LField obj name
  _ -> error $ "converting " ++ show q ++ " to lvalue isn't implemented"

emitAss :: Expr -> Expr -> CMonad ()
emitAss left expr = do
  t <- emitExpr expr
  assign t $ exprToLValue left

assign :: Type -> LValue -> CMonad ()
assign t lv = do
  when (isGCType t) $ do
    tell [Push $ "[" ++ stackPointer ++ "]"]
    gcIncr
    -- TODO: GC
    -- void $ dereference lv
    -- gcDecr
  _ <- emitLValue lv
  localReserve 2 $ \[l, e] ->
    tell [
      Pop l,
      Pop e,
      Mov ("qword[" ++ l ++ "]") e]
