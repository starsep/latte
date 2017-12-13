module TypecheckerState where

import AbsLatte
import Context
import Control.Monad
import Control.Monad.RWS (RWST, ask, get, lift, put, void)
import Data.Map (Map)
import Errors

type TypedFnDefs = Map Ident Type
type ClassDef = [ClassProp]
type ClassDefs = Map Ident ClassDef
type InheritanceTree = [(Ident, Ident)]
type TCEnv = (TypedFnDefs, Type, ClassDefs, InheritanceTree)
type TCIdentState = Map Ident Type
type TCDeclState = [Ident]
type TCState = (TCIdentState, TCDeclState, Context)
type TCMonad = RWST TCEnv () TCState IO

showError :: (Context -> IO ()) -> TCMonad ()
showError f = do
  context <- getContext
  lift $ f context

showErrorV :: (Context -> IO ()) -> TCMonad Type
showErrorV f = do
  _ <- showError f
  return Void

addDecl :: Ident -> TCMonad ()
addDecl ident = do
  (s, decl, context) <- get
  when (ident `elem` decl) $ showError $ Errors.alreadyDecl ident
  put (s, ident : decl, context)

getState :: TCMonad TCIdentState
getState = do
  (s, _, _) <- get
  return s

putState :: TCIdentState -> TCMonad TCIdentState
putState newState = do
  (oldState, decl, context) <- get
  put (newState, decl, context)
  return oldState

getDecl :: TCMonad TCDeclState
getDecl = do
  (_, decl, _) <- get
  return decl

putDecl :: TCDeclState -> TCMonad TCDeclState
putDecl newDecl = do
  (state, oldDecl, context) <- get
  put (state, newDecl, context)
  return oldDecl

getContext :: TCMonad Context
getContext = do
  (_, _, context) <- get
  return context

putContext :: Context -> TCMonad Context
putContext newContext = do
  (state, decl, oldContext) <- get
  put (state, decl, newContext)
  return oldContext

askTyped :: TCMonad TypedFnDefs
askTyped = do
  (typed, _, _, _) <- ask
  return typed

askReturn :: TCMonad Type
askReturn = do
  (_, ret, _, _) <- ask
  return ret

askClassDefs :: TCMonad ClassDefs
askClassDefs = do
  (_, _, classDefs, _) <- ask
  return classDefs

askInheritanceTree :: TCMonad InheritanceTree
askInheritanceTree = do
  (_, _, _, inheritanceTree) <- ask
  return inheritanceTree

addContext :: ContextItem -> TCMonad ()
addContext contextItem = do
  (Context context) <- getContext
  void $ putContext $ Context $ contextItem : context

addContextStmt :: ContextItem -> TCMonad ()
addContextStmt contextItem = do
  (Context context) <- getContext
  let contextNoStmt = filter isNotStmt context
  void $ putContext $ Context $ contextItem : contextNoStmt

dropContext :: TCMonad ()
dropContext = do
  (Context context) <- getContext
  void $ putContext $ Context $ tail context
