module TypecheckerState (
  Ident(Ident), TypedFnDefs, TCEnv, TCIdentState, TCDeclState, TCState, TCMonad,
  getState, putState, getDecl, putDecl, getContext, putContext,
  addContext, dropContext
) where

import AbsLatte
import Context
import Control.Monad.RWS (RWST, get, put, void)
import Data.Map (Map)
import Print (Ident(Ident))

type TypedFnDefs = Map Ident Type
type TCEnv = (TypedFnDefs, Type)
type TCIdentState = Map Ident (Bool, Type)
type TCDeclState = [Ident]
type TCState = (TCIdentState, TCDeclState, Context)
type TCMonad = RWST TCEnv () TCState IO

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

addContext :: ContextItem -> TCMonad ()
addContext contextItem = do
  (Context context) <- getContext
  void $ putContext $ Context $ contextItem : context

dropContext :: TCMonad ()
dropContext = do
  (Context context) <- getContext
  void $ putContext $ Context $ tail context
