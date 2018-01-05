module Env where

import AbsLatte
import Context
import Control.Monad
import Control.Monad.RWS (RWST, ask, get, lift, put, void)
import Data.Map (Map, (!))
import Errors

type TypecheckOutput = (TypedFnDefs, ClassesData)

data ClassField = ClassField Int Ident Type deriving (Show)
data ClassMethod = ClassMethod Int Ident Ident deriving (Show)
type ClassData = ([ClassMethod], [ClassField])
type ClassesData = Map Ident ClassData

type TopDefScope = (TypedFnDefs, ClassDefs, InheritanceTree, ClassNames)
type TypedFnDefs = Map Ident Type
type ClassDef = [ClassProp]
type ClassDefs = Map Ident ClassDef
type ClassNames = [Ident]

type InheritanceTree = Map Ident (Maybe Ident)

type TCEnv = (TypedFnDefs, Ident, ClassDefs, InheritanceTree, ClassNames)
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
  (typed, _, _, _, _) <- ask
  return typed

askIdent :: TCMonad Ident
askIdent = do
  (_, ident, _, _, _) <- ask
  return ident

askReturn :: TCMonad Type
askReturn = do
  ident <- askIdent
  typed <- askTyped
  let (Fun t _) = typed ! ident
  return t

askClassDefs :: TCMonad ClassDefs
askClassDefs = do
  (_, _, classDefs, _, _) <- ask
  return classDefs

askProps :: TCMonad ClassDef
askProps = do
  name <- askIdent
  classDefs <- askClassDefs
  return $ classDefs ! name

askInheritanceTree :: TCMonad InheritanceTree
askInheritanceTree = do
  (_, _, _, inheritanceTree, _) <- ask
  return inheritanceTree

askClassNames :: TCMonad ClassNames
askClassNames = do
  (_, _, _, _, classNames) <- ask
  return classNames

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
  unless (null context) $
    void $ putContext $ Context $ tail context
