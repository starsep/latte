module Env where

import AbsLatte
import Context
import Control.Monad
import Control.Monad.RWS (RWST, ask, get, lift, put)
import Data.Map (Map, (!))
import Errors

type TypecheckOutput = (TypedFnDefs, ClassesData)

type ClassName = Ident
data ClassField = ClassField Int Ident Type deriving (Show)
data ClassMethod = ClassMethod Int Ident ClassName [Type] deriving (Show)
type ClassData = ([ClassMethod], [ClassField])
type ClassesData = Map Ident ClassData

type TopDefScope =
  (TypedFnDefs, ClassDefs, InheritanceTree, ClassNames, ClassesData)
type TypedFnDefs = Map Ident Type
type ClassDef = [ClassProp]
type ClassDefs = Map Ident ClassDef
type ClassNames = [ClassName]

type InheritanceTree = Map Ident (Maybe Ident)

type TCEnv = (Ident, TopDefScope, Maybe Ident)
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
  error ""

addDecl :: Ident -> TCMonad ()
addDecl ident = do
  (s, decl, context) <- get
  when (ident `elem` decl) $ showError $ Errors.alreadyDecl ident
  put (s, ident : decl, context)

getState :: TCMonad TCIdentState
getState = do
  (s, _, _) <- get
  return s

putState :: TCIdentState -> TCMonad ()
putState newState = do
  (_, decl, context) <- get
  put (newState, decl, context)

getContext :: TCMonad Context
getContext = do
  (_, _, context) <- get
  return context

putContext :: Context -> TCMonad ()
putContext newContext = do
  (state, decl, _) <- get
  put (state, decl, newContext)

askTyped :: TCMonad TypedFnDefs
askTyped = do
  (_, (typed, _, _, _, _), _) <- ask
  return typed

askIdent :: TCMonad Ident
askIdent = do
  (ident, _, _) <- ask
  return ident

askClassName :: TCMonad (Maybe Ident)
askClassName = do
  (_, _, className) <- ask
  return className

askScope :: TCMonad TopDefScope
askScope = do
  (_, scope, _) <- ask
  return scope

askReturn :: TCMonad Type
askReturn = do
  ident <- askIdent
  typed <- askTyped
  let (Fun t _) = typed ! ident
  return t

askClassDefs :: TCMonad ClassDefs
askClassDefs = do
  (_, classDefs, _, _, _) <- askScope
  return classDefs

askClassData :: Ident -> TCMonad ClassData
askClassData className = do
  (_, _, _, _, classesData) <- askScope
  return $ classesData ! className

askProps :: TCMonad ClassDef
askProps = do
  name <- askIdent
  classDefs <- askClassDefs
  return $ classDefs ! name

askInheritanceTree :: TCMonad InheritanceTree
askInheritanceTree = do
  (_, _, inheritanceTree, _, _) <- askScope
  return inheritanceTree

askClassNames :: TCMonad ClassNames
askClassNames = do
  (_, _, _, classNames, _) <- askScope
  return classNames

addContext :: ContextItem -> TCMonad ()
addContext contextItem = do
  (Context context) <- getContext
  putContext $ Context $ contextItem : context

addContextStmt :: ContextItem -> TCMonad ()
addContextStmt contextItem = do
  (Context context) <- getContext
  let contextNoStmt = filter isNotStmt context
  putContext $ Context $ contextItem : contextNoStmt

dropContext :: TCMonad ()
dropContext = do
  (Context context) <- getContext
  unless (null context) $
    putContext $ Context $ tail context
