module Assert where

import AbsLatte
import {-# SOURCE #-} Classes
import Context
import Control.Monad
import Data.Map ((!))
import qualified Data.Map as Map
import Env
import Errors
import Pure
import {-# SOURCE #-} Typecheck (typeOf)

assertCorrectMain :: TypedFnDefs -> IO ()
assertCorrectMain typedFns = do
  let context = Context []
  when (Map.notMember (Ident "main") typedFns) $
    Errors.noMain context
  case typedFns ! Ident "main" of
    Fun Int [] -> return ()
    _ -> Errors.badMain context

assertNumericExpr :: Expr -> TCMonad ()
assertNumericExpr expr = do
  t <- typeOf expr
  case t of
    Int -> return ()
    _ -> showError $ Errors.nonNumeric expr t

assertType :: Expr -> Type -> TCMonad ()
assertType expr t = do
  typeof <- typeOf expr
  compatible <- isCompatibleType t typeof
  unless compatible $
    showError $ Errors.expectedExpression expr typeof t

assertVarDeclared :: Ident -> TCMonad ()
assertVarDeclared ident = do
  state <- getState
  when (Map.notMember ident state) $
    showError $ Errors.variableUndeclared ident

assertComparable :: Expr -> TCMonad ()
assertComparable expr = do
  t <- typeOf expr
  unless (isComparable t) $
    showError $ Errors.nonComparable expr t

assertBExpr :: Expr -> TCMonad ()
assertBExpr bExpr = do
  t <- typeOf bExpr
  case t of
    Bool -> return ()
    _ -> showError $ Errors.nonBoolean bExpr

assertArrayableType :: Type -> TCMonad ()
assertArrayableType t =
  when (t == Void) $
  showError $ Errors.arrayOfBadType t
