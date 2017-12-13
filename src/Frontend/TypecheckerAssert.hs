module TypecheckerAssert where

import {-# SOURCE #-} Typechecker (typeOf)

import AbsLatte
import Context
import Control.Monad
import Errors
import TypecheckerPure
import TypecheckerState
import qualified Data.Map as Map
import Data.Map ((!))

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
  when (t /= typeof) $
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
  unless (isSimpleType t) $
  case t of
    Array t' -> assertArrayableType t'
    _ -> showError $ Errors.arrayOfBadType t
