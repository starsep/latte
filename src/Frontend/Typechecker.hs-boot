module Typechecker where

import AbsLatte
import TypecheckerState

typeOf :: Expr -> TCMonad Type
