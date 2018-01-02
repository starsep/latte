module Typechecker where

import AbsLatte
import Env

typeOf :: Expr -> TCMonad Type
