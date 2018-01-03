module Typecheck where

import AbsLatte
import Env

typeOf :: Expr -> TCMonad Type
