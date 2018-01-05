module EmitClass where

import AbsLatte
import State

emitMethod :: Ident -> Ident -> [Arg] -> Block -> CMonad ()
emitVTables :: CMonad ()
emitNewClass :: Ident -> CMonad Type
emitField :: Expr -> Ident -> CMonad Type
emitMethodInvoke :: Expr -> Ident -> [Expr] -> CMonad Type
emitLField :: Expr -> Ident -> CMonad Type
