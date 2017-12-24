module CExpression where

import AbsLatte
import AsmStmt
import CompilerState
import Control.Monad.RWS (tell)

emitExpression :: Expr -> CMonad ()
emitExpression q = case q of
  EApp ident args -> emitEApp ident args
  EAdd e1 addOp e2 -> do
    emitExpression e1
    emitExpression e2
    tell [Pop "rax", Pop "rdi"]
    case addOp of
      Plus -> tell [Add "rax" "rdi"]
      Minus -> tell [Sub "rax" "rdi"]
    tell [Push "rax"]
  ELitInt number -> tell [Push $ show number]

emitEApp :: Ident -> [Expr] -> CMonad ()
emitEApp (Ident "printInt") [arg] = do
  emitExpression arg
  tell [Pop "rsi", Call "printInt"]
