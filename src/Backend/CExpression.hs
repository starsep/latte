module CExpression where

import AbsLatte
import AsmStmt
import CompilerState
import Control.Monad.RWS (tell)

binaryOp :: Expr -> Expr ->
  (String -> String -> AsmStmt) -> CMonad ()
binaryOp e1 e2 op = do
  emitExpression e1
  emitExpression e2
  tell [
    Pop "rax",
    Pop "rdi",
    op "rax" "rdi",
    Push "rax"]

relOpToAsm :: RelOp -> String
relOpToAsm op = case op of
  LTH -> "setl"
  LE -> "setle"
  GTH -> "setg"
  GE -> "setge"
  EQU -> "sete"
  NE -> "setne"

emitMulOp :: Expr -> Expr -> MulOp -> CMonad ()
emitMulOp e1 e2 mulOp = do
  emitExpression e1
  emitExpression e2
  let asmOp = if mulOp == Times then Mul else Divide
      result = if mulOp == Mod then "rdx" else "rax"
  tell [
    Pop "rcx",
    Pop "rax",
    Custom "cqo" [],
    asmOp "rcx",
    Push result]

emitExpression :: Expr -> CMonad ()
emitExpression q = case q of
  EApp ident args -> emitEApp ident args
  ELitInt number -> tell [Push $ show number]
  ELitTrue -> tell [Push "1"]
  ELitFalse -> tell [Push "0"]
  Neg expr -> do
    emitExpression expr
    tell [Xor "rax" "rax", Pop "rdi", Sub "rax" "rdi", Push "rax"]
  Not expr -> do
    emitExpression expr
    tell [Pop "rax", Xor "rax" "1", Push "rax"]
  EMul e1 mulOp e2 -> emitMulOp e1 e2 mulOp
  EAdd e1 addOp e2 -> case addOp of
    Plus -> binaryOp e1 e2 Add
    Minus -> binaryOp e1 e2 Sub
  EAnd e1 e2 -> binaryOp e1 e2 And
  EOr e1 e2 -> binaryOp e1 e2 Or
  ERel e1 op e2 -> do
    emitExpression e1
    emitExpression e2
    tell [
      Pop "rax",
      Pop "rdi",
      Cmp "rax" "rdi",
      Custom (relOpToAsm op) ["al"],
      Custom "movzx" ["eax", "al"],
      Push "rax"]
  EString s -> return () -- TODO
  _ -> return () -- TODO

emitEApp :: Ident -> [Expr] -> CMonad ()
emitEApp (Ident "printInt") [arg] = do
  emitExpression arg
  tell [Pop "rsi", Call "printInt"]

emitEApp (Ident "printString") [arg] = do
  emitExpression arg
  tell [Pop "rdi", Call "printString"]

emitEApp _ _ = return () -- TODO
