module CExpression where

import AbsLatte
import AsmStandard
import AsmStmt
import CompilerState
import Control.Monad
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
  ENull _ -> tell [Push "0"]
  EApp ident args -> do
    emitEApp ident args
    let argsToPop = length args - 6
    when (argsToPop > 0) $
      tell [Add "rsp" $ show $ argsToPop * 8]
    tell [Push "rax"]
  ELitInt number -> tell [Push $ show number]
  ELitTrue -> tell [Push "1"]
  ELitFalse -> tell [Push "0"]
  EArray _ num -> emitEApp (Ident "__new") [num]
  Neg expr -> do
    emitExpression expr
    tell [Pop "rax", Custom "neg" ["rax"], Push "rax"]
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
  _ -> return () -- TODO

emitEApp :: Ident -> [Expr] -> CMonad ()
emitEApp (Ident name) args = emitEApp' name args 0

emitEApp' :: String -> [Expr] -> Int -> CMonad ()
emitEApp' name [] _ = tell [Call name]
emitEApp' name args 6 = do
  let revArgs = reverse args
  forM_ revArgs emitExpression
  emitEApp' name [] 0
emitEApp' name (arg:args) nth = do
  emitExpression arg
  tell [Pop $ argRegisters !! nth]
  emitEApp' name args $ nth + 1
