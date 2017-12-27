module EmitExpr where

import AbsLatte
import AsmStandard
import AsmStmt
import CompilerState
import Control.Monad
import Control.Monad.RWS (tell)
import Label

binaryOp :: Expr -> Expr ->
  (String -> String -> AsmStmt) -> CMonad ()
binaryOp e1 e2 op = do
  emitExpr e1
  emitExpr e2
  tell [
    Pop "rdi",
    Pop "rax",
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
  emitExpr e1
  emitExpr e2
  let asmOp = if mulOp == Times then Mul else Divide
      result = if mulOp == Mod then "rdx" else "rax"
  tell [
    Pop "rcx",
    Pop "rax",
    Custom "cqo" [],
    asmOp "rcx",
    Push result]

emitExpr :: Expr -> CMonad ()
emitExpr q = case q of
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
    emitExpr expr
    tell [Pop "rax", Custom "neg" ["rax"], Push "rax"]
  Not expr -> do
    emitExpr expr
    tell [Pop "rax", Xor "rax" "1", Push "rax"]
  EMul e1 mulOp e2 -> emitMulOp e1 e2 mulOp
  EAdd e1 addOp e2 -> case addOp of
    Plus -> binaryOp e1 e2 Add
    Minus -> binaryOp e1 e2 Sub
  EAnd e1 e2 -> do
    emitExpr e1
    (push0Label, afterAndLabel) <- andLabels
    tell [
      Pop "rax",
      Cmp "rax" "0",
      Custom "je" [push0Label]]
    emitExpr e2
    tell [
      Jmp afterAndLabel,
      Label push0Label,
      Push "0",
      Label afterAndLabel]
  EOr e1 e2 -> do
    emitExpr e1
    (push1Label, afterOrLabel) <- orLabels
    tell [
      Pop "rax",
      Cmp "rax" "0",
      Custom "jne" [push1Label]]
    emitExpr e2
    tell [
      Jmp afterOrLabel,
      Label push1Label,
      Push "1",
      Label afterOrLabel]
  ERel e1 op e2 -> do
    emitExpr e1
    emitExpr e2
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
  forM_ revArgs emitExpr
  emitEApp' name [] 0
emitEApp' name (arg:args) nth = do
  emitExpr arg
  tell [Pop $ argRegisters !! nth]
  emitEApp' name args $ nth + 1
