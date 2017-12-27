module EmitExpr where

import AbsLatte
import Asm
import AsmStmt
import CompilerState
import Control.Monad
import Control.Monad.RWS (tell)
import Data.Map ((!))
import Label

binaryOp :: Expr -> Expr ->
  (String -> String -> AsmStmt) -> CMonad ()
binaryOp e1 e2 op = do
  _ <- emitExpr e1
  _ <- emitExpr e2
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

emitMulOp :: Expr -> Expr -> MulOp -> CMonad Type
emitMulOp e1 e2 mulOp = do
  _ <- emitExpr e1
  _ <- emitExpr e2
  let asmOp = if mulOp == Times then Mul else Divide
      result = if mulOp == Mod then "rdx" else "rax"
  tell [
    Pop "rcx",
    Pop "rax",
    Custom "cqo" [],
    asmOp "rcx",
    Push result]
  return Int

emitAddOp :: Expr -> Expr -> AddOp -> CMonad Type
emitAddOp e1 e2 Plus = do
  t <- emitExpr e1
  tell [Pop "rax"]
  case t of
    Str -> void $ emitEApp (Ident "_concat") [e1, e2]
    Int -> binaryOp e1 e2 Add
    _ -> return () -- TODO: error
  return t
emitAddOp e1 e2 Minus = do
  binaryOp e1 e2 Sub
  return Int

emitExpr :: Expr -> CMonad Type
emitExpr q = case q of
  ENull t -> do
    tell [Push "0"]
    return t
  EIndex index -> return Int -- TODO: implement
  EVar ident -> return Int -- TODO: implement
  EMethod lv ident args -> return Int -- TODO: implement
  EField lv ident -> return Int -- TODO: implement
  ELitInt number -> do
    tell [Push $ show number]
    return Int
  ELitTrue -> do
    tell [Push "1"]
    return Bool
  ELitFalse -> do
    tell [Push "0"]
    return Bool
  EApp ident args -> emitEApp ident args
  EArray t num -> do
    emitEApp (Ident "_new") [num]
    return $ Array t
  EClass t -> do
    let size = 8 -- TODO: correct size of class
    emitEApp (Ident "_new") [ELitInt size]
    return t
  EString s -> do
    label <- stringLiteralLabel s
    tell [
      Mov "rdi" label,
      Call "_copyStr",
      Push "rax"]
    return Str
  Neg expr -> do
    _ <- emitExpr expr
    tell [Pop "rax", Custom "neg" ["rax"], Push "rax"]
    return Int
  Not expr -> do
    _ <- emitExpr expr
    tell [Pop "rax", Xor "rax" "1", Push "rax"]
    return Bool
  EMul e1 mulOp e2 -> emitMulOp e1 e2 mulOp
  EAdd e1 addOp e2 -> emitAddOp e1 e2 addOp
  EAnd e1 e2 -> do
    _ <- emitExpr e1
    (push0Label, afterAndLabel) <- andLabels
    tell [
      Pop "rax",
      Cmp "rax" "0",
      Custom "je" [push0Label]]
    _ <- emitExpr e2
    tell [
      Jmp afterAndLabel,
      Label push0Label,
      Push "0",
      Label afterAndLabel]
    return Bool
  EOr e1 e2 -> do
    _ <- emitExpr e1
    (push1Label, afterOrLabel) <- orLabels
    tell [
      Pop "rax",
      Cmp "rax" "0",
      Custom "jne" [push1Label]]
    _ <- emitExpr e2
    tell [
      Jmp afterOrLabel,
      Label push1Label,
      Push "1",
      Label afterOrLabel]
    return Bool
  ERel e1 op e2 -> do
    _ <- emitExpr e2
    _ <- emitExpr e1
    tell [
      Pop "rax",
      Pop "rdi",
      Cmp "rax" "rdi",
      Custom (relOpToAsm op) ["al"],
      Custom "movzx" ["eax", "al"],
      Push "rax"]
    return Bool

emitEApp :: Ident -> [Expr] -> CMonad Type
emitEApp ident@(Ident name) args = do
  forM_ (reverse args) emitExpr
  moveArgsToRegisters 0 $ min 6 $ length args
  tell [Call name]
  let argsToPop = length args - 6
  when (argsToPop > 0) $
    tell [Add "rsp" $ show $ argsToPop * 8]
  tell [Push "rax"]
  typed <- askTypedFns
  return $ typed ! ident

moveArgsToRegisters :: Int -> Int -> CMonad ()
moveArgsToRegisters i n
  | i == n = return ()
  | otherwise = do
    tell [Pop $ argRegisters !! i]
    moveArgsToRegisters (i + 1) n
