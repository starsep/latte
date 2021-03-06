module EmitExpr where

import AbsLatte
import Asm
import Control.Monad
import Control.Monad.RWS (tell)
import Data.List.Split
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe
import {-# SOURCE #-} EmitClass
import Env
import Label
import State

binaryOp :: Expr -> Expr ->
  (String -> String -> AsmStmt) -> CMonad ()
binaryOp e1 e2 op = do
  _ <- emitExpr e1
  _ <- emitExpr e2
  localReserve 2 $ \[r1, r2] ->
    tell [
      Pop r2,
      Pop r1,
      op r1 r2,
      Push r1]

emitMulOp :: Expr -> Expr -> MulOp -> CMonad Type
emitMulOp e1 e2 mulOp = do
  _ <- emitExpr e1
  _ <- emitExpr e2
  localReserveReg mulOpArg $
    localReserve 1 $ \[r] ->
      tell [
        Pop r,
        Pop mulOpArg,
        Cqo,
        mulOpStmt mulOp r,
        Push $ mulOpResult mulOp]
  return Int

emitAddOp :: Expr -> Expr -> AddOp -> CMonad Type
emitAddOp e1 e2 Plus = do
  t <- localRWS $ emitExpr e1
  case t of
    Str -> void $ emitEApp (Ident "_concat") [e1, e2]
    _ -> binaryOp e1 e2 Add
  return t
emitAddOp e1 e2 Minus = do
  binaryOp e1 e2 Sub
  return Int

emitExpr :: Expr -> CMonad Type
emitExpr q = case q of
  ENull t _ -> do
    tell [Push "0"]
    return t
  ESubs subs -> dereference $ LSubs subs
  EVar ident -> dereference $ LVar ident
  EMethod lv ident args -> emitMethodInvoke lv ident args
  EField lv ident -> do
    t <- localRWS $ emitExpr lv
    case t of
      Array _ -> emitEApp (Ident "_arrayLength") [lv]
      _ -> emitField lv ident
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
    _ <- emitEApp (Ident "_newArray") [num]
    return $ Array t
  EClass className -> emitNewClass className
  EString s -> do
    label <- stringLiteralLabel s
    localReserveReg resultReg $
      tell [
        Mov (head argRegisters) label,
        Call "_copyStr",
        Push resultReg]
    return Str
  Neg expr -> do
    _ <- emitExpr expr
    localReserve 1 $ \[r] ->
      tell [Pop r, Negate r, Push r]
    return Int
  Not expr -> do
    _ <- emitExpr expr
    localReserve 1 $ \[r] ->
      tell [Pop r, Xor r "1", Push r]
    return Bool
  EMul e1 mulOp e2 -> emitMulOp e1 e2 mulOp
  EAdd e1 addOp e2 -> emitAddOp e1 e2 addOp
  EAnd e1 e2 -> do
    _ <- emitExpr e1
    (push0Label, afterAndLabel) <- andLabels
    localReserve 1 $ \[r] ->
      tell [
        Pop r,
        Cmp r "0",
        Je push0Label]
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
    localReserve 1 $ \[r] ->
      tell [
        Pop r,
        Cmp r "0",
        Jne push1Label]
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
    localReserve 2 $ \[r1, r2] -> do
      let r1_32 = to32bit r1
          r1_8 = to8bit r1
      tell [
        Pop r1,
        Pop r2,
        Cmp r1 r2,
        Set op r1_8,
        Movzx r1_32 r1_8,
        Push r1]
    return Bool

emitEApp :: Ident -> [Expr] -> CMonad Type
emitEApp ident args = do
  typed <- askTypedFns
  if Map.member ident typed then
    emitEApp' ident args Nothing
  else
    emitExpr $ EMethod (EVar (Ident "self")) ident args

emitEApp' :: Ident -> [Expr] -> Maybe ClassName -> CMonad Type
emitEApp' ident@(Ident name) args className = do
  let method = isJust className
      argsLen = length args
      argsRegs = take (min 6 argsLen) argRegisters
      argsToRemove = argsLen - 6
  forM_ (reverse args) emitExpr
  let callReg = last scratchRegisters
  when method $ do
    tell [Push "[rsp]"]
    let methodName = Ident $ head $ splitOn "@" name
    emitMethodVTable (fromJust className) methodName
    tell [Pop callReg]
  tell $ map Pop argsRegs
  let toCall = if method then callReg else name
  tell [Call toCall]
  when (argsToRemove > 0) $
    tell [Add stackPointer $ show $ argsToRemove * 8]
  typed <- askTypedFns
  let (Fun t _) = typed ! ident
  unless (t == Void) $
    localReserveReg resultReg $
      tell [Push resultReg]
  return t

emitEVarPtr :: Ident -> CMonad Type
emitEVarPtr (Ident name) = do
  (vars, _) <- getVars
  let varsScope = filter (Map.member name) vars
  if null varsScope then
    emitLField (EVar (Ident "self")) (Ident name)
  else do
    let (Address addr, t) = head varsScope ! name
    localReserve 1 $ \[r] ->
      tell [
        Mov r basePointer,
        Sub r $ show addr,
        Push r]
    return t

dereferenceTop :: CMonad ()
dereferenceTop =
  localReserve 1 $ \[r] ->
    tell [
      Pop r,
      Mov r $ "qword[" ++ r ++ "]",
      Push r]

dereference :: LValue -> CMonad Type
dereference lv = do
  t <- emitLValue lv
  dereferenceTop
  return t

callArrayPtr :: Expr -> CMonad ()
callArrayPtr index = do
  let regs@[a1, a2] = take 2 argRegisters
  void $ emitExpr index
  localReserveRegs regs $
    tell [
      Pop a2,
      Pop a1,
      Call "_arrayPtr",
      Push resultReg]

emitLValue :: LValue -> CMonad Type
emitLValue q = case q of
  LVar ident -> emitEVarPtr ident
  LSubs (Subs array index) -> do
    (Array t) <- emitExpr array
    callArrayPtr index
    return t
  LSubs (SubsR subs index) -> do
    (Array t) <- emitLValue $ LSubs subs
    dereferenceTop
    callArrayPtr index
    return t
  LField obj name -> emitLField obj name
