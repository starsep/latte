module Typechecker (typecheck, itemIdent) where

import AbsLatte
import Control.Monad
import Control.Monad.RWS (RWST, ask, get, put, lift, runRWST)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Errors
import PrintLatte (printTree)
import Data.Int

type TypedFnDefs = Map Ident Type
type TCEnv = (TypedFnDefs, Type)
type TCIdentState = Map Ident (Bool, Type)
type TCDeclState = [Ident]
type TCState = (TCIdentState, TCDeclState)
type TCMonad = RWST TCEnv () TCState IO

getState :: TCMonad TCIdentState
getState = do
  (s, _) <- get
  return s

putState :: TCIdentState -> TCMonad ()
putState newState = do
  decl <- getDecl
  put (newState, decl)

getDecl :: TCMonad TCDeclState
getDecl = do
  (_, decl) <- get
  return decl

addDecl :: Ident -> TCMonad ()
addDecl ident = do
  (s, decl) <- get
  when (ident `elem` decl) $ lift $ Errors.alreadyDecl ident
  put (s, ident : decl)

typeOf :: Expr -> TCMonad Type
typeOf q =
  case q of
    EApp ident args -> outputType ident args
    EVar ident -> typeOfIdent ident
    ELitInt x -> do
      let minInt = toInteger (minBound :: Int32)
      let maxInt = toInteger (maxBound :: Int32)
      when (x < minInt || x > maxInt) $
        lift $ Errors.int32 x
      return Int
    EString _ -> return Str
    ELitFalse -> return Bool
    ELitTrue -> return Bool
    Neg expr -> checkNeg expr
    Not expr -> do
      assertBExpr expr
      return Bool
    EAdd e1 op e2 -> checkAddOp e1 op e2
    EMul e1 mulOp e2 -> do
      when (mulOp == Mod) $ do
        assertType e1 Int
        assertType e2 Int
      checkNumOp e1 e2 (printTree mulOp)
    ERel e1 op e2 -> do
      checkRelOp e1 e2 (printTree op)
      return Bool
    EOr b1 b2 -> checkBExprOp b1 b2
    EAnd b1 b2 -> checkBExprOp b1 b2

typeOfFun :: Ident -> TCMonad Type
typeOfFun ident = do
  (typed, _) <- ask
  return $ typed ! ident

typeOfVar :: Ident -> TCMonad Type
typeOfVar ident = do
  state <- getState
  assertVarDeclared ident
  let (_, t) = state ! ident
  return t

typeOfIdent :: Ident -> TCMonad Type
typeOfIdent ident = do
  (typed, _) <- ask
  if Map.member ident typed then
    typeOfFun ident
  else
    typeOfVar ident

checkBExprOp :: Expr -> Expr -> TCMonad Type
checkBExprOp b1 b2 = do
  assertBExpr b1
  assertBExpr b2
  return Bool

checkBinOp :: Expr -> Expr -> String -> TCMonad Type
checkBinOp expr1 expr2 name = do
  t1 <- typeOf expr1
  t2 <- typeOf expr2
  unless (t1 == t2) $ lift $ Errors.diffTypesBinOp name t1 t2
  return t1

assertNumericExpr :: Expr -> TCMonad ()
assertNumericExpr expr = do
  t <- typeOf expr
  case t of
    Int -> return ()
    _ -> lift $ Errors.nonNumeric expr t

checkNeg :: Expr -> TCMonad Type
checkNeg expr = do
  assertNumericExpr expr
  typeOf expr

checkAddOp :: Expr -> AddOp -> Expr -> TCMonad Type
checkAddOp e1 Plus e2 = do
  t <- typeOf e1
  if t == Str then do
    assertType e2 Str
    return Str
  else
    checkNumOp e1 e2 (printTree Plus)

checkAddOp e1 Minus e2 = checkNumOp e1 e2 (printTree Minus)

checkNumOp :: Expr -> Expr -> String -> TCMonad Type
checkNumOp expr1 expr2 name = do
  assertNumericExpr expr1
  assertNumericExpr expr2
  checkBinOp expr1 expr2 name

checkArgs :: Ident -> [Expr] -> [Type] -> TCMonad ()
checkArgs ident args types = do
  let nArgs = length args
      expected = length types
  when (nArgs /= expected) $ lift $ Errors.numberOfArgs ident nArgs expected
  argsTypes <- mapM typeOf args
  when (argsTypes /= types) $ lift $ Errors.typesOfArgs ident argsTypes types

outputTypeFun :: Ident -> [Expr] -> TCMonad Type
outputTypeFun ident args = do
  (typed, _) <- ask
  let (Fun out argsTypes) = typed ! ident
  checkArgs ident args argsTypes
  return out

outputType :: Ident -> [Expr] -> TCMonad Type
outputType ident args = do
  (typed, _) <- ask
  if Map.member ident typed then
    outputTypeFun ident args
  else do
    _ <- lift $ Errors.functionUndeclared ident
    return Int

fnHeaderToFnType :: Type -> [Arg] -> Type
fnHeaderToFnType outType args =
  Fun outType $ map (\arg -> case arg of Arg t _ -> t) args

addTypedFnDef :: TypedFnDefs -> TopDef -> IO TypedFnDefs
addTypedFnDef typed (FnDef outType ident args _) = do
  when (Map.member ident typed) $ Errors.multipleFnDef ident
  return $ Map.insert ident (fnHeaderToFnType outType args) typed

assertCorrectMain :: TypedFnDefs -> IO ()
assertCorrectMain typedFns = do
  when (Map.notMember (Ident "main") typedFns) Errors.noMain
  case typedFns ! Ident "main" of
    Fun Int [] -> return ()
    _ -> Errors.badMain

checkShadow :: Ident -> TCMonad ()
checkShadow ident = do
  (typed, _) <- ask
  state <- getState
  when (Map.member ident typed) $ lift $ Errors.shadowTopDef ident
  -- when (Map.member ident state) $ lift $ Errors.shadowVariable ident

assertType :: Expr -> Type -> TCMonad ()
assertType expr t = do
  typeof <- typeOf expr
  when (t /= typeof) $ lift $ Errors.expectedExpression expr typeof t

assertVarDeclared :: Ident -> TCMonad ()
assertVarDeclared ident = do
  state <- getState
  when (Map.notMember ident state) $ lift $ Errors.variableUndeclared ident

itemIdent :: Item -> Ident
itemIdent item = case item of
  Init ident _ -> ident
  NoInit ident -> ident

typecheckDecl :: Item -> Type -> TCMonad ()
typecheckDecl item t = do
  when (t == Void) $ lift $ Errors.voidVariable $ itemIdent item
  let ident = itemIdent item
  case item of
    NoInit _ -> return ()
    Init _ expr -> assertType expr t
  state <- getState
  addDecl ident
  checkShadow ident
  putState (Map.insert (itemIdent item) (False, t) state)

typecheckIncr :: Ident -> TCMonad ()
typecheckIncr ident = do
  assertVarDeclared ident
  assertType (EVar ident) Int

typecheckAss :: Ident -> Expr -> TCMonad ()
typecheckAss ident expr = do
  ltype <- typeOf $ EVar ident
  assertType expr ltype

assertComparable :: Expr -> TCMonad ()
assertComparable expr = do
  t <- typeOf expr
  case t of
    Fun _ _ -> lift $ Errors.nonComparable expr t
    _ -> return ()

checkRelOp :: Expr -> Expr -> String -> TCMonad ()
checkRelOp expr1 expr2 name = do
  assertComparable expr1
  void $ checkBinOp expr1 expr2 name

assertBExpr :: Expr -> TCMonad ()
assertBExpr bExpr = do
  t <- typeOf bExpr
  case t of
    Bool -> return ()
    _ -> lift $ Errors.nonBoolean bExpr

typecheckStmt :: Stmt -> TCMonad ()
typecheckStmt stmt = do
  (_, returnType) <- ask
  case stmt of
    Empty -> return ()
    BStmt block -> typecheckBlock block
    Decl t items -> forM_ items (`typecheckDecl` t)
    Ass ident expr -> typecheckAss ident expr
    Incr ident -> typecheckIncr ident
    Decr ident -> typecheckIncr ident
    Ret expr -> do
      when (returnType == Void) $ lift $ Errors.retVoid expr
      t <- typeOf expr
      when (returnType /= t) $ lift $ Errors.badRetType expr t returnType
    VRet -> when (returnType /= Void) $ lift $ Errors.vRetNoVoid returnType
    While bExpr stmt' -> do
      assertBExpr bExpr
      typecheckStmt stmt'
    SExp expr -> void $ typeOf expr
    Cond expr stmt' -> do
      assertBExpr expr
      typecheckStmt stmt'
    CondElse expr stmt1 stmt2 -> do
      assertBExpr expr
      typecheckStmt stmt1
      typecheckStmt stmt2

typecheckBlock :: Block -> TCMonad ()
typecheckBlock (Block stmts) = do
  (state, decl) <- get
  put (state, [])
  forM_ stmts typecheckStmt
  put (state, decl)

addFunctionArgToState :: TCIdentState -> Arg -> IO TCIdentState
addFunctionArgToState state (Arg t arg) = do
  when (t == Void) $ Errors.voidArgument arg
  when (Map.member arg state) $ Errors.sameArgNames arg
  return $ Map.insert arg (False, t) state

typecheckFunction :: TopDef -> TypedFnDefs -> IO ()
typecheckFunction (FnDef outType i args body) typed = do
  funState <- foldM addFunctionArgToState Map.empty args
  void $ runRWST (typecheckBlock body) (typed, outType) (funState, [])
  when ((outType /= Void) && not (isReturning (BStmt body))) $
    Errors.notReturning i

isTrue :: Expr -> Bool
isTrue bExpr = case bExpr of
  ELitFalse -> False
  ELitTrue -> True
  Not expr -> not $ isTrue expr
  EOr b1 b2 -> isTrue b1 || isTrue b2
  EAnd b1 b2 -> isTrue b1 && isTrue b2
  _ -> False

isFalse :: Expr -> Bool
isFalse bExpr = case bExpr of
  ELitFalse -> True
  ELitTrue -> False
  Not expr -> not $ isFalse expr
  EOr b1 b2 -> isFalse b1 && isFalse b2
  EAnd b1 b2 -> isFalse b1 || isFalse b2
  _ -> False

mightBeReturning :: Stmt -> Bool
mightBeReturning stmt =
  isReturning stmt || case stmt of
    BStmt (Block s) -> any mightBeReturning s
    Cond bExpr stmt' -> not (isFalse bExpr) && mightBeReturning stmt'
    CondElse bExpr stmt1 stmt2 ->
      (not (isFalse bExpr) && mightBeReturning stmt1) ||
      (not (isTrue bExpr) && mightBeReturning stmt2)
    _ -> False

isReturning :: Stmt -> Bool
isReturning stmt = case stmt of
  BStmt (Block s) -> any isReturning s
  Ret _ -> True
  Cond bExpr stmt' -> isTrue bExpr && isReturning stmt'
  CondElse bExpr stmt1 stmt2 ->
    (isFalse bExpr || isReturning stmt1) && (isTrue bExpr || isReturning stmt2)
  While bExpr stmt' -> isTrue bExpr && mightBeReturning stmt'
  _ -> False

standardFunctions :: TypedFnDefs
standardFunctions =
  Map.fromList [
    (Ident "printInt", Fun Void [Int]),
    (Ident "printString", Fun Void [Str]),
    (Ident "error", Fun Void []),
    (Ident "readInt", Fun Int []),
    (Ident "readString", Fun Str [])
  ]

typecheck :: Program -> IO ()
typecheck (Program fns) = do
  typedFns <- foldM addTypedFnDef standardFunctions fns
  assertCorrectMain typedFns
  forM_ fns (`typecheckFunction` typedFns)
