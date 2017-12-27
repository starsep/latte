module Typechecker (typecheck, typeOf, TypecheckerOutput()) where

import AbsLatte
import Control.Monad
import Control.Monad.RWS (get, put, runRWST)
import qualified Data.Map as Map
import Data.Map ((!))
import Data.Maybe
import qualified Errors
import PrintLatte (printTree)
import Data.Int
import Context
import TypecheckerAssert
import TypecheckerPure
import TypecheckerState

type TypecheckerOutput = TypedFnDefs

typeOf :: Expr -> TCMonad Type
typeOf q =
  case q of
    EApp ident args -> outputType ident args
    EVar ident -> typeOfIdent ident
    ELitInt x -> do
      let minInt = toInteger (minBound :: Int32)
          maxInt = toInteger (maxBound :: Int32)
      when (x < minInt || x > maxInt) $
        showError $ Errors.int32 x
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
    EIndex index' ->
      case index' of
        Index expr index -> do
          assertNumericExpr index
          typeOfArray expr
        IndexR index e -> do
          assertNumericExpr e
          let expr = EIndex index
          typeOfArray expr
    EArray t e -> do
      assertArrayableType t
      assertNumericExpr e
      return $ Array t
    EField object name -> do
      t <- typeOf object
      case t of
        Array _ -> do
          when (name /= Ident "length") $
            showError $ Errors.arrayUnknownField name
          return Int
        ClassType className -> do
          field <- findProp className name
          case field of
            Field t' _ -> return t'
            _ -> showErrorV $ Errors.methodAsField className name
        _ -> showErrorV $ Errors.simpleTypeField object t name
    ENull t -> do
      unless (isClass t) $
        showError $ Errors.nullOfType t
      return t
    EClass t -> return t
    EMethod object name args -> do
      t <- typeOf object
      case t of
        ClassType className -> do
          method <- findProp className name
          case method of
            Method t' _ mArgs _ -> do
              checkArgs name args $ argsToTypes mArgs
              return t'
            _ -> showErrorV $ Errors.fieldAsMethod className name
        _ -> showErrorV $ Errors.methodCallOn object t name args

findProp :: Ident -> Ident -> TCMonad ClassProp
findProp className name = do
  classDefs <- askClassDefs
  let classDef = classDefs ! className
      props = filter (\p -> propName p == name) classDef
  when (null props) $
    showError $ Errors.unknownProperty className name
  return $ head props

typeOfArray :: Expr -> TCMonad Type
typeOfArray expr = do
  t <- typeOf expr
  case t of
    Array t' -> return t'
    _ -> showErrorV $ Errors.notArray expr t

typeOfIdent :: Ident -> TCMonad Type
typeOfIdent ident = do
  state <- getState
  assertVarDeclared ident
  return $ state ! ident

checkBExprOp :: Expr -> Expr -> TCMonad Type
checkBExprOp b1 b2 = do
  assertBExpr b1
  assertBExpr b2
  return Bool

checkBinOp :: Expr -> Expr -> String -> TCMonad Type
checkBinOp expr1 expr2 name = do
  t1 <- typeOf expr1
  t2 <- typeOf expr2
  unless (t1 == t2) $ showError $ Errors.diffTypesBinOp name t1 t2
  return t1

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
  when (nArgs /= expected) $
    showError $ Errors.numberOfArgs ident nArgs expected
  argsTypes <- mapM typeOf args
  when (argsTypes /= types) $
    showError $ Errors.typesOfArgs ident argsTypes types

outputTypeFun :: Ident -> [Expr] -> TCMonad Type
outputTypeFun ident args = do
  typed <- askTyped
  let (Fun out argsTypes) = typed ! ident
  checkArgs ident args argsTypes
  return out

outputType :: Ident -> [Expr] -> TCMonad Type
outputType ident args = do
  typed <- askTyped
  if Map.member ident typed then
    outputTypeFun ident args
  else
    showErrorV $ Errors.functionUndeclared ident

checkShadow :: Ident -> TCMonad ()
checkShadow ident = do
  typed <- askTyped
  when (Map.member ident typed) $
    showError $ Errors.shadowTopDef ident

typecheckDecl :: Item -> Type -> TCMonad ()
typecheckDecl item t = do
  when (t == Void) $
    showError $ Errors.voidVariable $ itemIdent item
  let ident = itemIdent item
  state <- getState
  addDecl ident
  checkShadow ident
  void $ putState (Map.insert (itemIdent item) t state)
  case item of
    NoInit _ ->
      case t of
        Array t' -> assertArrayableType t'
        _ -> return ()
    Init _ expr -> typecheckAss (EVar ident) expr

typecheckIncr :: Ident -> TCMonad ()
typecheckIncr ident = do
  assertVarDeclared ident
  assertType (EVar ident) Int

typecheckAss :: Expr -> Expr -> TCMonad ()
typecheckAss lvalue expr = do
  ltype <- typeOf lvalue
  rtype <- typeOf expr
  case lvalue of
    EVar _ -> assertType expr ltype
    EIndex _ ->
      when (ltype /= rtype) $
        showError $ Errors.notMatchingTypeIndex ltype rtype
    EField object name -> do
      t <- typeOf object
      case t of
        Array _ -> when (name == Ident "length") $
          showError Errors.lengthReadOnly
        _ -> return ()
    _ -> showError $ Errors.badLvalue lvalue ltype

checkRelOp :: Expr -> Expr -> String -> TCMonad ()
checkRelOp expr1 expr2 name = do
  assertComparable expr1
  void $ checkBinOp expr1 expr2 name

typecheckStmtCase :: Stmt -> TCMonad ()
typecheckStmtCase stmt = do
  returnType <- askReturn
  case stmt of
    Empty -> return ()
    BStmt block -> typecheckBlock block
    Decl t items -> forM_ items (`typecheckDecl` t)
    Ass lvalue expr -> typecheckAss lvalue expr
    Incr ident -> typecheckIncr ident
    Decr ident -> typecheckIncr ident
    Ret expr -> do
      t <- typeOf expr
      when (returnType == Void) $
        showError $ Errors.retVoid expr t
      when (returnType /= t) $
        showError $ Errors.badRetType expr t returnType
    VRet -> when (returnType /= Void) $
      showError $ Errors.vRetNoVoid returnType
    While bExpr stmt' -> do
      assertBExpr bExpr
      addContext $ CWhile bExpr
      typecheckStmt stmt'
      dropContext
    SExp expr -> void $ typeOf expr
    Cond expr stmt' -> do
      assertBExpr expr
      addContext $ CIf expr
      typecheckStmt stmt'
      dropContext
    CondElse expr stmt1 stmt2 -> do
      assertBExpr expr
      addContext $ CIf expr
      typecheckStmt stmt1
      dropContext
      addContext CElse
      typecheckStmt stmt2
      dropContext
    For t ident array stmt' -> do
      addContext $ CFor t ident array
      arrayType <- typeOf array
      when (arrayType /= Array t) $
        showError $ Errors.badArrayType array arrayType t
      typecheckBlock $ Block [Decl t [NoInit ident], stmt']
      dropContext

typecheckStmt :: Stmt -> TCMonad ()
typecheckStmt stmt = do
  addContextStmt $ CStmt stmt
  typecheckStmtCase stmt
  dropContext

typecheckBlock :: Block -> TCMonad ()
typecheckBlock (Block stmts) = do
  (state, decl, context) <- get
  put (state, [], context)
  forM_ stmts typecheckStmt
  put (state, decl, context)

addFunctionArgToState :: Context -> TCIdentState -> Arg -> IO TCIdentState
addFunctionArgToState context state (Arg t argIdent) = do
  when (t == Void) $
    Errors.voidArgument argIdent context
  when (Map.member argIdent state) $
    Errors.sameArgNames argIdent context
  return $ Map.insert argIdent t state

type TopDefScope = (TypedFnDefs, ClassDefs, InheritanceTree)

typecheckField :: Type -> Ident -> Context -> IO ()
typecheckField t name context =
  when (t == Void) $
    Errors.voidVariable name context

typecheckMethod :: Type -> Ident -> [Arg] -> Block -> IO ()
typecheckMethod out name args body = return ()

typecheckProp :: Context -> ClassProp -> IO ()
typecheckProp _ (Method out name args body) = typecheckMethod out name args body
typecheckProp context (Field t name) = typecheckField t name context

typecheckClass :: Ident -> [ClassProp] -> TopDefScope -> IO ()
typecheckClass name props scope = do
  let context = Context [CClass name]
      propNames = map propName props
      duplicate = firstNotUnique propNames
  when (isJust duplicate) $
    Errors.multipleProps (fromJust duplicate) context
  forM_ props (typecheckProp context)

typecheckFun :: (Type, Ident, [Arg], Block) -> TopDefScope -> IO ()
typecheckFun (outType, i, args, body) (typed, classDefs, inhTree) = do
  let fun = FnDef outType i args body
      context = Context [CFun outType i args]
  funState <- foldM (addFunctionArgToState context) Map.empty args
  let initState = (funState, [], context)
      initEnv = (typed, outType, classDefs, inhTree)
  void $ runRWST (typecheckBlock body) initEnv initState
  when ((outType /= Void) && not (isReturning (BStmt body))) $
    Errors.notReturning i context

typecheckTopDef :: TopDef -> TopDefScope -> IO ()
typecheckTopDef topDef scope = case topDef of
  FnDef outType i args body -> typecheckFun (outType, i, args, body) scope
  ClassDef ident props -> typecheckClass ident props scope
  ClassDefE ident _ props -> typecheckClass ident props scope

addTypedFnDef :: TypedFnDefs -> TopDef -> IO TypedFnDefs
addTypedFnDef typed (FnDef outType ident args _) = do
  when (Map.member ident typed) $
    Errors.multipleFnDef ident $ Context []
  return $ Map.insert ident (fnHeaderToFnType outType args) typed
addTypedFnDef typed _ = return typed

addClassDef :: (ClassDefs, InheritanceTree) ->
  TopDef -> IO (ClassDefs, InheritanceTree)
addClassDef (classDefs, inhTree) (ClassDef ident props) = do
  when (Map.member ident classDefs) $
    Errors.multipleClass ident $ Context []
  return (Map.insert ident props classDefs, inhTree)
addClassDef (classDefs, inhTree) (ClassDefE ident extends props) =
  addClassDef (classDefs, (ident, extends) : inhTree) (ClassDef ident props)
addClassDef acc _ = return acc

typecheck :: Program -> IO TypecheckerOutput
typecheck (Program topDefs) = do
  typedFns <- foldM addTypedFnDef standardFunctions topDefs
  (classDefs, inhTree) <- foldM addClassDef (Map.empty, []) topDefs
  assertCorrectMain typedFns
  forM_ topDefs (`typecheckTopDef` (typedFns, classDefs, inhTree))
  return typedFns
