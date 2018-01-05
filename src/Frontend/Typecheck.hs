module Typecheck (typeOf, typecheckBlock) where

import AbsLatte
import Assert
import Classes
import Context
import Control.Monad
import Control.Monad.Loops
import Control.Monad.RWS (get, put)
import qualified Data.Map as Map
import Data.Int
import Data.Map ((!))
import Env
import qualified Errors
import PrintLatte (printTree)
import Pure

typeOf :: Expr -> TCMonad Type
typeOf q =
  case q of
    EApp ident args -> outputType ident args
    EVar ident -> typeOfIdent ident
    ELitInt x -> do
      let minInt = toInteger (minBound :: Int64)
          maxInt = toInteger (maxBound :: Int64)
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
    ESubs subs' ->
      case subs' of
        Subs expr index -> do
          assertNumericExpr index
          typeOfArray expr
        SubsR subs index -> do
          assertNumericExpr index
          let expr = ESubs subs
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
    EClass t -> do
      classNames <- askClassNames
      unless (t `elem` classNames) $
        showError $ Errors.newUnknownClass t
      return $ ClassType t
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
  compatible1 <- isCompatibleType t1 t2
  compatible2 <- isCompatibleType t2 t1
  let compatible = compatible1 || compatible2
  unless compatible $
    showError $ Errors.diffTypesBinOp name t1 t2
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
  let zipped = zip types argsTypes
  let compatibleZipped (t1, t2) = isCompatibleType t1 t2
  compatible <- allM compatibleZipped zipped
  unless compatible $
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
    ESubs _ -> do
      compatible <- isCompatibleType ltype rtype
      unless compatible $
        showError $ Errors.notMatchingTypeIndex ltype rtype
    EField object name -> do
      t <- typeOf object
      case t of
        Array _ -> when (name == Ident "length") $
          showError Errors.lengthReadOnly
        ClassType className -> do
          fieldT <- typeOf $ EField object name
          compatible <- isCompatibleType fieldT rtype
          unless compatible $
            showError $ Errors.badFieldType className name fieldT expr rtype
        _ -> showError $ Errors.badLvalue lvalue ltype
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
      compatibleTypes <- isCompatibleType returnType t
      unless compatibleTypes $
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
      compatible <- isCompatibleType (Array t) arrayType
      unless compatible $
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
