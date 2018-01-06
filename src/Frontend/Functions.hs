module Functions where

import AbsLatte
import {-# SOURCE #-} Classes
import Context
import Control.Monad
import Control.Monad.RWS (runRWST)
import qualified Data.Map as Map
import Data.Maybe
import Env
import Errors
import Pure
import {-# SOURCE #-} Typecheck

addTypedFnDef :: ClassesData -> TypedFnDefs -> TopDef -> IO TypedFnDefs
addTypedFnDef classesData typed topDef = case topDef of
  FnDef outType ident args _ -> do
    when (Map.member ident typed) $
      Errors.multipleFnDef ident $ Context []
    return $ Map.insert ident (fnHeaderToFnType outType args) typed
  ClassDef name _ -> return $ addTypedFnDefClass classesData name typed
  ClassDefE name _ _ -> return $ addTypedFnDefClass classesData name typed

funContext :: (Type, Ident, [Arg]) -> Maybe Ident -> Context
funContext (outType, ident, args) className' = case className' of
  Nothing -> Context [CFun outType ident args]
  Just className -> Context [CMethod outType ident args className]

typecheckFun :: (Type, Ident, [Arg], Block) -> TopDefScope
  -> Maybe Ident -> TCIdentState -> IO ()
typecheckFun (outType, ident, args, body) scope className initIdent = do
  let context = funContext (outType, ident, args) className
      fun = FnDef outType ident args body
  funState <- foldM (addFunctionArgToState context) initIdent args
  let funState' = if isJust className then Map.insert (Ident "self") (ClassType $ fromJust className) funState else funState
      initState = (funState', [], context)
      initEnv = (ident, scope, className)
  void $ runRWST (typecheckBlock body) initEnv initState
  when ((outType /= Void) && not (isReturning (BStmt body))) $
    Errors.notReturning ident context

addFunctionArgToState :: Context -> TCIdentState -> Arg -> IO TCIdentState
addFunctionArgToState context state (Arg t argIdent) = do
  when (t == Void) $
    Errors.voidArgument argIdent context
  when (Map.member argIdent state) $
    Errors.sameArgNames argIdent context
  return $ Map.insert argIdent t state
