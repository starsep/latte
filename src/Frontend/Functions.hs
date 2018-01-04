module Functions where

import AbsLatte
import Context
import Control.Monad
import Control.Monad.RWS (runRWST)
import qualified Data.Map as Map
import Env
import Errors
import Pure
import Typecheck

addTypedFnDef :: TypedFnDefs -> TopDef -> IO TypedFnDefs
addTypedFnDef typed (FnDef outType ident args _) = do
  when (Map.member ident typed) $
    Errors.multipleFnDef ident $ Context []
  return $ Map.insert ident (fnHeaderToFnType outType args) typed
addTypedFnDef typed _ = return typed

typecheckFun :: (Type, Ident, [Arg], Block) -> TopDefScope -> IO ()
typecheckFun (outType, i, args, body) (typed, classDefs, inhTree, cNames) = do
  let fun = FnDef outType i args body
      context = Context [CFun outType i args]
  funState <- foldM (addFunctionArgToState context) Map.empty args
  let initState = (funState, [], context)
      initEnv = (typed, i, classDefs, inhTree, cNames)
  void $ runRWST (typecheckBlock body) initEnv initState
  when ((outType /= Void) && not (isReturning (BStmt body))) $
    Errors.notReturning i context

addFunctionArgToState :: Context -> TCIdentState -> Arg -> IO TCIdentState
addFunctionArgToState context state (Arg t argIdent) = do
  when (t == Void) $
    Errors.voidArgument argIdent context
  when (Map.member argIdent state) $
    Errors.sameArgNames argIdent context
  return $ Map.insert argIdent t state
