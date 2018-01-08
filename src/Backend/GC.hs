module GC where

import AbsLatte
import Asm
-- import Control.Monad
import Control.Monad.RWS (tell)
-- import qualified Data.Map as Map
import State

isGCType :: Type -> Bool
isGCType t = case t of
  Str -> True
  Array _ -> True
  _ -> False

gcScopeVars :: CMonad ()
gcScopeVars = do
  --(vars, _) <- getVars
  return ()
  --forM_ (Map.toList $ head vars) $ \(name, (addr, t)) ->
    --when (isGCType t) $ do
      -- TODO: GC
      --tell [Push $ show addr]
      --gcDecr

callGcFun :: String -> CMonad ()
callGcFun name = do
  let r = head argRegisters
  localReserveReg r $
    tell [
      Pop $ head argRegisters,
      Call name]

gcDecr :: CMonad ()
gcDecr = callGcFun "_gcDecr"

gcIncr :: CMonad ()
gcIncr = callGcFun "_gcIncr"
