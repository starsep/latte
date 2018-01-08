module GC where

import AbsLatte
import Asm
import Control.Monad.RWS (tell)
import State

isGCType :: Type -> Bool
isGCType t = case t of
  Str -> True
  Array _ -> True
  ClassType _ -> True
  _ -> False

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
