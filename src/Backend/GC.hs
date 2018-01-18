module GC where

import AbsLatte
import Asm
import Control.Monad.RWS (tell)
import State

isGCType :: Type -> Bool
isGCType t = case t of
  Str -> True
  _ -> False

callGcFun :: String -> CMonad ()
callGcFun name = do
  let r = head argRegisters
  localReserveReg r $
    tell [
      Pop r,
      Call name]

gcDecr :: CMonad ()
gcDecr = callGcFun "_gcDecr"

gcIncr :: CMonad ()
gcIncr = callGcFun "_gcIncr"
