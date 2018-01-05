module State where

import AbsLatte
import Asm
import Control.Monad
import Control.Monad.RWS (RWS, ask, put, get, listen, pass)
import Data.List
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Env (TypecheckOutput, TypedFnDefs, ClassesData)
import Locals

type CEnv = (TypecheckOutput, Locals)
type FName = String
type LabelId = Int
type StringLits = [String]
type VarAddr = [Map String (Address, Type)]
type VarState = (VarAddr, Address)
type CState = (FName, LabelId, StringLits, Registers, VarState)
type CMonad = RWS CEnv AsmStmts CState

data LValue
  = LVar Ident
  | LSubs ESubs
  | LField Expr Ident

putName :: String -> CMonad ()
putName name = do
  (_, labelId, strings, regs, vars) <- get
  put (name, labelId, strings, regs, vars)

getName :: CMonad FName
getName = do
  (name, _, _, _, _) <- get
  return name

nextLabelId :: CMonad LabelId
nextLabelId = do
  (name, res, strings, regs, vars) <- get
  put (name, res + 1, strings, regs, vars)
  return res

getReservedRegs :: CMonad Registers
getReservedRegs = do
  (_, _, _, regs, _) <- get
  return regs

putReservedRegs :: Registers -> CMonad ()
putReservedRegs regs = do
  (name, res, strings, _, vars) <- get
  put (name, res, strings, regs, vars)

stringLiteralLabel :: String -> CMonad String
stringLiteralLabel s = do
  (name, labelId, strings, regs, vars) <- get
  let len = length strings
      index = case elemIndex s strings of
        Just x -> len - x - 1
        Nothing -> len
      strings' = if index == len then s : strings else strings
  put (name, labelId, strings', regs, vars)
  return $ stringLiteralFromId index

getStrings :: CMonad StringLits
getStrings = do
  (_, _, strings, _, _) <- get
  return strings

getVars :: CMonad VarState
getVars = do
  (_, _, _, _, vars) <- get
  return vars

putVars :: VarState -> CMonad ()
putVars vars = do
  (name, labelId, strings, regs, _) <- get
  put (name, labelId, strings, regs, vars)

initVars :: VarState
initVars = ([Map.empty], Address 8)

emptyVars :: CMonad ()
emptyVars = putVars initVars

askTypedFns :: CMonad TypedFnDefs
askTypedFns = do
  ((typed, _), _) <- ask
  return typed

askClassesData :: CMonad ClassesData
askClassesData = do
  ((_, classesData), _) <- ask
  return classesData

askLocals :: CMonad Locals
askLocals = do
  (_, locals) <- ask
  return locals

localsSize :: CMonad String
localsSize = do
  name <- getName
  locals <- askLocals
  return $ show $ 8 * locals ! name

localRWS :: CMonad a -> CMonad a
localRWS action = do
  state <- get
  r <- pass $ do
    (r, _) <- listen action
    return (r, const [])
  put state
  return r

reserveReg :: Register -> CMonad ()
reserveReg reg = do
  reservedRegs <- getReservedRegs
  when (reg `elem` reservedRegs) $
    error $ show reg ++ " already reserved, WTF?!"
  putReservedRegs $ reg : reservedRegs

reserveRegs :: Int -> CMonad Registers
reserveRegs n = do
  reservedRegs <- getReservedRegs
  let available = take n $ scratchRegisters \\ reservedRegs
  when (length available < n) $
    error $ "WTF?!: couldn't reserve " ++ show n ++ " registers"
  mapM_ reserveReg available
  return available

freeRegs :: Registers -> CMonad ()
freeRegs regs = do
  reservedRegs <- getReservedRegs
  let afterFree = reservedRegs \\ regs
  putReservedRegs afterFree

localReserve :: Int -> (Registers -> CMonad a) -> CMonad a
localReserve n action = do
   regs <- reserveRegs n
   res <- action regs
   freeRegs regs
   return res

localReserveReg :: Register -> CMonad a -> CMonad a
localReserveReg reg = localReserveRegs [reg]

localReserveRegs :: Registers -> CMonad a -> CMonad a
localReserveRegs regs action = do
  forM_ regs reserveReg
  res <- action
  freeRegs regs
  return res
