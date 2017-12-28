module CompilerState where

import Asm
import Control.Monad
import Control.Monad.RWS (RWS, ask, put, get, listen, pass)
import Data.List
import Data.Map ((!))
import Debug.Trace
import Locals
import Typechecker (TypecheckerOutput)
import TypecheckerState (TypedFnDefs)

type CEnv = (TypecheckerOutput, Locals)
type FName = String
type LabelId = Int
type StringLits = [String]
type CState = (FName, LabelId, StringLits, Registers)
type CMonad = RWS CEnv AsmStmts CState

putName :: String -> CMonad ()
putName name = do
  (_, labelId, strings, regs) <- get
  put (name, labelId, strings, regs)

getName :: CMonad FName
getName = do
  (name, _, _, _) <- get
  return name

nextLabelId :: CMonad LabelId
nextLabelId = do
  (name, res, strings, regs) <- get
  put (name, res + 1, strings, regs)
  return res

getReservedRegs :: CMonad Registers
getReservedRegs = do
  (_, _, _, regs) <- get
  return regs

putReservedRegs :: Registers -> CMonad ()
putReservedRegs regs = do
  (name, res, strings, _) <- get
  put (name, res, strings, regs)

stringLiteralLabel :: String -> CMonad String
stringLiteralLabel s = do
  (name, labelId, strings, regs) <- get
  let len = length strings
      index = case elemIndex s strings of
        Just x -> len - x - 1
        Nothing -> len
      strings' = if index == len then s : strings else strings
  put (name, labelId, strings', regs)
  return $ stringLiteralFromId index

getStrings :: CMonad StringLits
getStrings = do
  (_, _, strings, _) <- get
  return strings

askTypedFns :: CMonad TypedFnDefs
askTypedFns = do
  (typed, _) <- ask
  return typed

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
  when (reg `elem` reservedRegs) $ do
    let v = trace (show reg ++ " already reserved, WTF?!") ()
    return v
  putReservedRegs $ reg : reservedRegs

reserveRegs :: Int -> CMonad Registers
reserveRegs n = do
  reservedRegs <- getReservedRegs
  let available = take n $ scratchRegisters \\ reservedRegs
  when (length available < n) $ do
    let errorMsg = "WTF?!: couldn't reserve " ++ show n ++ " registers"
        v = trace errorMsg ()
    return v
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
localReserveReg reg action = do
  reserveReg reg
  res <- action
  freeRegs [reg]
  return res
