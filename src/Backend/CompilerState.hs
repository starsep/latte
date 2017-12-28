module CompilerState where

import Asm
import Control.Monad
import Control.Monad.RWS (RWS, ask, put, get, tell, listen, pass)
import Data.List
import Typechecker (TypecheckerOutput)
import TypecheckerState (TypedFnDefs)

type CEnv = TypecheckerOutput
type CState = (String, Int, [String])
type CMonad = RWS CEnv AsmStmts CState

putName :: String -> CMonad ()
putName name = do
  (_, labelId, strings) <- get
  put (name, labelId, strings)

getName :: CMonad String
getName = do
  (name, _, _) <- get
  return name

nextLabelId :: CMonad Int
nextLabelId = do
  (name, res, strings) <- get
  put (name, res + 1, strings)
  return res

stringLiteralLabel :: String -> CMonad String
stringLiteralLabel s = do
  (name, labelId, strings) <- get
  let len = length strings
      index = case elemIndex s strings of
        Just x -> len - x - 1
        Nothing -> len
  put (name, labelId, if index == len then s : strings else strings)
  return $ stringLiteralFromId index

getStrings :: CMonad [String]
getStrings = do
  (_, _, strings) <- get
  return strings

askTypedFns :: CMonad TypedFnDefs
askTypedFns = ask

localRWS :: CMonad a -> CMonad a
localRWS action = do
  state <- get
  r <- pass $ do
    (r, _) <- listen action
    return (r, const [])
  put state
  return r
