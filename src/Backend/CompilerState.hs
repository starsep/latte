module CompilerState where

import Asm
import AsmStmt
import Control.Monad.RWS (RWS, put, get)
import Data.List
import Data.Maybe

type CEnv = ()
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
  let s' = parseStringLiteral s
  (name, labelId, strings) <- get
  let len = length strings
      index = fromMaybe len $ elemIndex s' strings
  put (name, labelId, if len == index then s' : strings else strings)
  return $ stringLiteralFromId index

getStrings :: CMonad [String]
getStrings = do
  (_, _, strings) <- get
  return strings
