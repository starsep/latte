module CompilerState where

import AsmStmt
import Control.Monad.RWS (RWS, put, get)

type CEnv = ()
type CState = (String, Int)
type CMonad = RWS CEnv AsmStmts CState

putName :: String -> CMonad ()
putName name = do
  (_, labelId) <- get
  put (name, labelId)

getName :: CMonad String
getName = do
  (name, _) <- get
  return name

nextLabelId :: CMonad Int
nextLabelId = do
  (name, res) <- get
  put (name, res + 1)
  return res 
