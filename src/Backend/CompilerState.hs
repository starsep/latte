module CompilerState where

import AsmStmt
import Control.Monad.RWS (RWS, put, get)

type CEnv = ()
type CState = String
type CMonad = RWS CEnv AsmStmts CState

putName :: String -> CMonad ()
putName = put

getName :: CMonad String
getName = do
  name <- get
  return name
