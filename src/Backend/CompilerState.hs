module CompilerState where

import AsmStmt
import Control.Monad.RWS (RWS)

type CEnv = ()
type CState = ()
type CMonad = RWS CEnv AsmStmts CState
