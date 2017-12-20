module Assembly where

import AsmStandard
import AsmStmt

emitHeader :: [AsmStmt]
emitHeader =
  [Global "main"] ++
  libcExterns ++
  dataSection
