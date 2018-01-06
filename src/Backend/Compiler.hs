module Compiler (compiler) where

import AbsLatte
import Asm
import Control.Monad
import Control.Monad.RWS (runRWS, tell)
import Env (TypecheckOutput)
import EmitClass
import EmitFunction
import Locals
import Optimize
import Pure (standardFunctionsNames)
import State

compiler :: Bool -> Program -> TypecheckOutput -> String
compiler optimizeOn prog tOut =
  let locals = localsProg prog
      initEnv = (tOut, locals)
      initState = ("", 0, [], [], initVars)
      (_, _, output) = runRWS (compile prog) initEnv initState
      output' = if optimizeOn then optimize output else output in
  printAsm output'

compile :: Program -> CMonad ()
compile prog = do
  emitHeader
  emitProgram prog

emitProgram :: Program -> CMonad ()
emitProgram (Program topdefs) = do
  emitVTables
  tell [SectionText]
  forM_ topdefs emitTopDef
  literals <- getStrings
  unless (null literals) $ do
    tell [SectionData]
    emitStringLiterals (reverse literals) 0

emitTopDef :: TopDef -> CMonad ()
emitTopDef (FnDef _ name args block) = emitFunction name args block
emitTopDef (ClassDef className props) =
  forM_ props $ \prop -> case prop of
    Field{} -> return ()
    Method _ name args block -> emitMethod className name args block
emitTopDef (ClassDefE name _ props) = emitTopDef (ClassDef name props)

emitStringLiterals :: [String] -> Int -> CMonad ()
emitStringLiterals [] _ = return ()
emitStringLiterals (s:rest) i = do
  let label = stringLiteralFromId i
  tell [DataDecl label DataByte [parseStringLiteral s]]
  emitStringLiterals rest (i+1)

emitHeader :: CMonad ()
emitHeader = do
  tell [Global "main", EmptyLine]
  tell $ map Extern standardFunctionsNames
