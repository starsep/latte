module Compiler (compiler) where

import AbsLatte
import Asm
import CompilerState
import Control.Monad
import Control.Monad.RWS (runRWS, tell)
import EmitStmt
import Locals
import Optimize
import Typechecker (TypecheckerOutput)
import TypecheckerPure (standardFunctionsNames)

compiler :: Bool -> String -> Program -> TypecheckerOutput -> String
compiler optimizeOn basename prog tOut =
  let locals = localsProg prog
      initEnv = (tOut, locals)
      initState = ("", 0, [])
      (_, _, output) = runRWS (compile prog) initEnv initState
      output' = if optimizeOn then optimize output else output in
  printAsm output'

compile :: Program -> CMonad ()
compile prog = do
  emitHeader
  emitProgram prog

emitProgram :: Program -> CMonad ()
emitProgram (Program topdefs) = do
  forM_ topdefs emitTopDef
  literals <- getStrings
  unless (null literals) $ do
    tell [SectionData]
    emitStringLiterals (reverse literals) 0

emitTopDef :: TopDef -> CMonad ()
emitTopDef (FnDef _ (Ident name) args block) = do
  putName name
  funHeader
  emitBlock block
  funFooter

emitTopDef _ = return ()

emitStringLiterals :: [String] -> Int -> CMonad ()
emitStringLiterals [] _ = return ()
emitStringLiterals (s:rest) i = do
  let label = stringLiteralFromId i
  tell [DataDecl label DataByte $ parseStringLiteral s]
  emitStringLiterals rest (i+1)

emitHeader :: CMonad ()
emitHeader = do
  tell [Global "main", EmptyLine]
  tell $ map Extern standardFunctionsNames
  tell [SectionText]

funHeader :: CMonad ()
funHeader = do
  name <- getName
  locSize <- localsSize
  tell [
    Label name,
    Push "rbp",
    Mov "rbp" "rsp",
    Sub "rsp" locSize]

funFooter :: CMonad ()
funFooter = do
  name <- getName
  locSize <- localsSize
  tell [
    Label $ name ++ "$end",
    Add "rsp" locSize,
    Pop "rbp",
    Return]
