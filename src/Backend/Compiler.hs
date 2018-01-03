module Compiler (compiler) where

import AbsLatte
import Asm
import Control.Monad
import Control.Monad.RWS (runRWS, tell)
import Env (TypecheckOutput)
import EmitExpr
import EmitStmt
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
  forM_ topdefs emitTopDef
  literals <- getStrings
  unless (null literals) $ do
    tell [SectionData]
    emitStringLiterals (reverse literals) 0

emitTopDef :: TopDef -> CMonad ()
emitTopDef (FnDef _ (Ident name) args block) = do
  emptyVars
  putName name
  funHeader
  emitArgs args
  emitBlock block
  funFooter

emitTopDef _ = return () -- TODO: classes

argDecl :: Arg -> Stmt
argDecl (Arg t ident) = Decl t [NoInit ident]

emitArgs :: [Arg] -> CMonad ()
emitArgs args = do
  let argsDecl = map argDecl args
      argsRegNum = min 6 $ length args
      argsRegs = take argsRegNum argRegisters
      argsInRegs = take argsRegNum args
      argsWithRegs = zip argsRegs argsInRegs
      restArgs = drop argsRegNum args
      argsIndexes = map (*8) [2..] :: [Int]
  forM_ argsRegs reserveReg
  forM_ argsDecl emitStmt
  forM_ argsWithRegs $ \(reg, Arg _ ident) -> do
    localReserve 1 $ \[addr] -> do
      void $ emitLValue $ LVar ident
      tell [
        Pop addr,
        Mov ("[" ++ addr ++ "]") reg]
    freeRegs [reg]
  forM_ (zip restArgs argsIndexes) $ \(Arg _ ident, index) ->
    localReserve 2 $ \[addr, val] -> do
      void $ emitLValue $ LVar ident
      tell [
        Pop addr,
        Mov val $ "[" ++ basePointer ++ " + " ++ show index ++ "]",
        Mov ("[" ++ addr ++ "]") val]

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
    Push basePointer,
    Mov basePointer stackPointer,
    Sub stackPointer locSize]

funFooter :: CMonad ()
funFooter = do
  name <- getName
  locSize <- localsSize
  tell [Label $ name ++ "$end"]
  when (name == "main") $
    tell [Call "_gcClean"]
  tell [
    Add stackPointer locSize,
    Pop basePointer,
    Return]
