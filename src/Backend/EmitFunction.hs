module EmitFunction where

import AbsLatte
import Asm
import Control.Monad
import Control.Monad.RWS (tell)
import EmitExpr
import EmitStmt
import State

emitFunction :: Ident -> [Arg] -> Block -> CMonad ()
emitFunction (Ident name) args block = do
  emptyVars
  putName name
  funHeader
  emitArgs args
  emitBlock block
  funFooter

argDecl :: Arg -> Stmt
argDecl (Arg t ident) = Decl t [NoInit ident]

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
