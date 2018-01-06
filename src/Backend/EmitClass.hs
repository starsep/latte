module EmitClass where

import AbsLatte
import Asm
import Control.Monad
import Control.Monad.RWS
import Data.Map ((!))
import qualified Data.Map as Map
import Env
import EmitExpr
import EmitFunction
import Label
import State

emitMethod :: Ident -> Ident -> [Arg] -> Block -> CMonad ()
emitMethod className name args block =
  let method = classMethodIdent className name
      args' = (Arg (ClassType className) (Ident "self") : args) in
  emitFunction method args' block

emitVTables :: CMonad ()
emitVTables = do
  classesData <- askClassesData
  tell [SectionData]
  forM_ (Map.toList classesData) $ \(className, (methods, _)) ->
    emitVTable className methods

emitVTable :: Ident -> [ClassMethod] -> CMonad ()
emitVTable className methods = do
  let rmethods = reverse methods
      label = vtableLabel className
      methodsLabels = map vtableString rmethods
      content = if null methodsLabels then ["0"] else methodsLabels
  tell [DataDecl label DataQword content]

vtableString :: ClassMethod -> String
vtableString (ClassMethod _ method className _) =
  classMethodLabel className method

emitNewClass :: Ident -> CMonad Type
emitNewClass className = do
  fields <- getClassFields className
  let size = toInteger $ length fields
      [r1, r2] = take 2 argRegisters
      vtable = vtableLabel className
  tell [
    Mov r1 $ show size,
    Mov r2 vtable,
    Call "_newClass",
    Push resultReg]
  return $ ClassType className

getClassFields :: Ident -> CMonad [ClassField]
getClassFields className = do
  classesData <- askClassesData
  let (_, fields) = classesData ! className
  return fields

emitField :: Expr -> Ident -> CMonad Type
emitField obj ident = do
  t <- emitLField obj ident
  dereferenceTop
  return t

emitMethodInvoke :: Expr -> Ident -> [Expr] -> CMonad Type
emitMethodInvoke lv ident args = do
  (ClassType className) <- emitExpr lv
  let methodName = classMethodIdent className ident
  reg <- emitMethodVTable className ident
  emitEApp' methodName (lv : args) (Just reg)

emitMethodVTable :: ClassName -> Ident -> CMonad Register
emitMethodVTable className method = do
  let regs@[res, r1, r2] = last scratchRegisters : take 2 argRegisters
  classesData <- askClassesData
  let (methods, _) = classesData ! className
      methods' = filter (\(ClassMethod _ name _ _) -> name == method) methods
      (ClassMethod methodNumber _ _ _) = head methods'
  localReserveRegs regs $
    tell [
      Pop r1,
      Mov r2 $ show methodNumber,
      Call "_vtableAsk",
      Mov res resultReg]
  return res

findFieldIndex :: Ident -> Ident -> CMonad (Int, Type)
findFieldIndex className field = do
  fields <- getClassFields className
  return $ findFieldIndex' fields field

findFieldIndex' :: [ClassField] -> Ident -> (Int, Type)
findFieldIndex' [] name = error $ "couldn't find field " ++ show name
findFieldIndex' (ClassField i n t : fields) name
  | n == name = (i, t)
  | otherwise = findFieldIndex' fields name

emitLField :: Expr -> Ident -> CMonad Type
emitLField obj name = do
  t <- emitExpr obj
  let (ClassType className) = t
      [r1, r2] = take 2 argRegisters
  (fieldNum, fieldType) <- findFieldIndex className name
  tell [
    Pop r1,
    Mov r2 $ show fieldNum,
    Call "_classField",
    Push resultReg]
  return fieldType
