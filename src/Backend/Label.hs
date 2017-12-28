module Label where

import Asm
import CompilerState
import Control.Monad.RWS (tell)

endLabel :: CMonad String
endLabel = do
  fname <- getName
  return $ fname ++ "$end"

jumpEndLabel :: CMonad ()
jumpEndLabel = do
  label <- endLabel
  tell [Jmp label]

twoLabels :: CMonad (String, Int, Int)
twoLabels = do
  fname <- getName
  labelId <- nextLabelId
  labelId' <- nextLabelId
  return (fname, labelId, labelId')

orLabels :: CMonad (String, String)
orLabels = do
  (fname, labelId, labelId') <- twoLabels
  return (fname ++ "$orPush1#" ++ show labelId,
    fname ++ "$orAfter#" ++ show labelId')

andLabels :: CMonad (String, String)
andLabels = do
  (fname, labelId, labelId') <- twoLabels
  return (fname ++ "$andPush0#" ++ show labelId,
    fname ++ "$andAfter#" ++ show labelId')

ifLabel :: CMonad String
ifLabel = do
  fname <- getName
  labelId <- nextLabelId
  return (fname ++ "$ifAfter#" ++ show labelId)

ifElseLabels :: CMonad (String, String)
ifElseLabels = do
  (fname, labelId, labelId') <- twoLabels
  return (fname ++ "$ifElse#" ++ show labelId,
    fname ++ "$ifElseAfter#" ++ show labelId')

whileLabels :: CMonad (String, String)
whileLabels = do
  (fname, labelId, labelId') <- twoLabels
  return (fname ++ "$whileBegin#" ++ show labelId,
    fname ++ "$whileAfter#" ++ show labelId')
