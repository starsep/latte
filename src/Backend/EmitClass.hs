module EmitClass where

import AbsLatte
import Asm
import Control.Monad
import Control.Monad.RWS
import Data.List
import qualified Data.Map as Map
import Env
import Label
import State

emitMethod :: Ident -> Ident -> [Arg] -> Block -> CMonad ()
emitMethod className name args block = do
  let label = classMethodLabel className name
  tell [
    Label label,
    Return]
  -- error "emitMethod isn't implemented"

emitVTables :: CMonad ()
emitVTables = do
  classesData <- askClassesData
  tell [SectionData]
  forM_ (Map.toList classesData) $ \(className, (methods, _)) ->
    emitVTable className methods

emitVTable :: Ident -> [ClassMethod] -> CMonad ()
emitVTable className methods = do
  let rmethods = reverse methods
  let label = vtableLabel className
      methodsLabels = map vtableString rmethods
      content = intercalate ", " methodsLabels
  unless (null content) $
    tell [DataDecl label DataQword content]

vtableString :: ClassMethod -> String
vtableString (ClassMethod _ method className) =
  classMethodLabel className method
