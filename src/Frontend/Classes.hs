module Classes where

import AbsLatte
import Context
import Control.Monad
import Control.Monad.RWS
import qualified Data.Map as Map
import Data.Map ((!), Map)
import Data.Maybe
import Env
import qualified Errors
import Pure

type ClassNames = [Ident]

addInhTree :: InheritanceTree -> Ident -> Ident -> InheritanceTree
addInhTree inhTree ident extends =
  Map.insert ident (Just extends) inhTree

addClassDef :: ClassNames -> (ClassDefs, InheritanceTree) ->
  TopDef -> IO (ClassDefs, InheritanceTree)
addClassDef classNames acc@(classDefs, inhTree) topDef = case topDef of
  ClassDef ident props -> do
    when (Map.member ident classDefs) $
      Errors.multipleClass ident $ Context []
    return (Map.insert ident props classDefs, inhTree)
  ClassDefE ident extends props -> do
    unless (extends `elem` classNames) $
      Errors.extendsUnknownClass ident extends $ Context []
    let inhTree' = addInhTree inhTree ident extends
    addClassDef classNames (classDefs, inhTree') (ClassDef ident props)
  _ -> return acc

collectClassNames :: ClassNames -> TopDef -> ClassNames
collectClassNames names topDef = case topDef of
  ClassDef ident _ -> ident : names
  ClassDefE ident _ _ -> ident : names
  _ -> names

type CycleEnv = InheritanceTree
type CycleState = (Map Ident Bool, [Ident])
type CycleMonad = RWST CycleEnv () CycleState IO ()

runFindCycle :: ClassNames -> InheritanceTree -> IO ()
runFindCycle classNames inhTree =
  void $ runRWST (findCycle classNames) inhTree (Map.empty, [])

findCycle :: ClassNames -> CycleMonad
findCycle classNames = do
  put (Map.fromList $ zip classNames $ repeat False, [])
  forM_ classNames $ \className -> do
    (visited, _) <- get
    put (visited, [])
    unless (visited ! className) $
      findCycleDfs className

findCycleDfs :: Ident -> CycleMonad
findCycleDfs ident = do
  (visited, stack) <- get
  when (ident `elem` stack) $
    lift $ Errors.classCycle ident $ Context []
  put (Map.insert ident True visited, ident : stack)
  graph <- ask
  let parent = graph ! ident
  when (isJust parent) $
    findCycleDfs $ fromJust parent

checkClass :: Ident -> TopDefScope -> IO ()
checkClass name (typed, classDefs, inhTree) = do
  let initEnv = (typed, name, classDefs, inhTree)
      initState = (Map.empty, [], Context [CClass name])
  void $ runRWST checkClassM initEnv initState

checkClassM :: TCMonad ()
checkClassM = do
  name <- askIdent
  props <- askProps
  let propNames = map propName props
      duplicate = firstNotUnique propNames
  when (isJust duplicate) $
    showError $ Errors.multipleProps (fromJust duplicate)
  forM_ props checkProp

checkField :: Type -> Ident -> TCMonad ()
checkField t name = do
  when (t == Void) $
    showError $ Errors.voidVariable name
  className <- askIdent
  checkFieldShadow name className

checkFieldShadow :: Ident -> Ident -> TCMonad ()
checkFieldShadow field c = do
  name <- askIdent
  parent <- classParent c
  classDefs <- askClassDefs
  when (name /= c) $ do
    let fieldsConfs = filter (isFieldConflict field) (classDefs ! c)
    unless (null fieldsConfs) $
      showError $ Errors.fieldConflict field c
  when (isJust parent) $ do
    let to = fromJust parent
    checkFieldShadow field to

isFieldConflict :: Ident -> ClassProp -> Bool
isFieldConflict f (Field _ f2) = f == f2
isFieldConflict f _ = False

checkMethod :: Type -> Ident -> [Arg] -> Block -> TCMonad ()
checkMethod out name args body = return ()

checkProp :: ClassProp -> TCMonad ()
checkProp (Method out name args body) = checkMethod out name args body
checkProp (Field t name) = checkField t name

classParent :: Ident -> TCMonad (Maybe Ident)
classParent className = do
  inhTree <- askInheritanceTree
  return $ inhTree ! className
