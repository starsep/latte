module Classes where

import AbsLatte
import Context
import Control.Monad
import Control.Monad.RWS
import Data.List
import qualified Data.Map as Map
import Data.Map ((!), Map)
import Data.Maybe
import Env
import qualified Errors
import Pure

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
checkClass name (typed, classDefs, inhTree, classNames) = do
  let initEnv = (typed, name, classDefs, inhTree, classNames)
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
isFieldConflict f prop = f == propName prop

checkMethod :: Type -> Ident -> [Arg] -> Block -> TCMonad ()
checkMethod out name args body = do
  className <- askIdent
  checkVirtualMethod className (out, name, args)

checkVirtualMethod :: Ident -> (Type, Ident, [Arg]) -> TCMonad ()
checkVirtualMethod className method = do
  parent <- classParent className
  classDefs <- askClassDefs
  name <- askIdent
  when (name /= className) $ do
    let methodConfs = filter (isMethodConflict method) (classDefs ! className)
    unless (null methodConfs) $ case head methodConfs of
      Method out' name' args' _ ->
        showError $ Errors.methodConflict method className (out', name', args')
      Field t name' ->
        showError $ Errors.methodFieldConflict method className (t, name')
  when (isJust parent) $ do
    let to = fromJust parent
    checkVirtualMethod to method

isMethodConflict :: (Type, Ident, [Arg]) -> ClassProp -> Bool
isMethodConflict (out, name, args) (Method out' name' args' _) =
  let typeOfArg (Arg t _) = t
      types = map typeOfArg in
  name == name' && (out /= out' || types args /= types args')
isMethodConflict (_, name, _) (Field _ fieldName) = name == fieldName

checkProp :: ClassProp -> TCMonad ()
checkProp (Method out name args body) = checkMethod out name args body
checkProp (Field t name) = checkField t name

classParent :: Ident -> TCMonad (Maybe Ident)
classParent className = do
  inhTree <- askInheritanceTree
  return $ inhTree ! className

isCompatibleType :: Type -> Type -> TCMonad Bool
isCompatibleType (Array visible) (Array actual) =
  isCompatibleType visible actual
isCompatibleType t@(ClassType visible) (ClassType actual) = do
  parent <- classParent actual
  let equal = visible == actual
      subType = isJust parent
  if subType then do
    compRec <- isCompatibleType t (ClassType $ fromJust parent)
    return $ equal || compRec
  else
    return equal
isCompatibleType visible actual = return $ visible == actual

findProp' :: Ident -> Ident -> Ident -> TCMonad ClassProp
findProp' topClass className name = do
  classDefs <- askClassDefs
  let classDef = classDefs ! className
      props = filter (\p -> propName p == name) classDef
  if not . null $ props then
    return $ head props
  else do
    parent <- classParent className
    when (isNothing parent) $
      showError $ Errors.unknownProperty topClass name
    findProp' topClass (fromJust parent) name

findProp :: Ident -> Ident -> TCMonad ClassProp
findProp className = findProp' className className

type CDEnv = (ClassDefs, InheritanceTree, ClassNames)
type CDState = ClassesData
type CDMonad = RWS CDEnv () CDState

runBuildClassesData :: CDEnv -> IO ClassesData
runBuildClassesData initEnv = do
  let (_, classesData, _) = runRWS buildClassesData initEnv Map.empty
  return classesData

buildClassesData :: CDMonad ()
buildClassesData = do
  classNames <- cdAskClassNames
  forM_ classNames
    buildClassData

getParentClassData :: Ident -> CDMonad ClassData
getParentClassData className = do
  parent <- cdParent className
  case parent of
    Just par -> do
      buildClassData par
      getClassData par
    Nothing -> return ([], [])

buildClassData :: Ident -> CDMonad ()
buildClassData className = do
  classesData <- getClassesData
  unless (Map.member className classesData) $
    buildClassDataImpl className

isMethod :: ClassProp -> Bool
isMethod prop = case prop of
  Method{} -> True
  Field{} -> False

buildClassDataImpl :: Ident -> CDMonad ()
buildClassDataImpl className = do
  (parentMethods, parentFields) <- getParentClassData className
  classDefs <- cdAskClassDefs
  let classDef = classDefs ! className
      (methods, fields) = partition isMethod classDef
      newMethods = foldl (addMethod className) parentMethods methods
      newFields = foldl addField parentFields fields
  classesData <- getClassesData
  put $ Map.insert className (newMethods, newFields) classesData

replaceMethod :: Ident -> Ident -> (Bool, [ClassMethod]) ->
  ClassMethod -> (Bool, [ClassMethod])
replaceMethod className name (r, methods) m@(ClassMethod i name' className') =
  if name == name' then
    (True, ClassMethod i name className : methods)
  else
    (r, m : methods)

addMethod :: Ident -> [ClassMethod] -> ClassProp -> [ClassMethod]
addMethod className [] (Method _ name _ _) = [ClassMethod 0 name className]
addMethod className methods@(ClassMethod i _ _ : _) (Method _ name _ _) =
  let replaceFun = replaceMethod className name
      (replaced, methods') = foldl replaceFun (False, []) methods in
  if replaced then
    reverse methods'
  else
    ClassMethod (i + 1) name className : methods
addMethod _ methods Field{} = methods

addField :: [ClassField] -> ClassProp -> [ClassField]
addField [] (Field _ name) = [ClassField 0 name]
addField fields@(ClassField i _ : _) (Field _ name) =
  ClassField (i + 1) name : fields
addField fields Method{} = fields

cdAskClassDefs :: CDMonad ClassDefs
cdAskClassDefs = do
  (classDefs, _, _) <- ask
  return classDefs

cdAskInheritanceTree :: CDMonad InheritanceTree
cdAskInheritanceTree = do
  (_, inhTree, _) <- ask
  return inhTree

cdAskClassNames :: CDMonad ClassNames
cdAskClassNames = do
  (_, _, classNames) <- ask
  return classNames

getClassesData :: CDMonad ClassesData
getClassesData = get

getClassData :: Ident -> CDMonad ClassData
getClassData className = do
  classesData <- getClassesData
  return $ classesData ! className

cdParent :: Ident -> CDMonad (Maybe Ident)
cdParent className = do
    inhTree <- cdAskInheritanceTree
    return $ inhTree ! className
