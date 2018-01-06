module Check (check) where

import AbsLatte
import Assert
import Control.Monad (foldM, forM_)
import qualified Data.Map as Map
import Classes
import Env
import Functions
import Pure

check :: Program -> IO TypecheckOutput
check (Program topDefs) = do
  let classNames = foldl collectClassNames [] topDefs
      initInhTree = Map.fromList $ zip classNames $ repeat Nothing
      initClassDefs = (Map.empty, initInhTree)
  (classDefs, inhTree) <- foldM (addClassDef classNames) initClassDefs topDefs
  runFindCycle classNames inhTree
  classesData <- runBuildClassesData (classDefs, inhTree, classNames)
  typedFns <- foldM (addTypedFnDef classesData) standardFunctions topDefs
  assertCorrectMain typedFns
  let topDefScope = (typedFns, classDefs, inhTree, classNames, classesData)
  forM_ topDefs (`checkTopDef` topDefScope)
  return (typedFns, classesData)

checkTopDef :: TopDef -> TopDefScope -> IO ()
checkTopDef topDef scope = case topDef of
  FnDef outType i args body ->
    typecheckFun (outType, i, args, body) scope Nothing Map.empty
  ClassDef ident _ -> checkClass ident scope
  ClassDefE ident _ _ -> checkClass ident scope
