module Locals where

import AbsLatte
import qualified Data.Map as Map
import Data.Map (Map)

type Locals = Map String Int

classMethodLabel :: String -> String -> String
classMethodLabel className method = className ++ "@" ++ method

localsProg :: Program -> Locals
localsProg (Program topDefs) = foldl localsTopDef Map.empty topDefs

localsTopDef :: Locals -> TopDef -> Locals
localsTopDef locals (FnDef _ (Ident name) args block) =
  Map.insert name (length args + localsBlock block) locals
localsTopDef locals (ClassDefE ident _ props) =
  localsTopDef locals (ClassDef ident props)
localsTopDef locals (ClassDef (Ident className) props) =
  foldl (localsClassProp className) locals props

localsClassProp :: String -> Locals -> ClassProp -> Locals
localsClassProp _ locals (Field _ _) = locals
localsClassProp className locals (Method t (Ident name) args block) =
  let method = Ident $ classMethodLabel className name in
  localsTopDef locals $ FnDef t method args block

localsBlock :: Block -> Int
localsBlock (Block stmts) = sum $ map localsStmt stmts

localsStmt :: Stmt -> Int
localsStmt q = case q of
  Empty -> 0
  BStmt b -> localsBlock b
  Decl _ items -> length items
  Ass{} -> 0
  Incr{} -> 0
  Decr{} -> 0
  Ret{} -> 0
  VRet -> 0
  Cond _ stmt -> localsStmt stmt
  CondElse _ stmt stmt' -> max (localsStmt stmt) (localsStmt stmt')
  While _ stmt -> localsStmt stmt
  For _ _ _ stmt -> 3 + localsStmt stmt
  SExp{} -> 0
