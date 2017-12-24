module AsmStmt where

import Data.List

type AsmStmts = [AsmStmt]

printAsm :: AsmStmts -> String
printAsm = foldl (\acc x ->
    acc ++
    (if indent x then "\t" else "") ++
    show x ++ "\n"
  ) ""

data AsmStmt
  = Add String String
  | And String String
  | Call String
  | Custom String [String]
  | CustomString String
  | DataDecl String DataSize String
  | Empty
  | Extern String
  | Global String
  | Label String
  | Leave
  | KernelCall
  | Mov String String
  | Pop String
  | Push String
  | Ret
  | SectionData
  | SectionText
  | Sub String String
  | Xor String String

indent :: AsmStmt -> Bool
indent stmt = case stmt of
  Add{} -> True
  And{} -> True
  Call{} -> True
  Custom{} -> True
  CustomString{} -> False
  DataDecl{} -> False
  Empty -> False
  Extern{} -> False
  Global{} -> False
  Label{} -> False
  Leave -> True
  KernelCall -> True
  Mov{} -> True
  Pop{} -> True
  Push{} -> True
  Ret -> True
  SectionData -> False
  SectionText -> False
  Sub{} -> True
  Xor{} -> True

instance Show AsmStmt where
  show (Add dest src) = "add " ++ dest ++ ", " ++ src
  show (And left right) = "and " ++ left ++ ", " ++ right
  show (Call name) = "alignCall " ++ name
  show (Custom op args) = op ++ " " ++ intercalate ", " args
  show (CustomString s) = s
  show (DataDecl name size content) =
    name ++ " " ++ show size ++ " " ++ content
  show Empty = ""
  show (Extern name) = "extern " ++ name
  show (Global name) = "global " ++ name
  show (Label name) = name ++ ":"
  show Leave = "leave"
  show KernelCall = "int 0x80"
  show (Mov dest src) = "mov " ++ dest ++ ", " ++ src
  show (Pop dest) = "pop " ++ dest
  show (Push src) = "push " ++ src
  show Ret = "ret"
  show SectionData = "\nsection .data"
  show SectionText = "\nsection .text"
  show (Sub dest src) = "sub " ++ dest ++ ", " ++ src
  show (Xor left right) = "xor " ++ left ++ ", " ++ right

data DataSize
  = DataByte

instance Show DataSize where
  show DataByte = "db"
