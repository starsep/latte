module AsmStmt where

type AsmStmts = [AsmStmt]

printAsm :: AsmStmts -> String
printAsm = foldl (\acc x ->
    acc ++
    (if indent x then "\t" else "") ++
    show x ++ "\n"
  ) ""

data AsmStmt
  = And String String
  | Call String
  | DataDecl String DataSize String
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
  | Xor String String

indent :: AsmStmt -> Bool
indent stmt = case stmt of
  And _ _ -> True
  Call _ -> True
  DataDecl{} -> False
  Extern _ -> False
  Global _ -> False
  Label _ -> False
  Leave -> True
  KernelCall -> True
  Mov _ _ -> True
  Pop _ -> True
  Push _ -> True
  Ret -> True
  SectionData -> False
  SectionText -> False
  Xor _ _ -> True

instance Show AsmStmt where
  show (And left right) = "and " ++ left ++ ", " ++ right
  show (Call name) = "call " ++ name
  show (DataDecl name size content) =
    name ++ " " ++ show size ++ " " ++ content
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
  show (Xor left right) = "xor " ++ left ++ ", " ++ right

data DataSize
  = DataByte

instance Show DataSize where
  show DataByte = "db"
