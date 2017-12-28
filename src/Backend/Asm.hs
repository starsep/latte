module Asm where

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
  | Cmp String String
  | Custom String [String]
  | CustomString String
  | DataDecl String DataSize String
  | Divide String
  | EmptyLine
  | Extern String
  | Global String
  | Jmp String
  | Label String
  | Mov String String
  | Mul String
  | Or String String
  | Pop String
  | Push String
  | SectionData
  | SectionText
  | Sub String String
  | Xor String String
  deriving (Eq)

indent :: AsmStmt -> Bool
indent stmt = case stmt of
  CustomString{} -> False
  EmptyLine -> False
  Extern{} -> False
  Global{} -> False
  Label{} -> False
  SectionData -> False
  SectionText -> False
  _ -> True

showBinOp :: String -> String -> String -> String
showBinOp op left right = op ++ " " ++ left ++ ", " ++ right

instance Show AsmStmt where
  show (Add dest src) = showBinOp "add" dest src
  show (And left right) = showBinOp "and" left right
  show (Call name) = "call " ++ name
  show (Cmp arg1 arg2) = showBinOp "cmp" arg1 arg2
  show (Custom op args) = op ++ " " ++ intercalate ", " args
  show (CustomString s) = s
  show (DataDecl name size content) =
    name ++ " " ++ show size ++ " " ++ content
  show (Divide divisor) = "idiv " ++ divisor
  show EmptyLine = ""
  show (Extern name) = "extern " ++ name
  show (Global name) = "global " ++ name
  show (Jmp label) = "jmp " ++ label
  show (Label name) = name ++ ":"
  show (Mov dest src) = showBinOp "mov" dest src
  show (Mul arg) = "imul " ++ arg
  show (Or dest src) = showBinOp "or" dest src
  show (Pop dest) = "pop " ++ dest
  show (Push src) = "push qword " ++ src
  show SectionData = "\nsection .data"
  show SectionText = "\nsection .text"
  show (Sub dest src) = showBinOp "sub" dest src
  show (Xor dest src) = showBinOp "xor" dest src

data DataSize
  = DataByte
  deriving (Eq)

instance Show DataSize where
  show DataByte = "db"

preserveRegisters :: [String]
preserveRegisters =
  ["rbx", "rsp", "rbp", "r12", "r13", "r14", "r15"]

scratchRegisters :: [String]
scratchRegisters =
  ["rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9", "r10", "r11"]

registers :: [String]
registers = preserveRegisters ++ scratchRegisters

stringLiteralFromId :: Int -> String
stringLiteralFromId num = "_stringLiteral" ++ show num

parseStringLiteral :: String -> String
parseStringLiteral s =
  let parsedString = show s
      content = init (tail parsedString) in
  "`" ++ content ++ "\\0`"
