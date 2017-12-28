module Asm where

import AbsLatte
import Data.List

type AsmStmts = [AsmStmt]
type Register = String
type Registers = [Register]

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
  | Cqo
  -- | Custom String [String]
  -- | CustomString String
  | DataDecl String DataSize String
  | Divide String
  | EmptyLine
  | Extern String
  | Global String
  | Je String
  | Jmp String
  | Jne String
  | Label String
  | Mov String String
  | Movzx String String
  | Mul String
  | Negate String
  | Or String String
  | Pop String
  | Push String
  | Return
  | SectionData
  | SectionText
  | Set RelOp String
  | Sub String String
  | Test String String
  | Xor String String
  deriving (Eq)

indent :: AsmStmt -> Bool
indent stmt = case stmt of
  -- CustomString{} -> False
  EmptyLine -> False
  Extern{} -> False
  Global{} -> False
  Label{} -> False
  SectionData -> False
  SectionText -> False
  _ -> True

showBinOp :: String -> String -> String -> String
showBinOp op left right = op ++ " " ++ left ++ ", " ++ right

relOpToAsm :: RelOp -> String
relOpToAsm op = case op of
  LTH -> "setl"
  LE -> "setle"
  GTH -> "setg"
  GE -> "setge"
  EQU -> "sete"
  NE -> "setne"

instance Show AsmStmt where
  show (Add dest src) = showBinOp "add" dest src
  show (And left right) = showBinOp "and" left right
  show (Call name) = "call " ++ name
  show (Cmp arg1 arg2) = showBinOp "cmp" arg1 arg2
  show Cqo = "cqo"
  --show (Custom op args) = op ++ " " ++ intercalate ", " args
  --show (CustomString s) = s
  show (DataDecl name size content) =
    name ++ " " ++ show size ++ " " ++ content
  show (Divide divisor) = "idiv " ++ divisor
  show EmptyLine = ""
  show (Extern name) = "extern " ++ name
  show (Global name) = "global " ++ name
  show (Je label) = "je " ++ label
  show (Jmp label) = "jmp " ++ label
  show (Jne label) = "jne " ++ label
  show (Label name) = name ++ ":"
  show (Mov dest src) = showBinOp "mov" dest src
  show (Movzx dest src) = showBinOp "movzx" dest src
  show (Mul arg) = "imul " ++ arg
  show (Negate arg) = "neg " ++ arg
  show (Or dest src) = showBinOp "or" dest src
  show (Pop dest) = "pop " ++ dest
  show (Push src) = "push qword " ++ src
  show Return = "ret"
  show SectionData = "\nsection .data"
  show SectionText = "\nsection .text"
  show (Set relOp arg) = relOpToAsm relOp ++ " " ++ arg
  show (Sub dest src) = showBinOp "sub" dest src
  show (Test arg1 arg2) = showBinOp "test" arg1 arg2
  show (Xor dest src) = showBinOp "xor" dest src

data DataSize
  = DataByte
  deriving (Eq)

instance Show DataSize where
  show DataByte = "db"

preserveRegisters :: Registers
preserveRegisters =
  ["rbx", "rsp", "rbp", "r12", "r13", "r14", "r15"]

scratchRegisters :: Registers
scratchRegisters =
  ["rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9", "r10", "r11"]

registers :: Registers
registers = preserveRegisters ++ scratchRegisters

argRegisters :: Registers
argRegisters = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

scratch32Registers :: Registers
scratch32Registers =
  ["eax", "edi", "esi", "edx", "ecx", "r8d", "r9d", "r10d", "r11d"]

scratch16Registers :: Registers
scratch16Registers =
  ["ax", "di", "si", "dx", "cx", "r8w", "r9w", "r10w", "r11w"]

scratch8Registers :: Registers
scratch8Registers =
  ["al", "dil", "sil", "dl", "cl", "r8b", "r9b", "r10b", "r11b"]

convertRegister :: Registers -> Register -> Register
convertRegister to reg =
  let (Just index) = elemIndex reg scratchRegisters in
  to !! index

to32bit :: Register -> Register
to32bit = convertRegister scratch32Registers

to16bit :: Register -> Register
to16bit = convertRegister scratch16Registers

to8bit :: Register -> Register
to8bit = convertRegister scratch8Registers

stringLiteralFromId :: Int -> String
stringLiteralFromId num = "_stringLiteral" ++ show num

parseStringLiteral :: String -> String
parseStringLiteral s =
  let parsedString = show s
      content = init (tail parsedString) in
  "`" ++ content ++ "\\0`"

mulOpArg :: Register
mulOpArg = "rax"

mulOpResult :: MulOp -> Register
mulOpResult Times = "rax"
mulOpResult Div = "rax"
mulOpResult Mod = "rdx"

mulOpStmt :: MulOp -> (String -> AsmStmt)
mulOpStmt Times = Mul
mulOpStmt Div = Divide
mulOpStmt Mod = Divide

resultReg :: Register
resultReg = "rax"

basePointer :: Register
basePointer = "rbp"

stackPointer :: Register
stackPointer = "rsp"
