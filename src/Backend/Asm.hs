module Asm where

argRegisters :: [String]
argRegisters = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

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
