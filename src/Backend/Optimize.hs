module Optimize where

import AsmStmt
import Data.List.Unique

optimizeOnce :: AsmStmts -> AsmStmts
optimizeOnce (Cmp r "0" : rest) =
  Custom "test" [r, r] : optimizeOnce rest
optimizeOnce (Mov r "0" : rest) = Xor r r : optimizeOnce rest
optimizeOnce (Jmp x : Label y : rest)
  | x == y = Label y : optimizeOnce rest
  | otherwise = Jmp x : Label y : optimizeOnce rest
optimizeOnce (Push x : Pop y : rest)
  | x == y = optimizeOnce rest
  | otherwise = Mov y x : optimizeOnce rest
optimizeOnce (h:t) = h : optimizeOnce t
optimizeOnce [] = []

optimize :: AsmStmts -> AsmStmts
optimize prog =
  let prog' = optimizeOnce prog in
  if prog == prog' then
    removeUnusedLabels prog
  else
    optimize prog'

removeUnusedLabels :: AsmStmts -> AsmStmts
removeUnusedLabels prog =
  let used = usedLabels prog
      isNotUnusedLabel s = case s of
        Label x -> x `elem` used
        Extern x -> x `elem` used
        _ -> True in
  filter isNotUnusedLabel prog

usedLabels :: AsmStmts -> [String]
usedLabels prog = sortUniq $ usedLabels' prog

usedLabels' :: AsmStmts -> [String]
usedLabels' (Call l : t) = l : usedLabels' t
usedLabels' (Jmp l : t) = l : usedLabels' t
usedLabels' (Custom "je" [l] : t) = l : usedLabels' t
usedLabels' (Custom "jne" [l] : t) = l : usedLabels' t
usedLabels' (_:t) = usedLabels' t
usedLabels' [] = ["main"]
