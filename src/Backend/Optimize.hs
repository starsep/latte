module Optimize where

import Asm
import Data.List.Unique
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map, (!))

optimizeStmt :: AsmStmts -> AsmStmts
optimizeStmt (Sub _ "0" : rest) = optimizeStmt rest
optimizeStmt (Add _ "0" : rest) = optimizeStmt rest
optimizeStmt (Cmp r "0" : rest) =
  Test r r : optimizeStmt rest
optimizeStmt (Mov r "0" : rest) = Xor r r : optimizeStmt rest
optimizeStmt (Jmp x : Label y : rest)
  | x == y = Label y : optimizeStmt rest
  | otherwise = Jmp x : Label y : optimizeStmt rest
optimizeStmt (Push x : Pop y : rest)
  | x == y = optimizeStmt rest
  | otherwise = Mov y x : optimizeStmt rest
optimizeStmt (h:t) = h : optimizeStmt t
optimizeStmt [] = []

optimizeOnce :: AsmStmts -> AsmStmts
optimizeOnce prog =
  removeDeadCode $ removeUnusedLabels $ optimizeStmt prog

optimize :: AsmStmts -> AsmStmts
optimize prog =
  let prog' = optimizeOnce prog in
  if prog == prog' then
    prog
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
usedLabels prog =
  sortUniq $ foldl (\acc s -> labelInstruction s ++ acc) ["main"] prog

labelInstruction :: AsmStmt -> [String]
labelInstruction (Call l) = [l]
labelInstruction (Jmp l) = [l]
labelInstruction (Je l) = [l]
labelInstruction (Jne l) = [l]
labelInstruction _ = []

conditionalJump :: AsmStmt -> Bool
conditionalJump stmt = case stmt of
  Je{} -> True
  Jne{} -> True
  Call{} -> True -- not conditional but returning.
  _ -> False

type Labels = Map String Int
type Useful = Set Int

removeDeadCode :: AsmStmts -> AsmStmts
removeDeadCode prog =
  let labels = findLabels Map.empty 0 prog
      progLen = length prog
      numberedProg = zip [0..progLen - 1] prog
      useful = buildUseful Set.empty numberedProg
      mainN = labels ! "main"
      useful' = dfs mainN progLen prog labels useful
      nProg = filter (\(n, _) -> Set.member n useful') numberedProg in
      map snd nProg

dfs :: Int -> Int -> AsmStmts -> Labels -> Useful -> Useful
dfs n len prog labels useful =
  let useful' = Set.insert n useful
      stmt = prog !! n
      next = nextInstructions stmt labels n
      next' = filter (\x -> x < len && Set.notMember x useful') next in
  foldl (\u nth -> dfs nth len prog labels u) useful' next'

nextInstructions :: AsmStmt -> Labels -> Int -> [Int]
nextInstructions Return _ _ = []
nextInstructions stmt labels n =
  let to = labelInstruction stmt in
  if null to then
    [n + 1]
  else (
    let label = head to in
    if Map.member label labels then (
      let nth = labels ! label in
      if conditionalJump stmt then
        [nth, n + 1]
      else
        [nth])
    else [n + 1])

buildUseful :: Useful -> [(Int, AsmStmt)] -> Useful
buildUseful useful ((n, h):t) =
  let useful' = if isUseful h then Set.insert n useful else useful in
  buildUseful useful' t
buildUseful useful [] = useful

isUseful :: AsmStmt -> Bool
isUseful stmt = case stmt of
  Label "main" -> True
  EmptyLine -> True
  SectionData -> True
  SectionText -> True
  DataDecl{} -> True
  Extern{} -> True
  Global{} -> True
  _ -> False

findLabels :: Labels -> Int -> AsmStmts -> Labels
findLabels labels n (Label name : t) =
    findLabels (Map.insert name n labels) (n + 1) t
findLabels labels n (_ : t) = findLabels labels (n + 1) t
findLabels labels _ [] = labels
