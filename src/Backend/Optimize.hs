module Optimize (optimize) where

import Asm
import Data.List.Unique
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read

optimizeStmt :: AsmStmts -> AsmStmts
optimizeStmt (Sub _ "0" : rest) = optimizeStmt rest
optimizeStmt (Add _ "0" : rest) = optimizeStmt rest
optimizeStmt (Cmp r "0" : rest) =
  Test r r : optimizeStmt rest
optimizeStmt (Mov r "0" : rest) =
  if isAddress r then
    Mov r "0" : optimizeStmt rest
  else
    Xor r r : optimizeStmt rest
optimizeStmt (Add r1 x : Add r2 y : rest) =
  optimizeAdd (r1, x, r2, y) 1 1 ++ optimizeStmt rest
optimizeStmt (Sub r1 x : Add r2 y : rest) =
  optimizeAdd (r1, x, r2, y) (-1) 1 ++ optimizeStmt rest
optimizeStmt (Add r1 x : Sub r2 y : rest) =
  optimizeAdd (r1, x, r2, y) 1 (-1) ++ optimizeStmt rest
optimizeStmt (Sub r1 x : Sub r2 y : rest) =
  optimizeAdd (r1, x, r2, y) (-1) (-1) ++ optimizeStmt rest
optimizeStmt (Push p : Add r x : rest) =
  if x /= stackPointer then
    Push p : optimizeStmt (Add r x : rest)
  else (
    let v = read x :: Int
        x' = show $ v - 8 in
    Add stackPointer x' : optimizeStmt rest)
optimizeStmt (Jmp x : Label y : rest)
  | x == y = Label y : optimizeStmt rest
  | otherwise = Jmp x : Label y : optimizeStmt rest
optimizeStmt (Push x : Pop y : rest)
  | x == y = optimizeStmt rest
  | otherwise = Mov y x : optimizeStmt rest
optimizeStmt (h:t) = h : optimizeStmt t
optimizeStmt [] = []

type AsmAddOp = String -> String -> AsmStmt

signToAdd :: Int -> AsmAddOp
signToAdd x = if x > 0 then Add else Sub

optimizeAdd :: (String, String, String, String) ->
  Int -> Int -> AsmStmts
optimizeAdd (r1, x, r2, y) s1 s2 =
  let x' = readMaybe x :: Maybe Int
      y' = readMaybe y :: Maybe Int in
  if r1 /= r2 || isNothing x' || isNothing y' then
    [signToAdd s1 r1 x, signToAdd s2 r2 y]
  else (
    let xv = s1 * fromJust x'
        yv = s2 * fromJust y'
        value = xv + yv in
    [signToAdd value r1 $ show value]
  )

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
        Label x -> '@' `elem` x || x `elem` used
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
      usefulLabels = map snd $ filter (isMainOrMethod . fst) (Map.toList labels)
      dfsWithAcc acc x = Set.union acc $ dfs x progLen prog labels useful
      useful' = foldl dfsWithAcc Set.empty usefulLabels
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

isMainOrMethod :: String -> Bool
isMainOrMethod x = x == "main" || '@' `elem` x

isUseful :: AsmStmt -> Bool
isUseful stmt = case stmt of
  Label x -> isMainOrMethod x
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
