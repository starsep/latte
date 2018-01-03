module Main (main) where

import AbsLatte
import Control.Monad
import Context
import Compiler
import Check (check)
import Data.List (isPrefixOf, partition)
import ErrM
import ParLatte
import Print (parsing)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.IO (openFile, IOMode(ReadMode), hGetContents, hPutStrLn, stderr)

-- import System.Exit (die)
-- doesn't work on ghc 7.6.3
die :: String -> IO ()
die err = hPutStrLn stderr err >> exitFailure

parseFlags :: [String] -> (Bool, [String])
parseFlags argsWithFlags =
  let (flags, args) = partition (\x -> "-" `isPrefixOf` x) argsWithFlags in
  ("-O0" `notElem` flags, args)

parseArgs :: IO (Bool, String)
parseArgs = do
    argsWithFlags <- getArgs
    let (optimizeOn, args) = parseFlags argsWithFlags
    path <- getExecutablePath
    when (length args /= 1) $
        die $ "Usage: " ++ path ++ " SOURCE"
    return (optimizeOn, head args)

readSource :: String -> IO String
readSource filename = do
    file <- openFile filename ReadMode
    hGetContents file

main :: IO ()
main = do
    (optimizeOn, filename) <- parseArgs
    source <- readSource filename
    program <- case pProgram (myLexer source) of
        Ok p -> return p
        Bad msg -> do
          parsing msg $ Context [CParLex]
          return $ Program []
    types <- check program
    hPutStrLn stderr "OK"
    putStrLn $ compiler optimizeOn program types
