module Main (main) where

import Control.Monad
import Data.List (isPrefixOf, partition)
import System.Exit (exitFailure)
import System.Environment (getArgs, getExecutablePath)
import System.FilePath.Posix (takeBaseName)
import System.IO (openFile, IOMode(ReadMode), hGetContents, hPutStrLn, stderr)
import ParLatte
import AbsLatte
import ErrM

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

compiler :: Bool -> String -> Program -> String
compiler optimizeOn basename prog = "Hello Compiler!"

main :: IO ()
main = do
    (optimizeOn, filename) <- parseArgs
    let basename = takeBaseName filename
    source <- readSource filename
    program <- case pProgram (myLexer source) of
        Ok p -> return p
        Bad msg -> do
          die $ "Lexing failed: " ++ show msg
          return $ Program []
    putStrLn $ compiler optimizeOn basename program
