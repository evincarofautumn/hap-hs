module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.List (stripPrefix)
import Hap.Compiler (compile, newEmptyContext)
import Hap.Language (parseProgram)
import Hap.Runtime (newEmptyEnv, run)
import System.Console.Haskeline (getInputLine, outputStrLn, runInputT)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPrint, hPutStrLn, stderr)
import qualified System.Console.Haskeline as Haskeline

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [path] -> do
      source <- readFile path
      case parseProgram path source of
        Left parseError -> do
          hPrint stderr parseError
          exitWith $ ExitFailure 1
        Right program -> do
          print program
    _ -> do
      hPutStrLn stderr "Usage: hap [source]"
      exitWith $ ExitFailure 1

runRepl :: IO ()
runRepl = do
  context <- newEmptyContext
  env <- newEmptyEnv
  let
    loop = do
      entry <- getInputLine "> "
      case entry of
        Nothing -> do
          outputStrLn "Bye!"
        Just line -> case stripPrefix "//" line of
          Just command -> case command of
            "quit" -> do
              outputStrLn "Bye!"
            _ -> do
              outputStrLn $ "Unknown command '" ++ command ++ "'."
              loop
          Nothing -> do
            case parseProgram "<interactive>" line of
              Left parseError -> do
                outputStrLn $ show parseError
                loop
              Right program -> do
                case compile context program of
                  Left compileError -> do
                    outputStrLn compileError
                  Right compiled -> do
                    liftIO $ run env compiled
                loop

  runInputT Haskeline.defaultSettings loop
