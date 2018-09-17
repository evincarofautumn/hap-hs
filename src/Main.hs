{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Applicative ((<**>), many, optional)
import Control.Arrow (returnA)
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Hap.Compiler (Context, compile, newEmptyContext)
import Hap.Language (parseProgram)
import Hap.Runtime (Env, Flag(..), newEmptyEnv, run)
import Options.Applicative.Arrows (asA, runA)
import SDL (($=))
import System.Console.Haskeline (getInputLine, outputStrLn, runInputT)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPrint, hPutStrLn, stderr)
import qualified Options.Applicative as Options
import qualified SDL
import qualified System.Console.Haskeline as Haskeline

data InputMode = InteractiveInput | BatchInput

data OutputMode = TextOutput | GraphicsOutput

data Options = Options
  { optInputMode :: !InputMode
  , optInputPaths :: [FilePath]
  , optOutputMode :: !OutputMode
  }

optionsParser :: Options.Parser Options
optionsParser = runA $ proc () -> do

  optInputMode <- opt
    (fmap (fromMaybe InteractiveInput)
      . optional
      . Options.option parseInputMode)
    [ Options.long "input"
    , Options.short 'i'
    , Options.metavar "interactive/batch"
    , Options.help "Input mode (default 'interactive'). \
      \Interactive mode allows entering programs interactively; \
      \batch mode allows reading programs from source files."
    ] -< ()

  optOutputMode <- opt
    (fmap (fromMaybe GraphicsOutput)
      . optional
      . Options.option parseOutputMode)
    [ Options.long "output"
    , Options.short 'o'
    , Options.metavar "graphics/text"
    , Options.help "Output mode (default 'graphics'). \
      \Graphics mode allows graphics/audio output and \
      \keyboard/mouse/joystick input; \
      \text mode allows text output and input."
    ] -< ()

  optInputPaths <- opt
    (many . Options.strArgument)
    [ Options.metavar "PATH..."
    , Options.help "Paths to Hap source files. \
      \These are preloaded in interactive mode and executed in batch mode."
    ] -< ()

  returnA -< Options{..}

  where
    opt f = asA . f . mconcat

    parseInputMode = Options.eitherReader $ \ case
      "interactive" -> Right InteractiveInput
      "batch" -> Right BatchInput
      unknownMode -> Left $ concat
        [ "Unknown input mode '"
        , unknownMode
        , "'. Valid modes are 'interactive' and 'batch'. \
          \Default mode is 'interactive'."
        ]

    parseOutputMode = Options.eitherReader $ \ case
      "text" -> Right TextOutput
      "graphics" -> Right GraphicsOutput
      unknownMode -> Left $ concat
        [ "Unknown output mode '"
        , unknownMode
        , "'. Valid modes are 'text' and 'graphics'. \
          \Default mode is 'graphics'."
        ]

main :: IO ()
main = start =<< Options.execParser options
  where
    options = Options.info (optionsParser <**> Options.helper)
      $ Options.fullDesc
        <> Options.progDesc "Run Hap programs or interactively write Hap code."
        <> Options.header "Hap - a simple event-based programming language."

start :: Options -> IO ()
start options = case optOutputMode options of

  TextOutput -> case optInputMode options of

    InteractiveInput -> do
      exit =<< runRepl

    BatchInput -> case optInputPaths options of

      [] -> do
        batchMissingPaths
        exit 1

      paths -> do
        context <- newEmptyContext
        env <- newEmptyEnv putStr []
        exit =<< runBatch context env paths

  GraphicsOutput -> do

    SDL.initializeAll
    window <- SDL.createWindow "Hap" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    quit <- newEmptyMVar

    -- In graphics mode, we run the graphics loop on the main thread and start
    -- the REPL or program in a separate thread.
    --
    -- TODO: Add communication channels between runtime and graphics loop.
    _ <- forkIO $ case optInputMode options of

      InteractiveInput -> do
        putMVar quit =<< runRepl

      BatchInput -> case optInputPaths options of

        [] -> do
          batchMissingPaths
          putMVar quit 1

        paths -> do
          context <- newEmptyContext
          -- TODO: Add graphics communication channels to environment.
          env <- newEmptyEnv putStr []
          putMVar quit =<< runBatch context env paths

    startGraphicsLoop renderer quit
    status <- takeMVar quit
    exit status

  where

    startGraphicsLoop :: SDL.Renderer -> MVar Int -> IO ()
    startGraphicsLoop renderer quit = loop
      where
        loop = do
          events <- SDL.pollEvents
          let gotQuitEvent = not $ null $ filter isQuitEvent events
          replQuit <- not <$> isEmptyMVar quit
          if
            | gotQuitEvent -> putMVar quit 0
            | replQuit -> pure ()
            | otherwise -> do
              SDL.rendererDrawColor renderer $= SDL.V4 0 128 255 255
              SDL.clear renderer
              SDL.present renderer
              loop
          where
            isQuitEvent event = case SDL.eventPayload event of
              SDL.KeyboardEvent keyboardEvent
                | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                , SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
                -> True
              SDL.QuitEvent
                -> True
              _ -> False

    batchMissingPaths :: IO ()
    batchMissingPaths = do
      hPutStrLn stderr "Running in batch mode with no input source files; exiting."

    runBatch :: Context IO -> Env IO -> [FilePath] -> IO Int
    runBatch context env paths = do
      sources <- traverse readFile paths
      let parseResults = zipWith parseProgram paths sources
      case partitionEithers parseResults of

        -- All parses were successful.
        ([], programs) -> do
          let program = mconcat programs
          case compile context program of
            Left compileError -> do
              hPutStrLn stderr compileError
              pure 1
            Right compiled -> do
              -- TODO: Return exit status.
              run env compiled
              pure 0

        -- At least one source file failed to parse.
        (parseErrors, _) -> do
          traverse_ (hPrint stderr) parseErrors
          pure 1

runRepl :: IO Int
runRepl = runInputT Haskeline.defaultSettings $ do
  context <- liftIO (newEmptyContext @(Haskeline.InputT IO))
  env <- liftIO $ newEmptyEnv Haskeline.outputStr []
  let
    prompt = "> "  -- ""

    loop :: Haskeline.InputT IO Int
    loop = do
      entry <- getInputLine prompt
      case entry of
        Nothing -> do
          outputStrLn "Bye!"
          pure 0
        Just line -> case stripPrefix "//" line of
          Just command -> case command of
            "quit" -> do
              outputStrLn "Bye!"
              pure 0
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
                    run env compiled
                loop
  loop

exit :: Int -> IO a
exit status = case status of
  0 -> exitWith ExitSuccess
  _ -> exitWith $ ExitFailure status
