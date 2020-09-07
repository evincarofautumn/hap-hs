{-# LANGUAGE OverloadedStrings #-}

module Hap.Parse
  ( Parser
  , alexGetPosition
  , alexShowError
  -- , happyError
  , errorP
  , failP
  , happyTokenizer
  , returnP
  , runParser
  , thenP
  ) where

import Control.Monad (join)
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Text.Prettyprint.Doc (pretty)
import Hap.Token
import Hap.Tokenizer
import qualified Data.Text.Prettyprint.Doc as Pretty

import Debug.Trace

-- TODO: Use structured parse error type instead of 'String'.
newtype Parser a = Parser { unParser :: String -> Alex (Either String a) }
  deriving (Applicative, Functor, Monad)
    via (ExceptT String (ReaderT String Alex))

runParser :: Parser a -> String -> String -> Either String a
runParser parser sourceName input
  = join $ runAlex input $ unParser parser sourceName

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

returnP :: a -> Parser a
returnP = return

failP :: String -> Parser a
failP message = do
  AlexPn offset row column <- liftAlex alexGetPosition
  -- Parser . const . pure . Left
  liftAlex (alexShowError (Row row, Column column, Just message))

{-
errorP :: Token SourceSpan -> Parser a
errorP token = do
  -- pos <- getPosP
  input <- liftAlex alexGetInput
  let
    message = show $ Pretty.hsep
      [ pretty (tokenAnno token) <> ":"
      , "error:"
      , "unexpected"
      , Pretty.squotes (pretty token) <> Pretty.semi
      , "input:"
      , pretty (show input)
      ]
  -- liftAlex $ alexShowError (0, 0, Just message)
  failP message
-}

alexShowError :: (Show r, Show c) => (r, c, Maybe String) -> Alex a
alexShowError (row, column, e)
  = alexError $ "show-error: " ++ (show (row, column, e))

getPosP :: Parser AlexPosn
getPosP = Parser $ const $ Right <$> alexGetPosition

alexGetPosition :: Alex AlexPosn
alexGetPosition = Alex $ \ s@AlexState { alex_pos = pos } -> Right (s, pos)

errorP :: Token SourceSpan -> Parser a
errorP token = liftAlex do
  AlexPn _ row col <- alexGetPosition
  alexShowError (row, col, Just (show (pretty token)))

{-
happyError :: Parser a
happyError = Parser \ _sourceName -> do
  AlexPn _ row col <- alexGetPosition
  alexShowError (row, col, Nothing)
-}

alexGetState :: Alex AlexState
alexGetState = Alex $ \ s -> Right (s, s)

liftAlex :: Alex a -> Parser a
liftAlex = Parser . const . fmap Right

happyTokenizer :: (Token SourceSpan -> Parser a) -> Parser a
happyTokenizer continue = do
  input <- liftAlex alexGetInput
  state <- liftAlex alexGetState
  continue =<< lexToken input state
  where

    dummyUserState = undefined
    dummyStartCode = 0

    lexToken :: AlexInput -> AlexState -> Parser (Token SourceSpan)
    lexToken input@(_, _, _, oldBuf) state = loop
      where
        loop = case alexScanUser dummyUserState input 0 of
          AlexEOF -> do
            () <- flip trace (pure ()) $ concat
              ["got eof"]
            pure EofToken
          AlexError errorState@(loc', _prevChar, _bytes, _buf) -> do
            -- TODO: Proper error reporting.
            () <- flip trace (pure ()) $ concat
              ["failing in state", show errorState]
            failP (show errorState)
          AlexSkip input' skipWidth -> do
            () <- flip trace (pure ()) $ concat
              ["skipping ", show skipWidth, " chars"]
            liftAlex $ alexSetInput input'
            lexToken input' state
          AlexToken input'@(end, _prevChar, _bytes, buf') tokenWidth returnToken
            -> liftAlex do
              -- Note that we pass the /current/ input state to the token
              -- continuation, not the /next/ state.
              t <- returnToken input tokenWidth
              () <- flip trace (pure ()) $ concat
                [ "consumed "
                , show tokenWidth
                , " chars ("
                , take tokenWidth oldBuf
                , ") and returning token "
                , show t
                , " from buffer "
                , show oldBuf
                , " to "
                , show buf'
                ]
              alexSetInput input'
              pure t
