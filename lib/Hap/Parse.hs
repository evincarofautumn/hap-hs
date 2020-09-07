{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE StandaloneDeriving #-}

module Hap.Parse
--   ( Parser
--   , alexGetPosition
--   , alexShowError
--   -- , happyError
--   , errorP
--   , failP
--   , happyTokenizer
--   , returnP
--   , runParser
--   , thenP
--   ) where
  ( hapLexToken
  , hapBegin
  )
  where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), get)
import Data.Text.Prettyprint.Doc (pretty)
import Hap.ParserMonad
import Hap.Token
import Hap.Tokenizer
import qualified Data.Text.Prettyprint.Doc as Pretty

import Debug.Trace

-- Tokenization

hapGetToken
  :: AlexInput
  -> StateT ParserState (Either String) (Token SourceSpan, AlexInput)
hapGetToken input = do
  parserState <- get
  case alexScan input (parserStateStartCode parserState) of
    AlexEOF -> pure (EofToken, input)
    AlexError input' -> lift $ Left $ "Lexical error at " ++ show (pretty (inputPosition input))
    AlexSkip input' _width -> hapGetToken input'
    AlexToken input' width yield
      -> unParser (yield (input :: AlexInput) (width :: Int) :: Parser (Token SourceSpan)) (input' :: AlexInput)
      -- yield (input', take width (inputBuffer input))

hapBegin :: StartCode -> Action
hapBegin startCode (input, _) = do
  setStartCode startCode
  hapGetToken input

hapLexToken :: (Token SourceSpan -> Parser a) -> Parser a
hapLexToken continue = Parser $ \ input -> do
  (token, input') <- hapGetToken input
  unParser (continue token) input'

-- TODO: Use structured parse error type instead of 'String'.
-- newtype Parser a = Parser { unParser :: String -> Alex (Either String a) }
--   deriving (Applicative, Functor, Monad)
--     via (ExceptT String (ReaderT String Alex))
-- 
-- runParser :: Parser a -> String -> String -> Either String a
-- runParser parser sourceName input
--   = case runAlex input $ unParser parser sourceName of
--     Left alexError -> Left ("outer error: " ++ alexError)
--     Right alexResult -> case alexResult of
--       Left parserError -> Left ("inner error: " ++ parserError)
--       Right result -> Right result
--   -- = join $ runAlex input $ unParser parser sourceName
-- 
-- thenP :: Parser a -> (a -> Parser b) -> Parser b
-- thenP = (>>=)
-- 
-- returnP :: a -> Parser a
-- returnP = return
-- 
-- failP :: String -> Parser a
-- failP message = do
--   AlexPn offset row column <- liftAlex alexGetPosition
--   -- Parser . const . pure . Left
--   liftAlex (alexShowError (Row row, Column column, Just message))
-- 
-- {-
-- errorP :: Token SourceSpan -> Parser a
-- errorP token = do
--   -- pos <- getPosP
--   input <- liftAlex alexGetInput
--   let
--     message = show $ Pretty.hsep
--       [ pretty (tokenAnno token) <> ":"
--       , "error:"
--       , "unexpected"
--       , Pretty.squotes (pretty token) <> Pretty.semi
--       , "input:"
--       , pretty (show input)
--       ]
--   -- liftAlex $ alexShowError (0, 0, Just message)
--   failP message
-- -}
-- 
-- alexShowError :: (Show r, Show c) => (r, c, Maybe String) -> Alex a
-- alexShowError (row, column, e)
--   = alexError $ "show-error: " ++ (show (row, column, e))
-- 
-- getPosP :: Parser AlexPosn
-- getPosP = Parser $ const $ Right <$> alexGetPosition
-- 
-- alexGetPosition :: Alex AlexPosn
-- alexGetPosition = Alex $ \ s@AlexState { alex_pos = pos } -> Right (s, pos)
-- 
-- errorP :: Token SourceSpan -> Parser a
-- errorP token = liftAlex do
--   AlexPn _ row col <- alexGetPosition
--   state <- alexGetState
--   let
--     message = concat
--       [ "error at token "
--       , show (pretty token)
--       , " in state "
--       , show state
--       ]
--   alexShowError (row, col, Just message)
-- 
-- {-
-- happyError :: Parser a
-- happyError = Parser \ _sourceName -> do
--   AlexPn _ row col <- alexGetPosition
--   alexShowError (row, col, Nothing)
-- -}
-- 
-- deriving instance Show AlexState
-- 
-- alexGetState :: Alex AlexState
-- alexGetState = Alex $ \ s -> Right (s, s)
-- 
-- liftAlex :: Alex a -> Parser a
-- liftAlex = Parser . const . fmap Right
-- 
-- happyTokenizer :: (Token SourceSpan -> Parser a) -> Parser a
-- happyTokenizer continue = continue =<< lexToken
--   where
-- 
--     dummyUserState = error "dummy user state"
--     dummyStartCode = 0
-- 
--     lexToken :: Parser (Token SourceSpan)
--     lexToken = do
--       input@(loc, _previousChar, _bytes, buffer) <- liftAlex alexGetInput
--       case alexScan input dummyStartCode of
--         AlexEOF -> do
--           () <- flip trace (pure ()) $ concat
--             ["got eof"]
--           -- TODO: Include source span (@SourceSpan (Just loc, Just loc')@).
--           pure EofToken
--         AlexError input'@(loc', _previousChar', _bytes', buffer') -> do
--           () <- flip trace (pure ()) $ concat
--             [ "failing; input: "
--             , show input
--             , "; error state:"
--             , show input'
--             ]
--           -- TODO: Proper error reporting.
--           failP (show input ++ " / " ++ show input')
--         AlexSkip input' skipWidth -> do
--           () <- flip trace (pure ()) $ concat
--             ["skipping ", show skipWidth, " chars"]
--           liftAlex $ alexSetInput input'
--           lexToken
--         AlexToken input'@(loc', _prevChar', _bytes', buffer') width yield -> do
--           liftAlex $ alexSetInput input'
--           -- Note that we pass the /current/ input state to the token
--           -- continuation, not the /next/ state.
--           -- TODO: Include source span (@SourceSpan (Just loc, Just loc')@).
--           t <- liftAlex $ yield input width
--           let
--             (firstPartOfBuffer, firstPartOfBuffer') = let
--               bufferLength = length buffer
--               bufferLength' = length buffer'
--               difference = (bufferLength - bufferLength')
--               context = 8
--               in (take (difference + context) buffer, take context buffer')
--           () <- flip trace (pure ()) $ concat
--             [ "consumed "
--             , show width
--             , " chars ("
--             , take width buffer
--             , ") and returning token "
--             , show t
--             , " from buffer "
--             , show (firstPartOfBuffer ++ "...")
--             , " to "
--             , show (firstPartOfBuffer' ++ "...")
--             ]
--           pure t
