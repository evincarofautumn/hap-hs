{-# LANGUAGE OverloadedStrings #-}

module Hap.Parse
  ( lexToken
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), get)
import Data.Text.Prettyprint.Doc (pretty)
import Hap.ParserMonad (AlexInput(..), Parser(..), ParserState(..))
import Hap.Token (SourceSpan, Token(..))
import Hap.Tokenizer (AlexReturn(..), alexScan)
import qualified Data.Text.Prettyprint.Doc as Pretty

getToken
  :: AlexInput
  -> StateT ParserState (Either String) (Token SourceSpan, AlexInput)
getToken input = do
  parserState <- get
  case alexScan input (parserStateStartCode parserState) of
    AlexEOF -> pure (EofToken, input)
    AlexError input' -> lift $ Left $ show $ Pretty.hsep
      [ Pretty.hcat
        [ pretty $ inputSourceName input
        , Pretty.colon
        , pretty $ inputPosition input
        , Pretty.colon
        ]
      , "lexical error; prior input:"
      , pretty $ show $ inputBuffer input
      , "remaining input:"
      , pretty $ show $ inputBuffer input'
      ]
    AlexSkip input' _width -> getToken input'
    AlexToken input' width yield
      -> unParser (yield input width) input'

lexToken :: (Token SourceSpan -> Parser a) -> Parser a
lexToken continue = Parser $ \ input -> do
  (token, input') <- getToken input
  unParser (continue token) input'
