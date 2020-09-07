module Hap.Parse
  ( Parser
  , alexGetPosition
  , alexShowError
  , happyError
  , happyTokenizer
  , returnP
  , runAlex
  , thenP
  ) where

import Hap.Token
import Hap.Tokenizer

type Parser a = Alex a

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

returnP :: a -> Parser a
returnP = return

alexShowError :: (Show l, Show c) => (l, c, Maybe String) -> Alex a
alexShowError (line, column, e)
  = alexError $ "show-error: " ++ (show (line, column, e))

alexGetPosition :: Alex AlexPosn
alexGetPosition = Alex $ \ s@AlexState { alex_pos = pos } -> Right (s, pos)

happyError :: Parser a
happyError = do
  (AlexPn _ line col) <- alexGetPosition
  alexShowError (line, col, Nothing)

happyTokenizer :: (Token SourceSpan -> Parser a) -> Parser a
happyTokenizer f = alexMonadScan >>= f
