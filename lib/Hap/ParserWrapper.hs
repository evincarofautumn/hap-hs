-- TODO: Rename this and the other parser files.
module Hap.ParserWrapper where

import Data.List (stripPrefix)
import Hap.Language (Program)
import Hap.Parse (runAlex)
import Hap.Parser (parser)
import Hap.Token (SourceSpan)

data ErrorClass
  = SyntaxError !(Maybe String)
  | LexicalError
  | GeneralError !String
  deriving stock (Show)

data Error = Error
  { errorLine   :: !Int
  , errorColumn :: !Int
  , errorClass  :: !ErrorClass
  } deriving stock (Show)

parse :: String -> Either Error (Program SourceSpan)
parse input = case runAlex input parser of
  Right result -> Right result
  Left message
    | Just showError <- stripPrefix showErrorPrefix message -> let
      (line, column, message') = read showError :: (Int, Int, Maybe String)
      in Left $ Error line column $ SyntaxError message'
    | Just lexicalError <- stripPrefix lexicalErrorPrefix message -> let
      -- TODO: Parse this properly.
      lineString = takeWhile (/= ',') lexicalError
      line = read lineString :: Int
      columnString = drop (length ", column " + length lineString) lexicalError
      column = read columnString :: Int
      in Left $ Error line column LexicalError
    | otherwise -> Left $ Error 0 0 $ GeneralError message
  where
    showErrorPrefix :: String
    -- TODO: Common up with string in 'Hap.Parse'.
    showErrorPrefix = "show-error: "

    lexicalErrorPrefix :: String
    lexicalErrorPrefix = "lexical error at line "
