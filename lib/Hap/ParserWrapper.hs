-- TODO: Rename this and the other parser files.
module Hap.ParserWrapper
  ( ParseError(..)
  , ParseErrorInfo(..)
  , parseProgram
  , parseTokens
  ) where

import Control.Arrow (left)
import Data.List (stripPrefix)
import Hap.Language (Program)
import Hap.Parse (Parser, runParser)
import Hap.Parser (programParser, tokensParser)
import Hap.Token (Column(..), Row(..), SourceSpan, Token)

data ParseErrorInfo
  = SyntaxErrorInfo !(Maybe String)
  | LexicalErrorInfo
  | GeneralErrorInfo !String
  deriving stock (Show)

data ParseError = ParseError
  { errorSource :: !String
  , errorPos    :: !(Maybe (Row, Column))
  , errorInfo   :: !ParseErrorInfo
  } deriving stock (Show)

parseTokens :: String -> String -> Either ParseError [Token SourceSpan]
parseTokens = runParser' tokensParser

parseProgram :: String -> String -> Either ParseError (Program SourceSpan)
parseProgram = runParser' programParser

runParser' :: Parser a -> String -> String -> Either ParseError a
runParser' parser sourceName input
  = left (parseErrorFromMessage sourceName)
  $ runParser parser sourceName input


parseErrorFromMessage :: String -> String -> ParseError
parseErrorFromMessage sourceName message
  | Just showError <- stripPrefix showErrorPrefix message = let
    (row, column, message') = read showError :: (Int, Int, Maybe String)
    in parseError (Just (Row row, Column column))
      $ SyntaxErrorInfo message'

  | Just lexicalError <- stripPrefix lexicalErrorPrefix message = let
    -- TODO: Parse this properly.
    rowString = takeWhile (/= ',') lexicalError
    row = read rowString :: Int
    columnString = drop (length ", column " + length rowString) lexicalError
    column = read columnString :: Int
    in parseError (Just (Row row, Column column)) LexicalErrorInfo

  | otherwise = parseError Nothing $ GeneralErrorInfo message

  where

    showErrorPrefix :: String
    -- TODO: Common up with string in 'Hap.Parse'.
    showErrorPrefix = "show-error: "

    lexicalErrorPrefix :: String
    lexicalErrorPrefix = "lexical error at line "

    parseError :: Maybe (Row, Column) -> ParseErrorInfo -> ParseError
    parseError = ParseError sourceName
