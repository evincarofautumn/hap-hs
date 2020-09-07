{-# LANGUAGE OverloadedStrings #-}

module Hap.ParserMonad
  ( Action
  , AlexInput(..)
  , Parser(..)
  , ParserState(..)
  , StartCode
  , alexGetByte
  , alexInputPrevChar
  , andBegin
  , emptyInput
  , emptyState
  , getSourcePosition
  , parseFailure
  , returnToken
  , runParser
  , setStartCode
  , sourceSpan
  , spanned
  , startColumn
  , startPosition
  , startRow
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), evalStateT, modify)
import Data.Char (ord)
import Data.Text.Prettyprint.Doc (pretty)
import Hap.Token
import qualified Data.Text.Prettyprint.Doc as Pretty

-- TODO: Use structured parse error type instead of 'String'.
newtype Parser a = Parser
  { unParser
    :: AlexInput
    -> StateT ParserState (Either String) (a, AlexInput) }
  deriving (Applicative, Functor, Monad)
    via (StateT AlexInput (StateT ParserState (Either String)))

instance MonadFail Parser where
  fail = Parser . const . lift . Left

runParser :: Parser a -> String -> String -> Either String a
runParser parser sourceName source
  = fmap fst $ flip evalStateT emptyState
  $ unParser parser emptyInput
  { inputSourceName = sourceName
  , inputBuffer     = source
  }

emptyInput :: AlexInput
emptyInput = AlexInput
  -- TODO: Do something more sensible with unset source name.
  { inputSourceName = "<anonymous>"
  , inputPosition   = startPosition
  , inputBuffer     = mempty
  }

data ParserState = ParserState
  { parserStateStartCode :: !StartCode
  }

type StartCode = Int

emptyState :: ParserState
emptyState = ParserState
  { parserStateStartCode = 0 :: StartCode
  }

type Action
  = (AlexInput, String)
  -> StateT ParserState (Either String)
    (Token SourceSpan, AlexInput)

setStartCode :: StartCode -> StateT ParserState (Either String) ()
setStartCode startCode = modify \ parserState
  -> parserState { parserStateStartCode = startCode }

andBegin :: Action -> StartCode -> Action
(action `andBegin` startCode) x = do
  setStartCode startCode
  action x

returnToken :: Token SourceSpan -> Action
returnToken token (input, _) = lift $ pure (token, input)

--------------------------------------------------------------------------------
-- Input
--------------------------------------------------------------------------------

-- | Lexer input state.
--
-- TODO: Derive lenses.
--
-- See note [Alex Name].
data AlexInput = AlexInput
  { inputSourceName :: !FilePath
  , inputPosition   :: !SourcePosition
  , inputBuffer     :: !String
  }

-- | Get the next “byte” (character) of input.
--
-- See note [Alex Name].
alexGetByte :: AlexInput -> Maybe (Int, AlexInput)
alexGetByte input = case inputBuffer input of
  x : xs -> Just
    ( ord x
    , input
      { inputPosition = updateSourcePosition x (inputPosition input)
      , inputBuffer   = xs
      }
    )
  [] -> Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _
  = error "Lexer doesn't implement alexInputPrevChar for left contexts"

parseFailure :: Token SourceSpan -> Parser a
parseFailure currentToken = do
  sourceName <- getSourceName
  position <- getSourcePosition
  fail $ show $ Pretty.hsep
    [ Pretty.hcat
      [ pretty sourceName
      , Pretty.colon
      , pretty position
      , Pretty.colon
      ]
    , "parse error at"
    , Pretty.squotes (pretty currentToken)
    ]

-- Note [Alex Name]:
--
-- The name of anything annotated with this note is significant to Alex, because
-- it’s referenced from the generated code, and thus can’t be changed.

--------------------------------------------------------------------------------
-- Positions
--------------------------------------------------------------------------------

getSourceName :: Parser String
getSourceName = Parser \ input@AlexInput { inputSourceName }
  -> pure (inputSourceName, input)

getSourcePosition :: Parser SourcePosition
getSourcePosition = Parser \ input@AlexInput { inputPosition }
  -> pure (inputPosition, input)

updateSourcePosition :: Char -> SourcePosition -> SourcePosition
updateSourcePosition character
  (SourcePosition (Offset offset) (Row row) (Column column))
  = uncurry (SourcePosition (Offset (succ offset))) case character of
    '\n' -> (Row (row + 1), startColumn)
    '\t' -> (Row row, Column ((column + tabWidth) `div` tabWidth * tabWidth))
    _    -> (Row row, Column (column + 1))
  where
    tabWidth :: Int
    tabWidth = 8

-- data SourcePosition = SourcePosition !Offset !Row !Column

startPosition :: SourcePosition
startPosition = SourcePosition (Offset 0) startRow startColumn

startRow :: Row
startRow = Row 1

startColumn :: Column
startColumn = Column 1

-- TODO: Account for multi-line tokens; width is just characters.
sourceSpan :: SourcePosition -> Int -> SourceSpan
sourceSpan start@(SourcePosition offset row column) width
  = SourceSpan (Just start, Just end)
  where
    end = SourcePosition
      (Offset (getOffset offset + width))
      row
      (Column (getColumn column + width))

spanned
  :: (SourceSpan -> String -> a)
  -> AlexInput
  -> Int
  -> Parser a
spanned continue
  = \ AlexInput { inputPosition, inputBuffer } width
  -> pure $ continue
    (sourceSpan inputPosition width)
    (take width inputBuffer)
