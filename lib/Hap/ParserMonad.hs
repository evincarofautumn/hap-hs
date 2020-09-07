{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hap.ParserMonad
  ( Action
  , AlexInput(..)
  , Parser(..)
  , ParserState(..)
  , StartCode
  , alexGetByte
  , alexInputPrevChar
  , alexMove
  , andBegin
  , emptyInput
  , emptyState
  , getSourcePosition
  , happyError
  , returnToken
  , runParser
  , setStartCode
  , sourceSpan
  , spanned
  , startPosition
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), evalStateT, modify)
import Data.Char (ord)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Word (Word8)
import Hap.Token

newtype Parser a = Parser
  { unParser
    :: AlexInput
    -> StateT ParserState (Either String) (a, AlexInput) }
  deriving (Applicative, Functor, Monad)
    via (StateT AlexInput (StateT ParserState (Either String)))

{-
instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return x = Parser \ input -> pure (input, x)
  Parser m >>= k = Parser \ input -> do
    (input', x) <- m input
    unParser (k x) input'
-}

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

-- Inputs

-- TODO: Derive lenses.
data AlexInput = AlexInput
  { inputSourceName :: !FilePath
  , inputPosition :: !SourcePosition
  , inputBuffer :: !String
  }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input = case inputBuffer input of
  x : xs -> Just
    ( fromIntegral (ord x)
    , input
      { inputPosition = alexMove (inputPosition input) x
      , inputBuffer   = xs
      }
    )
  [] -> Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "Lexer doesn't implement alexInputPrevChar"

happyError :: Parser a
happyError = do
  p <- getSourcePosition
  fail $ "Parse error at " ++ show (pretty p)

-- Positions

getSourcePosition :: Parser SourcePosition
getSourcePosition = Parser \ input@AlexInput { inputPosition }
  -> pure (inputPosition, input)

alexMove :: SourcePosition -> Char -> SourcePosition
alexMove (SourcePosition (Offset offset) (Row row) (Column column)) character
  = uncurry (SourcePosition (Offset (succ offset))) case character of
    '\n' -> (Row (succ row), Column 1)
    '\t' -> (Row row, Column ((column + 8) `div` 8 * 8))
    _    -> (Row row, Column (column + 1))

-- data SourcePosition = SourcePosition !Offset !Row !Column

startPosition :: SourcePosition
startPosition = SourcePosition (Offset 0) (Row 1) (Column 1)

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
