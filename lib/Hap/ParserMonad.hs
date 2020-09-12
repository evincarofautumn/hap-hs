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

import Control.Monad (guard, join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), evalStateT, modify)
import Data.Char (ord)
import Data.Functor (($>))
import Data.Text.Prettyprint.Doc (Pretty(..), (<+>))
import Hap.Pretty (oxford)
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

parseFailure :: (Token SourceSpan, [String]) -> Parser a
parseFailure (currentToken, expectedTokens) = do
  sourceName <- getSourceName
  position <- getSourcePosition
  let
    prefix = Pretty.hcat
      [ pretty sourceName
      , Pretty.colon
      , pretty position
      , Pretty.colon
      ]
  fail $ show $ Pretty.vcat $ (prefix <+>) <$> messages
  where
    messages = join
      [ pure unexpected
      , guard (not (null expectedTokens)) $> expected
      ]
    unexpected = "unexpected" <+> Pretty.squotes (pretty currentToken)
    expected = Pretty.hsep
      $ "expected one of" <> Pretty.colon
      : oxford Pretty.comma "or"
        (prettyTerminal <$> expectedTokens)
    prettyTerminal = pretty . getTerminal

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

data Terminal

  -- Primitives
  = WordTerminal
  | DigitsTerminal

  -- Symbols
  | LeftParenthesisTerminal
  | RightParenthesisTerminal
  | BangTerminal
  | NumberTerminal
  | PercentTerminal
  | AndTerminal
  | StarTerminal
  | PlusTerminal
  | CommaTerminal
  | MinusTerminal
  | RightArrowTerminal
  | DotTerminal
  | SlashTerminal
  | ColonTerminal
  | SemicolonTerminal
  | LessThanTerminal
  | LessThanOrEqualTerminal
  | NotEqualTerminal
  | EqualTerminal
  | GreaterThanTerminal
  | GreaterThanOrEqualTerminal
  | QuestionTerminal
  | AtTerminal
  | LeftSquareBracketTerminal
  | BackslashTerminal
  | RightSquareBracketTerminal
  | CaretTerminal
  | UnderscoreTerminal
  | LeftCurlyBraceTerminal
  | PipeTerminal
  | RightCurlyBraceTerminal
  | TildeTerminal

  -- Keywords
  | AddTerminal
  | AfterTerminal
  | AllTerminal
  | AsTerminal
  | AsyncTerminal
  | AtomicTerminal
  | BeforeTerminal
  | ChangeTerminal
  | EachTerminal
  | ElseTerminal
  | EntityTerminal
  | EveryTerminal
  | FalseTerminal
  | ForTerminal
  | FunctionTerminal
  | HasTerminal
  | IfTerminal
  | InTerminal
  | LastTerminal
  | LongTerminal
  | NeedsTerminal
  | NextTerminal
  | NullTerminal
  | OnTerminal
  | RedoTerminal
  | RemoveTerminal
  | ReturnTerminal
  | SetTerminal
  | TrueTerminal
  | UntilTerminal
  | VarTerminal
  | WhenTerminal
  | WheneverTerminal
  | WhereTerminal
  | WhichTerminal
  | WhileTerminal

instance Pretty Terminal where

  pretty = \ case

    -- Primitives
    WordTerminal   -> "name part"
    DigitsTerminal -> "number part"

    -- Symbols
    LeftParenthesisTerminal    -> "left parenthesis ('(')"
    RightParenthesisTerminal   -> "right parenthesis (')')"
    BangTerminal               -> "exclamation mark ('!')"
    NumberTerminal             -> "number sign ('#')"
    PercentTerminal            -> "percent sign ('%')"
    AndTerminal                -> "and sign / ampersand ('&')"
    StarTerminal               -> "star / asterisk ('*')"
    PlusTerminal               -> "plus sign ('+')"
    CommaTerminal              -> "comma (',')"
    MinusTerminal              -> "minus sign / hyphen ('-')"
    RightArrowTerminal         -> "right arrow ('->')"
    DotTerminal                -> "dot / period / fullstop ('.')"
    SlashTerminal              -> "slash ('/')"
    ColonTerminal              -> "colon (':')"
    SemicolonTerminal          -> "semicolon (';')"
    LessThanTerminal           -> "less-than sign ('<')"
    LessThanOrEqualTerminal    -> "less-than or equal sign ('<=')"
    NotEqualTerminal           -> "not-equal sign ('<>')"
    EqualTerminal              -> "equal sign ('=')"
    GreaterThanTerminal        -> "greater-than sign ('>')"
    GreaterThanOrEqualTerminal -> "greater-than or equal sign ('>=')"
    QuestionTerminal           -> "question mark ('?')"
    AtTerminal                 -> "at sign ('@')"
    LeftSquareBracketTerminal  -> "left square bracket ('[')"
    BackslashTerminal          -> "backslash ('\\')"
    RightSquareBracketTerminal -> "right square bracket (']')"
    CaretTerminal              -> "caret ('^')"
    UnderscoreTerminal         -> "underscore ('_')"
    LeftCurlyBraceTerminal     -> "left curly brace ('{')"
    PipeTerminal               -> "vertical bar / pipe sign ('|')"
    RightCurlyBraceTerminal    -> "right curly brace ('}')"
    TildeTerminal              -> "tilde ('~')"

    -- Keywords
    AddTerminal      -> "'add' keyword"
    AfterTerminal    -> "'after' keyword"
    AllTerminal      -> "'all' keyword"
    AsTerminal       -> "'as' keyword"
    AsyncTerminal    -> "'async' keyword"
    AtomicTerminal   -> "'atomic' keyword"
    BeforeTerminal   -> "'before' keyword"
    ChangeTerminal   -> "'change' keyword"
    EachTerminal     -> "'each' keyword"
    ElseTerminal     -> "'else' keyword"
    EntityTerminal   -> "'entity' keyword"
    EveryTerminal    -> "'every' keyword"
    FalseTerminal    -> "'false' keyword"
    ForTerminal      -> "'for' keyword"
    FunctionTerminal -> "'function' keyword"
    HasTerminal      -> "'has' keyword"
    IfTerminal       -> "'if' keyword"
    InTerminal       -> "'in' keyword"
    LastTerminal     -> "'last' keyword"
    LongTerminal     -> "'long' keyword"
    NeedsTerminal    -> "'needs' keyword"
    NextTerminal     -> "'next' keyword"
    NullTerminal     -> "'null' keyword"
    OnTerminal       -> "'on' keyword"
    RedoTerminal     -> "'redo' keyword"
    RemoveTerminal   -> "'remove' keyword"
    ReturnTerminal   -> "'return' keyword"
    SetTerminal      -> "'set' keyword"
    TrueTerminal     -> "'true' keyword"
    UntilTerminal    -> "'until' keyword"
    VarTerminal      -> "'var' keyword"
    WhenTerminal     -> "'when' keyword"
    WheneverTerminal -> "'whenever' keyword"
    WhereTerminal    -> "'where' keyword"
    WhichTerminal    -> "'which' keyword"
    WhileTerminal    -> "'while' keyword"

getTerminal :: String -> Terminal
getTerminal = \ case

  -- Primitives
  "word"     -> WordTerminal
  "digits"   -> DigitsTerminal

  -- Symbols
  "'('"      -> LeftParenthesisTerminal
  "')'"      -> RightParenthesisTerminal
  "'!'"      -> BangTerminal
  "'#'"      -> NumberTerminal
  "'%'"      -> PercentTerminal
  "'&'"      -> AndTerminal
  "'*'"      -> StarTerminal
  "'+'"      -> PlusTerminal
  "','"      -> CommaTerminal
  "'-'"      -> MinusTerminal
  "'->'"     -> RightArrowTerminal
  "'.'"      -> DotTerminal
  "'/'"      -> SlashTerminal
  "':'"      -> ColonTerminal
  "';'"      -> SemicolonTerminal
  "'<'"      -> LessThanTerminal
  "'<='"     -> LessThanOrEqualTerminal
  "'<>'"     -> NotEqualTerminal
  "'='"      -> EqualTerminal
  "'>'"      -> GreaterThanTerminal
  "'>='"     -> GreaterThanOrEqualTerminal
  "'?'"      -> QuestionTerminal
  "'@'"      -> AtTerminal
  "'['"      -> LeftSquareBracketTerminal
  "'\\'"     -> BackslashTerminal
  "']'"      -> RightSquareBracketTerminal
  "'^'"      -> CaretTerminal
  "'_'"      -> UnderscoreTerminal
  "'{'"      -> LeftCurlyBraceTerminal
  "'|'"      -> PipeTerminal
  "'}'"      -> RightCurlyBraceTerminal
  "'~'"      -> TildeTerminal

  -- Keywords
  "add"      -> AddTerminal
  "after"    -> AfterTerminal
  "all"      -> AllTerminal
  "as"       -> AsTerminal
  "async"    -> AsyncTerminal
  "atomic"   -> AtomicTerminal
  "before"   -> BeforeTerminal
  "change"   -> ChangeTerminal
  "each"     -> EachTerminal
  "else"     -> ElseTerminal
  "entity"   -> EntityTerminal
  "every"    -> EveryTerminal
  "false"    -> FalseTerminal
  "for"      -> ForTerminal
  "function" -> FunctionTerminal
  "has"      -> HasTerminal
  "if"       -> IfTerminal
  "in"       -> InTerminal
  "last"     -> LastTerminal
  "long"     -> LongTerminal
  "needs"    -> NeedsTerminal
  "next"     -> NextTerminal
  "null"     -> NullTerminal
  "on"       -> OnTerminal
  "redo"     -> RedoTerminal
  "remove"   -> RemoveTerminal
  "return"   -> ReturnTerminal
  "set"      -> SetTerminal
  "true"     -> TrueTerminal
  "until"    -> UntilTerminal
  "var"      -> VarTerminal
  "when"     -> WhenTerminal
  "whenever" -> WheneverTerminal
  "where"    -> WhereTerminal
  "which"    -> WhichTerminal
  "while"    -> WhileTerminal

  unknown    -> error $ "unknown token " ++ show unknown
