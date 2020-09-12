{-# LANGUAGE OverloadedStrings #-}

module Hap.Token
  ( Column(..)
  , DecimalDigit(..)
  , Keyword(..)
  , Offset(..)
  , Row(..)
  , SourcePosition(..)
  , SourceSpan(..)
  , Token(..)
  , decimalDigitChar
  , tokenAnno
  ) where

import Data.Char (ord)
import Data.Ix (inRange)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Hap.Data.Monoid (Maximum(..), Minimum(..))
import qualified Data.Text.Prettyprint.Doc as Pretty

newtype Offset = Offset { getOffset :: Int }
  deriving stock (Eq, Ord, Show)

newtype Row = Row { getRow :: Int }
  deriving stock (Eq, Ord, Show)

instance Pretty Row where
  pretty = pretty . getRow

newtype Column = Column { getColumn :: Int }
  deriving stock (Eq, Ord, Show)

instance Pretty Column where
  pretty = pretty . getColumn

-- TODO: Add "begin" and "end" positions? Would allow 'Monoid'.
data SourcePosition = SourcePosition !Offset !Row !Column
  deriving stock (Eq, Ord, Show)

instance Pretty SourcePosition where
  pretty (SourcePosition _ row column) = Pretty.hcat
    [ pretty row
    , "."
    , pretty column
    ]

newtype SourceSpan = SourceSpan (Maybe SourcePosition, Maybe SourcePosition)
  deriving stock (Show)
  deriving (Monoid, Semigroup)
    via (Minimum SourcePosition, Maximum SourcePosition)

instance Pretty SourceSpan where
  pretty (SourceSpan range) = case range of
    (Nothing,    Nothing)  -> "?"
    (Just start, Nothing)  -> pretty start <> "-?"
    (Nothing,    Just end) -> "?-" <> pretty end
    (Just start, Just end) -> prettyPositionSpan start end
    where
      prettyPositionSpan
        (SourcePosition _ startRow startColumn)
        (SourcePosition _ endRow endColumn)
        | startRow == endRow = if startColumn == endColumn

          -- row.column
          then Pretty.hcat
            [ pretty startRow
            , "."
            , pretty startColumn
            ]

          -- row.column1-column2
          else Pretty.hcat
            [ pretty startRow
            , "."
            , pretty startColumn
            , "-"
            , pretty endColumn
            ]

        -- row1.column1-row2.column2
        | otherwise = Pretty.hcat
          [ pretty startRow
          , "."
          , pretty startColumn
          , "-"
          , pretty endRow
          , "."
          , pretty endColumn
          ]

data Token anno

  -- | An identifier part.
  = WordToken (anno, Text)

  -- | A (contextual) keyword.
  | KeywordToken (anno, Keyword)

  -- | A number part.
  | DigitsToken (anno, NonEmpty DecimalDigit)

  -- | @(@
  | LeftParenthesisToken anno

  -- | @)@
  | RightParenthesisToken anno

  -- | @!@
  | BangToken anno

  -- | @#@
  | NumberToken anno

  -- | @%@
  | PercentToken anno

  -- | @&@
  | AndToken anno

  -- | @*@
  | StarToken anno

  -- | @+@
  | PlusToken anno

  -- | @,@
  | CommaToken anno

  -- | @-@
  | MinusToken anno

  -- | @->@
  | RightArrowToken anno

  -- | @.@
  | DotToken anno

  -- | @/@
  | SlashToken anno

  -- | @:@
  | ColonToken anno

  -- | @;@
  | SemicolonToken anno

  -- | @<@
  | LessThanToken anno

  -- | @<=@
  | LessThanOrEqualToken anno

  -- | @<>@
  | NotEqualToken anno

  -- | @=@
  | EqualToken anno

  -- | @>@
  | GreaterThanToken anno

  -- | @>=@
  | GreaterThanOrEqualToken anno

  -- | @?@
  | QuestionToken anno

  -- | @\@@
  | AtToken anno

  -- | @[@
  | LeftSquareBracketToken anno

  -- | @\\@
  | BackslashToken anno

  -- | @]@
  | RightSquareBracketToken anno

  -- | @^@
  | CaretToken anno

  -- | @_@
  | UnderscoreToken anno

  -- | @{@
  | LeftCurlyBraceToken anno

  -- | @|@
  | PipeToken anno

  -- | @}@
  | RightCurlyBraceToken anno

  -- | @~@
  | TildeToken anno

  -- | End of file.
  | EofToken

  deriving stock (Eq, Show)

instance Pretty (Token anno) where
  pretty = \ case
    WordToken (_, word)       -> pretty word
    KeywordToken (_, keyword) -> pretty keyword
    DigitsToken (_, digits)   -> foldMap pretty digits
    LeftParenthesisToken{}    -> "("
    RightParenthesisToken{}   -> ")"
    BangToken{}               -> "!"
    NumberToken{}             -> "#"
    PercentToken{}            -> "%"
    AndToken{}                -> "&"
    StarToken{}               -> "*"
    PlusToken{}               -> "+"
    CommaToken{}              -> ","
    MinusToken{}              -> "-"
    RightArrowToken{}         -> "->"
    DotToken{}                -> "."
    SlashToken{}              -> "/"
    ColonToken{}              -> ":"
    SemicolonToken{}          -> ";"
    LessThanToken{}           -> "<"
    LessThanOrEqualToken{}    -> "<="
    NotEqualToken{}           -> "<>"
    EqualToken{}              -> "="
    GreaterThanToken{}        -> ">"
    GreaterThanOrEqualToken{} -> ">="
    QuestionToken{}           -> "?"
    AtToken{}                 -> "@"
    LeftSquareBracketToken{}  -> "["
    BackslashToken{}          -> "\\"
    RightSquareBracketToken{} -> "]"
    CaretToken{}              -> "^"
    UnderscoreToken{}         -> "_"
    LeftCurlyBraceToken{}     -> "{"
    PipeToken{}               -> "|"
    RightCurlyBraceToken{}    -> "}"
    TildeToken{}              -> "~"
    EofToken                  -> "<end of file>"

newtype DecimalDigit = DecimalDigit Int
  deriving stock (Eq, Show)

decimalDigitChar :: DecimalDigit -> Char
decimalDigitChar = toEnum . (+ ord '0') . fromEnum

instance Bounded DecimalDigit where
  minBound = DecimalDigit 0
  maxBound = DecimalDigit 9

instance Enum DecimalDigit where
  fromEnum (DecimalDigit x) = x
  toEnum x
    | inRange (0, 9) x = DecimalDigit x
    | otherwise = error
      $ "toEnum: decimal digit out of range (" <> show x <> ")"

instance Pretty DecimalDigit where
  pretty = pretty . fromEnum

data Keyword
  = AddKeyword
  | AfterKeyword
  | AllKeyword
  | AsKeyword
  | AsyncKeyword
  | AtomicKeyword
  | BeforeKeyword
  | ChangeKeyword
  | EachKeyword
  | ElseKeyword
  | EntityKeyword
  | EveryKeyword
  | FalseKeyword
  | ForKeyword
  | FunctionKeyword
  | HasKeyword
  | IfKeyword
  | InKeyword
  | LastKeyword
  | LongKeyword
  | NeedsKeyword
  | NextKeyword
  | NullKeyword
  | OnKeyword
  | RedoKeyword
  | RemoveKeyword
  | ReturnKeyword
  | SetKeyword
  | TrueKeyword
  | UntilKeyword
  | VarKeyword
  | WhenKeyword
  | WheneverKeyword
  | WhereKeyword
  | WhichKeyword
  | WhileKeyword
  deriving stock (Eq, Show)

instance Pretty Keyword where
  pretty = \ case
    AddKeyword      -> "add"
    AfterKeyword    -> "after"
    AllKeyword      -> "all"
    AsKeyword       -> "as"
    AsyncKeyword    -> "async"
    AtomicKeyword   -> "atomic"
    BeforeKeyword   -> "before"
    ChangeKeyword   -> "change"
    EachKeyword     -> "each"
    ElseKeyword     -> "else"
    EntityKeyword   -> "entity"
    EveryKeyword    -> "every"
    FalseKeyword    -> "false"
    ForKeyword      -> "for"
    FunctionKeyword -> "function"
    HasKeyword      -> "has"
    IfKeyword       -> "if"
    InKeyword       -> "in"
    LastKeyword     -> "last"
    LongKeyword     -> "long"
    NeedsKeyword    -> "needs"
    NextKeyword     -> "next"
    NullKeyword     -> "null"
    OnKeyword       -> "on"
    RedoKeyword     -> "redo"
    RemoveKeyword   -> "remove"
    ReturnKeyword   -> "return"
    SetKeyword      -> "set"
    TrueKeyword     -> "true"
    UntilKeyword    -> "until"
    VarKeyword      -> "var"
    WhenKeyword     -> "when"
    WheneverKeyword -> "whenever"
    WhereKeyword    -> "where"
    WhichKeyword    -> "which"
    WhileKeyword    -> "while"

tokenAnno :: Token anno -> anno
tokenAnno = \ case
  WordToken               (anno, _) -> anno
  KeywordToken            (anno, _) -> anno
  DigitsToken             (anno, _) -> anno
  LeftParenthesisToken    anno      -> anno
  RightParenthesisToken   anno      -> anno
  BangToken               anno      -> anno
  NumberToken             anno      -> anno
  PercentToken            anno      -> anno
  AndToken                anno      -> anno
  StarToken               anno      -> anno
  PlusToken               anno      -> anno
  CommaToken              anno      -> anno
  MinusToken              anno      -> anno
  RightArrowToken         anno      -> anno
  DotToken                anno      -> anno
  SlashToken              anno      -> anno
  ColonToken              anno      -> anno
  SemicolonToken          anno      -> anno
  LessThanToken           anno      -> anno
  LessThanOrEqualToken    anno      -> anno
  NotEqualToken           anno      -> anno
  EqualToken              anno      -> anno
  GreaterThanToken        anno      -> anno
  GreaterThanOrEqualToken anno      -> anno
  QuestionToken           anno      -> anno
  AtToken                 anno      -> anno
  LeftSquareBracketToken  anno      -> anno
  BackslashToken          anno      -> anno
  RightSquareBracketToken anno      -> anno
  CaretToken              anno      -> anno
  UnderscoreToken         anno      -> anno
  LeftCurlyBraceToken     anno      -> anno
  PipeToken               anno      -> anno
  RightCurlyBraceToken    anno      -> anno
  TildeToken              anno      -> anno
  -- TODO: Use proper error reporting or remove partiality here.
  EofToken                          -> error "cannot get annotation from end of file"
