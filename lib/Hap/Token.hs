module Hap.Token where

import Hap.Data.Monoid (Maximum(..), Minimum(..))
import Data.Text (Text)

newtype Offset = Offset { getOffset :: Int }
  deriving stock (Eq, Ord, Show)

newtype Line = Line { getLine :: Int }
  deriving stock (Eq, Ord, Show)

newtype Column = Column { getColumn :: Int }
  deriving stock (Eq, Ord, Show)

-- TODO: Add "begin" and "end" positions? Would allow 'Monoid'.
data SourcePosition = SourcePosition !Offset !Line !Column
  deriving stock (Eq, Ord, Show)

newtype SourceSpan = SourceSpan (Maybe SourcePosition, Maybe SourcePosition)
  deriving stock (Show)
  deriving (Monoid, Semigroup)
    via (Minimum SourcePosition, Maximum SourcePosition)

data Token anno

  -- | An identifier part.
  = WordToken (anno, Text)

  -- | A (contextual) keyword.
  | KeywordToken (anno, Keyword)

  -- | An integer literal part.
  | DigitsToken (anno, Integer)

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
  | ForKeyword
  | FunctionKeyword
  | HasKeyword
  | IfKeyword
  | LastKeyword
  | LongKeyword
  | NeedsKeyword
  | NextKeyword
  | OnKeyword
  | RedoKeyword
  | RemoveKeyword
  | ReturnKeyword
  | SetKeyword
  | UntilKeyword
  | VarKeyword
  | WhenKeyword
  | WheneverKeyword
  | WhereKeyword
  | WhichKeyword
  | WhileKeyword
  deriving stock (Eq, Show)

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
  LeftCurlyBraceToken     anno      -> anno
  PipeToken               anno      -> anno
  RightCurlyBraceToken    anno      -> anno
  TildeToken              anno      -> anno
  -- TODO: Use proper error reporting.
  EofToken                          -> error "cannot get annotation from end of file"
