{

module Hap.Tokenizer where

import Hap.Token
import qualified Data.Text as Text

}

%wrapper "monad"

--------------------------------------------------------------------------------
-- Character classes
--------------------------------------------------------------------------------

$whitespace    = [\t\n\v\f\r\ ]
$word_start    = [A-Z_a-z]
$word_continue = [0-9A-Z_a-z]
$digit         = [0-9]

--------------------------------------------------------------------------------
-- Lexical classes
--------------------------------------------------------------------------------

@word         = $word_start $word_continue*
@line_comment = "//".*
@integer      = $digit+

--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

token :-

--------------------------------------------------------------------------------
-- Silence
--------------------------------------------------------------------------------

  $whitespace+  ;
  @line_comment ;

--------------------------------------------------------------------------------
-- Primitive Tokens
--------------------------------------------------------------------------------

  -- We must match a word and then dispatch on whether it was a keyword in
  -- order to easily disambiguate keywords from name parts.
  @word { spanned \ s t -> case t of
    "add"      -> KeywordToken (s, AddKeyword)
    "after"    -> KeywordToken (s, AfterKeyword)
    "all"      -> KeywordToken (s, AllKeyword)
    "as"       -> KeywordToken (s, AsKeyword)
    "async"    -> KeywordToken (s, AsyncKeyword)
    "atomic"   -> KeywordToken (s, AtomicKeyword)
    "before"   -> KeywordToken (s, BeforeKeyword)
    "change"   -> KeywordToken (s, ChangeKeyword)
    "each"     -> KeywordToken (s, EachKeyword)
    "else"     -> KeywordToken (s, ElseKeyword)
    "entity"   -> KeywordToken (s, EntityKeyword)
    "every"    -> KeywordToken (s, EveryKeyword)
    "false"    -> KeywordToken (s, FalseKeyword)
    "for"      -> KeywordToken (s, ForKeyword)
    "function" -> KeywordToken (s, FunctionKeyword)
    "has"      -> KeywordToken (s, HasKeyword)
    "if"       -> KeywordToken (s, IfKeyword)
    "last"     -> KeywordToken (s, LastKeyword)
    "long"     -> KeywordToken (s, LongKeyword)
    "needs"    -> KeywordToken (s, NeedsKeyword)
    "next"     -> KeywordToken (s, NextKeyword)
    "on"       -> KeywordToken (s, OnKeyword)
    "redo"     -> KeywordToken (s, RedoKeyword)
    "remove"   -> KeywordToken (s, RemoveKeyword)
    "return"   -> KeywordToken (s, ReturnKeyword)
    "set"      -> KeywordToken (s, SetKeyword)
    "true"     -> KeywordToken (s, TrueKeyword)
    "until"    -> KeywordToken (s, UntilKeyword)
    "var"      -> KeywordToken (s, VarKeyword)
    "when"     -> KeywordToken (s, WhenKeyword)
    "whenever" -> KeywordToken (s, WheneverKeyword)
    "where"    -> KeywordToken (s, WhereKeyword)
    "which"    -> KeywordToken (s, WhichKeyword)
    "while"    -> KeywordToken (s, WhileKeyword)
    _          -> WordToken (s, Text.pack t)
  }

  @integer { spanned \ s t -> DigitsToken (s, read t) }

--------------------------------------------------------------------------------
-- Symbol Tokens
--------------------------------------------------------------------------------

  "("           { spanned \ s _t -> LeftParenthesisToken s }
  ")"           { spanned \ s _t -> RightParenthesisToken s }
  "!"           { spanned \ s _t -> BangToken s }
  "#"           { spanned \ s _t -> NumberToken s }
  "%"           { spanned \ s _t -> PercentToken s }
  "&"           { spanned \ s _t -> AndToken s }
  "*"           { spanned \ s _t -> StarToken s }
  "+"           { spanned \ s _t -> PlusToken s }
  ","           { spanned \ s _t -> CommaToken s }
  "-"           { spanned \ s _t -> MinusToken s }
  "."           { spanned \ s _t -> DotToken s }
  "/"           { spanned \ s _t -> SlashToken s }
  ":"           { spanned \ s _t -> ColonToken s }
  ";"           { spanned \ s _t -> SemicolonToken s }
  "<"           { spanned \ s _t -> LessThanToken s }
  "<="          { spanned \ s _t -> LessThanOrEqualToken s }
  "<>"          { spanned \ s _t -> NotEqualToken s }
  "="           { spanned \ s _t -> EqualToken s }
  ">"           { spanned \ s _t -> GreaterThanToken s }
  ">="          { spanned \ s _t -> GreaterThanOrEqualToken s }
  "?"           { spanned \ s _t -> QuestionToken s }
  "@"           { spanned \ s _t -> AtToken s }
  "["           { spanned \ s _t -> LeftSquareBracketToken s }
  "\\"          { spanned \ s _t -> BackslashToken s }
  "]"           { spanned \ s _t -> RightSquareBracketToken s }
  "^"           { spanned \ s _t -> CaretToken s }
  "{"           { spanned \ s _t -> LeftCurlyBraceToken s }
  "|"           { spanned \ s _t -> PipeToken s }
  "}"           { spanned \ s _t -> RightCurlyBraceToken s }
  "~"           { spanned \ s _t -> TildeToken s }

--------------------------------------------------------------------------------

{

tokenizer :: Alex (Token SourceSpan)
tokenizer = alexMonadScan

alexEOF :: Alex (Token SourceSpan)
alexEOF = pure EofToken

sourcePosition :: AlexPosn -> SourcePosition
sourcePosition (AlexPn offset row column)
  = SourcePosition (Offset offset) (Row row) (Column column)

sourceSpan :: AlexPosn -> Int -> SourceSpan
sourceSpan alexPosn width = SourceSpan (Just start, Just end)
  where
    start@(SourcePosition offset row column) = sourcePosition alexPosn
    end = SourcePosition
      (Offset (getOffset offset + width))
      row
      (Column (getColumn column + width))

spanned
  :: (SourceSpan -> String -> a)
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex a
spanned k
  = \ (position, _previousChar, _bytes, buffer) width
  -> pure $ k (sourceSpan position width) (take width buffer)

}
