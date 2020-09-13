{

module Hap.Tokenizer
  ( AlexReturn(..)
  , alexScan
  ) where

import Control.Monad (join)
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe (fromMaybe)
import Hap.ParserMonad (AlexInput, alexGetByte, spanned)
import Hap.Token (Keyword(..), Token(..), quoteFromChar)
import qualified Data.Text as Text

}

--------------------------------------------------------------------------------
-- Character classes
--------------------------------------------------------------------------------

$whitespace    = [\t\n\v\f\r\ ]
$word_start    = [A-Za-z]
$word_continue = [0-9A-Za-z]
$digit         = [0-9]
$quote         = [\'\"]
$nonquote      = ~$quote

--------------------------------------------------------------------------------
-- Lexical classes
--------------------------------------------------------------------------------

@word         = $word_start $word_continue*
@line_comment = "//".*
@digits       = $digit+
@text         = $quote $nonquote* $quote

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
    "in"       -> KeywordToken (s, InKeyword)
    "last"     -> KeywordToken (s, LastKeyword)
    "long"     -> KeywordToken (s, LongKeyword)
    "needs"    -> KeywordToken (s, NeedsKeyword)
    "next"     -> KeywordToken (s, NextKeyword)
    "null"     -> KeywordToken (s, NullKeyword)
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

  @digits { spanned \ s t -> DigitsToken
    ( s
    , fromMaybe (toEnum 0 :| []) $ nonEmpty
      $ (toEnum . subtract (ord '0') . ord) <$> t
    )
  }

  @text { let
    getOpen = Text.splitAt 1
    getClose = join (Text.splitAt . subtract 1 . Text.length)
    in spanned \ s (getOpen . Text.pack -> (open, getClose -> (body, close)))
    -> TextToken
      (quoteFromChar (Text.head open))
      (quoteFromChar (Text.head close))
      (s, body)
  }

--------------------------------------------------------------------------------
-- Symbol Tokens
--------------------------------------------------------------------------------

  "("           { spanned \ s _t -> LeftParenthesisToken s    }
  ")"           { spanned \ s _t -> RightParenthesisToken s   }
  "!"           { spanned \ s _t -> BangToken s               }
  "#"           { spanned \ s _t -> NumberToken s             }
  "%"           { spanned \ s _t -> PercentToken s            }
  "&"           { spanned \ s _t -> AndToken s                }
  "*"           { spanned \ s _t -> StarToken s               }
  "+"           { spanned \ s _t -> PlusToken s               }
  ","           { spanned \ s _t -> CommaToken s              }
  "-"           { spanned \ s _t -> MinusToken s              }
  "->"          { spanned \ s _t -> RightArrowToken s         }
  "."           { spanned \ s _t -> DotToken s                }
  "/"           { spanned \ s _t -> SlashToken s              }
  ":"           { spanned \ s _t -> ColonToken s              }
  ";"           { spanned \ s _t -> SemicolonToken s          }
  "<"           { spanned \ s _t -> LessThanToken s           }
  "<="          { spanned \ s _t -> LessThanOrEqualToken s    }
  "<>"          { spanned \ s _t -> NotEqualToken s           }
  "="           { spanned \ s _t -> EqualToken s              }
  ">"           { spanned \ s _t -> GreaterThanToken s        }
  ">="          { spanned \ s _t -> GreaterThanOrEqualToken s }
  "?"           { spanned \ s _t -> QuestionToken s           }
  "@"           { spanned \ s _t -> AtToken s                 }
  "["           { spanned \ s _t -> LeftSquareBracketToken s  }
  "\\"          { spanned \ s _t -> BackslashToken s          }
  "]"           { spanned \ s _t -> RightSquareBracketToken s }
  "^"           { spanned \ s _t -> CaretToken s              }
  "_"           { spanned \ s _t -> UnderscoreToken s         }
  "{"           { spanned \ s _t -> LeftCurlyBraceToken s     }
  "|"           { spanned \ s _t -> PipeToken s               }
  "}"           { spanned \ s _t -> RightCurlyBraceToken s    }
  "~"           { spanned \ s _t -> TildeToken s              }

--------------------------------------------------------------------------------

{
}
