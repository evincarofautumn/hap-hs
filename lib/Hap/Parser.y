{

{-# LANGUAGE OverloadedStrings #-}

module Hap.Parser where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Hap.Language
import Hap.Parse
import Hap.ParserMonad
import Hap.Token
import Hap.Tokenizer
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

}

%name      programParser Program
%name      tokensParser Tokens
%tokentype { Token SourceSpan }
%monad     { Parser } -- { thenP } { returnP }
%lexer     { hapLexToken } { EofToken }
-- %error { errorP }

%token

--------------------------------------------------------------------------------
-- Primitive Tokens
--------------------------------------------------------------------------------

  word     { WordToken   $$ }
  integer  { DigitsToken $$ }

--------------------------------------------------------------------------------
-- Symbol Tokens
--------------------------------------------------------------------------------

  '('      { LeftParenthesisToken    _ }
  ')'      { RightParenthesisToken   _ }
  '!'      { BangToken               _ }
  '#'      { NumberToken             _ }
  '%'      { PercentToken            _ }
  '&'      { AndToken                _ }
  '*'      { StarToken               _ }
  '+'      { PlusToken               _ }
  ','      { CommaToken              _ }
  '-'      { MinusToken              _ }
  '.'      { DotToken                _ }
  '/'      { SlashToken              _ }
  ':'      { ColonToken              _ }
  ';'      { SemicolonToken          _ }
  '<'      { LessThanToken           _ }
  '<='     { LessThanOrEqualToken    _ }
  '<>'     { NotEqualToken           _ }
  '='      { EqualToken              _ }
  '>'      { GreaterThanToken        _ }
  '>='     { GreaterThanOrEqualToken _ }
  '?'      { QuestionToken           _ }
  '@'      { AtToken                 _ }
  '['      { LeftSquareBracketToken  _ }
  '\\'     { BackslashToken          _ }
  ']'      { RightSquareBracketToken _ }
  '^'      { CaretToken              _ }
  '{'      { LeftCurlyBraceToken     _ }
  '|'      { PipeToken               _ }
  '}'      { RightCurlyBraceToken    _ }
  '~'      { TildeToken              _ }

--------------------------------------------------------------------------------
-- Keyword Tokens
--------------------------------------------------------------------------------

  addKeyword      { KeywordToken (_, AddKeyword)      }
  afterKeyword    { KeywordToken (_, AfterKeyword)    }
  allKeyword      { KeywordToken (_, AllKeyword)      }
  asKeyword       { KeywordToken (_, AsKeyword)       }
  asyncKeyword    { KeywordToken (_, AsyncKeyword)    }
  atomicKeyword   { KeywordToken (_, AtomicKeyword)   }
  beforeKeyword   { KeywordToken (_, BeforeKeyword)   }
  changeKeyword   { KeywordToken (_, ChangeKeyword)   }
  eachKeyword     { KeywordToken (_, EachKeyword)     }
  elseKeyword     { KeywordToken (_, ElseKeyword)     }
  entityKeyword   { KeywordToken (_, EntityKeyword)   }
  everyKeyword    { KeywordToken (_, EveryKeyword)    }
  falseKeyword    { KeywordToken (_, FalseKeyword)    }
  forKeyword      { KeywordToken (_, ForKeyword)      }
  functionKeyword { KeywordToken (_, FunctionKeyword) }
  hasKeyword      { KeywordToken (_, HasKeyword)      }
  ifKeyword       { KeywordToken (_, IfKeyword)       }
  lastKeyword     { KeywordToken (_, LastKeyword)     }
  longKeyword     { KeywordToken (_, LongKeyword)     }
  needsKeyword    { KeywordToken (_, NeedsKeyword)    }
  nextKeyword     { KeywordToken (_, NextKeyword)     }
  onKeyword       { KeywordToken (_, OnKeyword)       }
  redoKeyword     { KeywordToken (_, RedoKeyword)     }
  removeKeyword   { KeywordToken (_, RemoveKeyword)   }
  returnKeyword   { KeywordToken (_, ReturnKeyword)   }
  setKeyword      { KeywordToken (_, SetKeyword)      }
  trueKeyword     { KeywordToken (_, TrueKeyword)      }
  untilKeyword    { KeywordToken (_, UntilKeyword)    }
  varKeyword      { KeywordToken (_, VarKeyword)      }
  whenKeyword     { KeywordToken (_, WhenKeyword)     }
  wheneverKeyword { KeywordToken (_, WheneverKeyword) }
  whereKeyword    { KeywordToken (_, WhereKeyword)    }
  whichKeyword    { KeywordToken (_, WhichKeyword)    }
  whileKeyword    { KeywordToken (_, WhileKeyword)    }

--------------------------------------------------------------------------------
-- Any Token (MUST BE LAST)
--------------------------------------------------------------------------------

  token { $$ }

--------------------------------------------------------------------------------
-- Precedences
--------------------------------------------------------------------------------

%nonassoc noElseKeyword
%nonassoc elseKeyword

%%

--------------------------------------------------------------------------------
-- Grammar
--------------------------------------------------------------------------------

-- Parse just a list of tokens, for debugging.
Tokens :: { [Token SourceSpan] }
  : many(token) { $1 }

-- Main entry point.
Program :: { Program SourceSpan }
  : Statement { Program [$1] }

Statement :: { Statement SourceSpan }
  : atomicKeyword Statement { atomicStatement $1 $2 }
  | IfStatement             { $1 }
  | '{' many(Statement) '}' { blockStatement $1 $2 $3 }
  | Expression ';'          { expressionStatement $1 $2 }
  | ';'                     { emptyStatement $1 }

IfStatement :: { Statement SourceSpan }
  : ifKeyword '(' Expression ')' Statement
    %prec noElseKeyword
    { ifStatement $1 $2 $3 $4 $5 Nothing }
  | ifKeyword '(' Expression ')' Statement ElseClause
    { ifStatement $1 $2 $3 $4 $5 (Just $6) }

ElseClause :: { (SourceSpan, Statement SourceSpan) }
  : elseKeyword Statement { elseClause $1 $2 }

Expression :: { Expression SourceSpan }
  : integer            { integerExpression $1 }
  | Boolean            { booleanExpression $1 }
  | '(' Expression ')' { groupExpression $1 $2 $3 }
  | Identifier         { identifierExpression $1 }

Boolean :: { (SourceSpan, Bool) }
  : trueKeyword  { (tokenAnno $1, True) }
  | falseKeyword { (tokenAnno $1, False) }

Identifier :: { (SourceSpan, Identifier) }
  : IdentifierStart many(IdentifierContinue) { identifierParts $1 $2 }

IdentifierStart :: { (SourceSpan, Text) }
  : word { $1 }

-- TODO: Allow numbers.
IdentifierContinue :: { (SourceSpan, Text) }
  : word           { $1 }
  | ContextualWord { $1 }

--------------------------------------------------------------------------------
-- Contextual Keywords
--------------------------------------------------------------------------------

-- A keyword contextually interpreted as a name part.
ContextualWord :: { (SourceSpan, Text) }
  : addWord      { $1 }
  | afterWord    { $1 }
  | allWord      { $1 }
  | asWord       { $1 }
  | asyncWord    { $1 }
  | atomicWord   { $1 }
  | beforeWord   { $1 }
  | changeWord   { $1 }
  | eachWord     { $1 }
  | elseWord     { $1 }
  | entityWord   { $1 }
  | everyWord    { $1 }
  | falseWord    { $1 }
  | forWord      { $1 }
  | functionWord { $1 }
  | hasWord      { $1 }
  | ifWord       { $1 }
  | lastWord     { $1 }
  | longWord     { $1 }
  | needsWord    { $1 }
  | nextWord     { $1 }
  | onWord       { $1 }
  | redoWord     { $1 }
  | removeWord   { $1 }
  | returnWord   { $1 }
  | setWord      { $1 }
  | trueWord     { $1 }
  | untilWord    { $1 }
  | varWord      { $1 }
  | whenWord     { $1 }
  | wheneverWord { $1 }
  | whereWord    { $1 }
  | whichWord    { $1 }
  | whileWord    { $1 }

addWord      : addKeyword      { (tokenAnno $1, "add")      }
afterWord    : afterKeyword    { (tokenAnno $1, "after")    }
allWord      : allKeyword      { (tokenAnno $1, "all")      }
asWord       : asKeyword       { (tokenAnno $1, "as")       }
asyncWord    : asyncKeyword    { (tokenAnno $1, "async")    }
atomicWord   : atomicKeyword   { (tokenAnno $1, "atomic")   }
beforeWord   : beforeKeyword   { (tokenAnno $1, "before")   }
changeWord   : changeKeyword   { (tokenAnno $1, "change")   }
eachWord     : eachKeyword     { (tokenAnno $1, "each")     }
elseWord     : elseKeyword     { (tokenAnno $1, "else")     }
entityWord   : entityKeyword   { (tokenAnno $1, "entity")   }
everyWord    : everyKeyword    { (tokenAnno $1, "every")    }
falseWord    : falseKeyword    { (tokenAnno $1, "false")    }
forWord      : forKeyword      { (tokenAnno $1, "for")      }
functionWord : functionKeyword { (tokenAnno $1, "function") }
hasWord      : hasKeyword      { (tokenAnno $1, "has")      }
ifWord       : ifKeyword       { (tokenAnno $1, "if")       }
lastWord     : lastKeyword     { (tokenAnno $1, "last")     }
longWord     : longKeyword     { (tokenAnno $1, "long")     }
needsWord    : needsKeyword    { (tokenAnno $1, "needs")    }
nextWord     : nextKeyword     { (tokenAnno $1, "next")     }
onWord       : onKeyword       { (tokenAnno $1, "on")       }
redoWord     : redoKeyword     { (tokenAnno $1, "redo")     }
removeWord   : removeKeyword   { (tokenAnno $1, "remove")   }
returnWord   : returnKeyword   { (tokenAnno $1, "return")   }
setWord      : setKeyword      { (tokenAnno $1, "set")      }
trueWord     : trueKeyword     { (tokenAnno $1, "true")     }
untilWord    : untilKeyword    { (tokenAnno $1, "until")    }
varWord      : varKeyword      { (tokenAnno $1, "var")      }
whenWord     : whenKeyword     { (tokenAnno $1, "when")     }
wheneverWord : wheneverKeyword { (tokenAnno $1, "whenever") }
whereWord    : whereKeyword    { (tokenAnno $1, "where")    }
whichWord    : whichKeyword    { (tokenAnno $1, "which")    }
whileWord    : whileKeyword    { (tokenAnno $1, "while")    }

--------------------------------------------------------------------------------
-- Grammar Utilities
--------------------------------------------------------------------------------

-- Zero or more.
many(p)  -- :: { Parser a -> Parser [a] }
  : manyR(p) { reverse $1 }

-- Zero or more, reversed.
manyR(p)  -- :: { Parser a -> Parser [a] }
  : manyR(p) p { $2 : $1 }
  |            { [] }

-- Zero or one.
opt(p)  -- :: { Parser a -> Parser (Maybe a) }
  : p { Just $1 }
  |   { Nothing }

-- One or more.
some(p)  -- :: { Parser a -> Parser (NonEmpty a) }
  : someR(p) { NonEmpty.reverse $1 }

-- One or more, reversed.
someR(p)  -- :: { Parser a -> Parser (NonEmpty a) }
  : someR(p) p { NonEmpty.cons $2 $1 }
  | p          { $1 :| [] }

{

--------------------------------------------------------------------------------
-- Statement
--------------------------------------------------------------------------------

atomicStatement
  :: Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
atomicStatement atomicKeyword body = AtomicStatement pos body
  where
    pos = mconcat
      [ tokenAnno atomicKeyword
      , statementAnno body
      ]

blockStatement
  :: Token SourceSpan
  -> [Statement SourceSpan]
  -> Token SourceSpan
  -> Statement SourceSpan
blockStatement openBrace statements closeBrace = BlockStatement pos statements
  where
    pos = mconcat $ concat
      [ [tokenAnno openBrace]
      , fmap statementAnno statements
      , [tokenAnno closeBrace]
      ]

expressionStatement
  :: Expression SourceSpan
  -> Token SourceSpan
  -> Statement SourceSpan
expressionStatement body semicolon = ExpressionStatement pos body
  where
    pos = mconcat
      [ expressionAnno body
      , tokenAnno semicolon
      ]

emptyStatement
  :: Token SourceSpan
  -> Statement SourceSpan
emptyStatement semicolon = EmptyStatement pos
  where
    pos = tokenAnno semicolon

ifStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Expression SourceSpan
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Maybe (SourceSpan, Statement SourceSpan)
  -> Statement SourceSpan
ifStatement ifKeyword
  leftParenthesis condition rightParenthesis
  thenClause elseClause
  = IfStatement pos condition thenClause (snd <$> elseClause)
  where
    pos = mconcat
      [ tokenAnno ifKeyword
      , tokenAnno leftParenthesis
      , expressionAnno condition
      , tokenAnno rightParenthesis
      , statementAnno thenClause
      , foldMap fst elseClause
      ]

elseClause
  :: Token SourceSpan
  -> Statement SourceSpan
  -> (SourceSpan, Statement SourceSpan)
elseClause elseKeyword body = (pos, body)
  where
    pos = mconcat
      [ tokenAnno elseKeyword
      , statementAnno body
      ]

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

groupExpression
  :: Token SourceSpan
  -> Expression SourceSpan
  -> Token SourceSpan
  -> Expression SourceSpan
groupExpression leftParenthesis body rightParenthesis
  = GroupExpression pos body
  where
    pos = mconcat
      [ tokenAnno leftParenthesis
      , expressionAnno body
      , tokenAnno rightParenthesis
      ]

identifierExpression
  :: (SourceSpan, Identifier)
  -> Expression SourceSpan
identifierExpression (pos, identifier) = IdentifierExpression pos identifier

identifierParts
  :: (SourceSpan, Text)
  -> [(SourceSpan, Text)]
  -> (SourceSpan, Identifier)
identifierParts
  (startSpan, startPart)
  (unzip -> (continueSpans, continueParts))
  =
  ( sconcat (startSpan :| continueSpans)
  , Identifier (startPart :| continueParts)
  )

integerExpression
  :: (SourceSpan, Integer)
  -> Expression SourceSpan
integerExpression (pos, value) = LiteralExpression pos (IntegerLiteral value)

booleanExpression
  :: (SourceSpan, Bool)
  -> Expression SourceSpan
booleanExpression (pos, value) = LiteralExpression pos (BooleanLiteral value)

}
