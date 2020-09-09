{

{-# LANGUAGE OverloadedStrings #-}

module Hap.Parser
  ( programParser
  , tokensParser
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Hap.Language
  ( Binding(..)
  , Expression(..)
  , Identifier(..)
  , Literal(..)
  , Program(..)
  , Signature(..)
  , Statement(..)
  , expressionAnno
  , signatureAnno
  , statementAnno
  )
import Hap.Parse (lexToken)
import Hap.ParserMonad (Parser, parseFailure)
import Hap.Token (Keyword(..), Token(..), SourceSpan(..), tokenAnno)
import Hap.Tokenizer ()
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

}

%name      programParser Program
%name      tokensParser Tokens
%tokentype { Token SourceSpan }
%monad     { Parser }
%lexer     { lexToken } { EofToken }
%error     { parseFailure }

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
  inKeyword       { KeywordToken (_, InKeyword)       }
  lastKeyword     { KeywordToken (_, LastKeyword)     }
  longKeyword     { KeywordToken (_, LongKeyword)     }
  needsKeyword    { KeywordToken (_, NeedsKeyword)    }
  nextKeyword     { KeywordToken (_, NextKeyword)     }
  nullKeyword     { KeywordToken (_, NullKeyword)     }
  onKeyword       { KeywordToken (_, OnKeyword)       }
  redoKeyword     { KeywordToken (_, RedoKeyword)     }
  removeKeyword   { KeywordToken (_, RemoveKeyword)   }
  returnKeyword   { KeywordToken (_, ReturnKeyword)   }
  setKeyword      { KeywordToken (_, SetKeyword)      }
  trueKeyword     { KeywordToken (_, TrueKeyword)     }
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
  : many(Statement) { Program $1 }

  Statement :: { Statement SourceSpan }
    : AtomicStatement     { $1 }
    | AfterStatement      { $1 }
    | AsLongAsStatement   { $1 }
    | BlockStatement      { $1 }
    | EmptyStatement      { $1 }
    | ForAllStatement     { $1 }
    | ForEachStatement    { $1 }
    | FunctionStatement   { $1 }
    | IfStatement         { $1 }
    | LastStatement       { $1 }
    | NextStatement       { $1 }
    | OnAddStatement      { $1 }
    | OnChangeStatement   { $1 }
    | OnRemoveStatement   { $1 }
    | OnSetStatement      { $1 }
    | RedoStatement       { $1 }
    | ReturnStatement     { $1 }
    | VarStatement        { $1 }
    | WhenStatement       { $1 }
    | WheneverStatement   { $1 }
    | WhileStatement      { $1 }
    -- TODO: Check whether this must appear last.
    | ExpressionStatement { $1 }

    AtomicStatement :: { Statement SourceSpan }
      : atomicKeyword Statement
      { atomicStatement $1 $2 }

    AfterStatement :: { Statement SourceSpan }
      : afterKeyword '(' Expression ')' Statement
      { afterStatement $1 $2 $3 $4 $5 }

    AsLongAsStatement
      : asKeyword longKeyword asKeyword '(' Expression ')' Statement
      { asLongAsStatement $1 $2 $3 $4 $5 $6 $7 }

    BlockStatement :: { Statement SourceSpan }
      : '{' many(Statement) '}'
      { blockStatement $1 $2 $3 }

    EmptyStatement :: { Statement SourceSpan }
      : ';'
      { emptyStatement $1 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    ForAllStatement :: { Statement SourceSpan }
      : forKeyword allKeyword '(' Identifier ':' Expression ')' Statement
      { forAllStatement $1 $2 $3 $4 $5 $6 $7 $8 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    ForEachStatement :: { Statement SourceSpan }
      : forKeyword eachKeyword '(' Identifier ':' Expression ')' Statement
      { forEachStatement $1 $2 $3 $4 $5 $6 $7 $8 }

    FunctionStatement :: { Statement SourceSpan }
      : functionKeyword Identifier ParameterList opt(TypeAnnotation) Statement
      { functionStatement $1 $2 $3 $4 $5 }

      -- TODO: Preserve source spans of parentheses and separators?
      ParameterList :: { [Binding SourceSpan] }
        : '(' sepEnd(',', Binding) ')'
        { $2 }

      -- See [Binding].

    IfStatement :: { Statement SourceSpan }
      : ifKeyword '(' Expression ')' Statement
        %prec noElseKeyword
        { ifStatement $1 $2 $3 $4 $5 Nothing }
      | ifKeyword '(' Expression ')' Statement ElseClause
        { ifStatement $1 $2 $3 $4 $5 (Just $6) }

      ElseClause :: { (SourceSpan, Statement SourceSpan) }
        : elseKeyword Statement
        { elseClause $1 $2 }

    LastStatement :: { Statement SourceSpan }
      : lastKeyword opt(Identifier) ';'
      { lastStatement $1 $2 $3 }

    NextStatement :: { Statement SourceSpan }
      : nextKeyword opt(Identifier) ';'
      { nextStatement $1 $2 $3 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    OnAddStatement :: { Statement SourceSpan }
      : onKeyword addKeyword '(' Identifier ':' Expression ')' Statement
      { onAddStatement $1 $2 $3 $4 $5 $6 $7 $8 }

    OnChangeStatement :: { Statement SourceSpan }
      : onKeyword changeKeyword '(' sepEnd1(',', Identifier) ')' Statement
      { onChangeStatement $1 $2 $3 $4 $5 $6 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    OnRemoveStatement :: { Statement SourceSpan }
      : onKeyword removeKeyword '(' Identifier ':' Expression ')' Statement
      { onRemoveStatement $1 $2 $3 $4 $5 $6 $7 $8 }

    OnSetStatement :: { Statement SourceSpan }
      : onKeyword setKeyword '(' sepEnd1(',', Identifier) ')' Statement
      { onSetStatement $1 $2 $3 $4 $5 $6 }

    RedoStatement :: { Statement SourceSpan }
      : redoKeyword opt(Identifier) ';'
      { redoStatement $1 $2 $3 }

    ReturnStatement :: { Statement SourceSpan }
      : returnKeyword opt(Expression) ';'
      { returnStatement $1 $2 $3 }

    VarStatement :: { Statement SourceSpan }
      : varKeyword sepEnd1(',', Binding) ';'
      { varStatement $1 $2 $3 }

      -- See [Binding].

    WhenStatement :: { Statement SourceSpan }
      : whenKeyword '(' Expression ')' Statement
      { whenStatement $1 $2 $3 $4 $5 }

    WheneverStatement :: { Statement SourceSpan }
      : wheneverKeyword '(' Expression ')' Statement
      { wheneverStatement $1 $2 $3 $4 $5 }

    WhileStatement :: { Statement SourceSpan }
      : whileKeyword '(' Expression ')' Statement
      { whileStatement $1 $2 $3 $4 $5 }

    ExpressionStatement :: { Statement SourceSpan }
      : Expression ';'
      { expressionStatement $1 $2 }

      -- See [Expression].

  Binding
    :: { Binding SourceSpan }
    : Identifier opt(TypeAnnotation) opt(Initializer)
    { binding $1 $2 $3 }

  -- TODO: Preserve source span of colon.
  TypeAnnotation
    :: { Signature SourceSpan }
    : ':' Signature { $2 }

  -- TODO: Preserve source span of equals.
  -- TODO: Decide on equals symbol for initializer.
  Initializer
    :: { Expression SourceSpan }
    : '=' Expression { $2 }

  Expression :: { Expression SourceSpan }
    : Boolean            { booleanExpression $1 }
    | '(' Expression ')' { groupExpression $1 $2 $3 }
    | Identifier         { identifierExpression $1 }
    | integer            { integerExpression $1 }
    | Null               { nullExpression $1 }

    Boolean :: { (SourceSpan, Bool) }
      : trueKeyword  { (tokenAnno $1, True) }
      | falseKeyword { (tokenAnno $1, False) }

    Identifier :: { (SourceSpan, Identifier) }
      : IdentifierStart many(IdentifierContinue)
      { identifierParts $1 $2 }

      IdentifierStart :: { (SourceSpan, Text) }
        : word { $1 }

      -- TODO: Allow numbers.
      IdentifierContinue :: { (SourceSpan, Text) }
        : word           { $1 }
        | ContextualWord { $1 }

    Null :: { SourceSpan }
      : nullKeyword  { tokenAnno $1 }

  -- TODO: Flesh out other types of signatures.
  Signature :: { Signature SourceSpan }
    : ConstructorSignature { $1 }

    ConstructorSignature :: { Signature SourceSpan }
      : Identifier { uncurry ConstructorSignature $1 }

-- Note [Name-Expression Separator in Quantifiers]:
--
-- Currently the grammar uses a colon (':') for the separator between names and
-- expressions in statements that “quantify” a variable over a block in some
-- way, such as ‘for each’ and ‘on add’. Using a symbol is nice for avoiding
-- ambiguity with identifiers by using a keyword such as ‘in’, but it’s
-- arbitrary and not grounded in any specific usability arguments apart from
-- precedent in some other languages.

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
inWord       : inKeyword       { (tokenAnno $1, "in")       }
lastWord     : lastKeyword     { (tokenAnno $1, "last")     }
longWord     : longKeyword     { (tokenAnno $1, "long")     }
needsWord    : needsKeyword    { (tokenAnno $1, "needs")    }
nextWord     : nextKeyword     { (tokenAnno $1, "next")     }
nullWord     : nullKeyword     { (tokenAnno $1, "null")     }
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

-- First of two.
fst(a, b)  -- :: { Parser a -> Parser b -> Parser a }
  : a b { $1 }

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

-- Zero or more, with separator between.
sep(s, p)  -- :: { Parser a -> Parser b -> Parser [b] }
  : opt(sep1(s, p)) { maybe [] NonEmpty.toList $1 }

-- One or more, with separator between.
sep1(s, p)  -- :: { Parser a -> Parser b -> Parser (NonEmpty b) }
  : p many(snd(s, p)) { $1 :| $2 }

-- Zero or more, with separator between and optionally after.
-- TODO: Preserve source span of trailing separator?
sepEnd(s, p)  -- :: { Parser a -> Parser b -> Parser [b] }
  : opt(sep1(s, p)) opt(s) { maybe [] NonEmpty.toList $1 }

-- One or more, with separator between and optionally after.
-- TODO: Preserve source span of trailing separator?
sepEnd1(s, p)  -- :: { Parser a -> Parser b -> Parser (NonEmpty b) }
  : sep1(s, p) opt(s) { $1 }

-- Second of two.
snd(a, b)  -- :: { Parser a -> Parser b -> Parser b }
  : a b { $2 }

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

afterStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Expression SourceSpan
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
afterStatement afterKeyword leftParenthesis condition rightParenthesis body
  = AfterStatement pos condition body
  where
    pos = mconcat
      [ tokenAnno afterKeyword
      , tokenAnno leftParenthesis
      , expressionAnno condition
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

asLongAsStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Token SourceSpan
  -> Token SourceSpan
  -> Expression SourceSpan
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
asLongAsStatement asKeyword longKeyword asKeyword' leftParenthesis condition
  rightParenthesis body
  = AsLongAsStatement pos condition body
  where
    pos = mconcat
      [ tokenAnno asKeyword
      , tokenAnno longKeyword
      , tokenAnno asKeyword'
      , tokenAnno leftParenthesis
      , expressionAnno condition
      , tokenAnno rightParenthesis
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

emptyStatement
  :: Token SourceSpan
  -> Statement SourceSpan
emptyStatement semicolon = EmptyStatement pos
  where
    pos = tokenAnno semicolon

forAllStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Token SourceSpan
  -> (SourceSpan, Identifier)
  -> Token SourceSpan
  -> Expression SourceSpan
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
forAllStatement forKeyword allKeyword leftParenthesis (variablePos, variable)
  colon container rightParenthesis body
  = ForAllStatement pos variable container body
  where
    pos = mconcat
      [ tokenAnno forKeyword
      , tokenAnno allKeyword
      , tokenAnno leftParenthesis
      , variablePos
      , tokenAnno colon
      , expressionAnno container
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

forEachStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Token SourceSpan
  -> (SourceSpan, Identifier)
  -> Token SourceSpan
  -> Expression SourceSpan
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
forEachStatement forKeyword eachKeyword leftParenthesis (variablePos, variable)
  colon container rightParenthesis body
  = ForEachStatement pos variable container body
  where
    pos = mconcat
      [ tokenAnno forKeyword
      , tokenAnno eachKeyword
      , tokenAnno leftParenthesis
      , variablePos
      , tokenAnno colon
      , expressionAnno container
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

functionStatement
  :: Token SourceSpan
  -> (SourceSpan, Identifier)
  -> [Binding SourceSpan]
  -> Maybe (Signature SourceSpan)
  -> Statement SourceSpan
  -> Statement SourceSpan
functionStatement functionKeyword (namePos, name) params returnType body
  = FunctionStatement pos name params returnType body
  where
    pos = mconcat
      [ tokenAnno functionKeyword
      , namePos
      , foldMap bindingAnno params
      , foldMap signatureAnno returnType
      , statementAnno body
      ]

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

lastStatement
  :: Token SourceSpan
  -> Maybe (SourceSpan, Identifier)
  -> Token SourceSpan
  -> Statement SourceSpan
lastStatement lastKeyword mIdentifier semicolon
  = LastStatement pos (snd <$> mIdentifier)
  where
    pos = mconcat
      [ tokenAnno lastKeyword
      , foldMap fst mIdentifier
      , tokenAnno semicolon
      ]

nextStatement
  :: Token SourceSpan
  -> Maybe (SourceSpan, Identifier)
  -> Token SourceSpan
  -> Statement SourceSpan
nextStatement nextKeyword mIdentifier semicolon
  = NextStatement pos (snd <$> mIdentifier)
  where
    pos = mconcat
      [ tokenAnno nextKeyword
      , foldMap fst mIdentifier
      , tokenAnno semicolon
      ]

onAddStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Token SourceSpan
  -> (SourceSpan, Identifier)
  -> Token SourceSpan
  -> Expression SourceSpan
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
onAddStatement onKeyword addKeyword leftParenthesis (variablePos, variable)
  colon container rightParenthesis body
  = OnAddStatement pos variable container body
  where
    pos = mconcat
      [ tokenAnno onKeyword
      , tokenAnno addKeyword
      , tokenAnno leftParenthesis
      , variablePos
      , tokenAnno colon
      , expressionAnno container
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

onChangeStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Token SourceSpan
  -> NonEmpty (SourceSpan, Identifier)
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
onChangeStatement onKeyword changeKeyword leftParenthesis variables
  rightParenthesis body
  = OnChangeStatement pos (snd <$> variables) body
  where
    pos = mconcat
      [ tokenAnno onKeyword
      , tokenAnno changeKeyword
      , tokenAnno leftParenthesis
      , foldMap fst variables
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

onRemoveStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Token SourceSpan
  -> (SourceSpan, Identifier)
  -> Token SourceSpan
  -> Expression SourceSpan
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
onRemoveStatement onKeyword removeKeyword leftParenthesis
  (variablePos, variable) colon container rightParenthesis body
  = OnRemoveStatement pos variable container body
  where
    pos = mconcat
      [ tokenAnno onKeyword
      , tokenAnno removeKeyword
      , tokenAnno leftParenthesis
      , variablePos
      , tokenAnno colon
      , expressionAnno container
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

onSetStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Token SourceSpan
  -> NonEmpty (SourceSpan, Identifier)
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
onSetStatement onKeyword setKeyword leftParenthesis variables
  rightParenthesis body
  = OnSetStatement pos (snd <$> variables) body
  where
    pos = mconcat
      [ tokenAnno onKeyword
      , tokenAnno setKeyword
      , tokenAnno leftParenthesis
      , foldMap fst variables
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

redoStatement
  :: Token SourceSpan
  -> Maybe (SourceSpan, Identifier)
  -> Token SourceSpan
  -> Statement SourceSpan
redoStatement redoKeyword mIdentifier semicolon
  = RedoStatement pos (snd <$> mIdentifier)
  where
    pos = mconcat
      [ tokenAnno redoKeyword
      , foldMap fst mIdentifier
      , tokenAnno semicolon
      ]

returnStatement
  :: Token SourceSpan
  -> Maybe (Expression SourceSpan)
  -> Token SourceSpan
  -> Statement SourceSpan
returnStatement returnKeyword mResult semicolon
  = ReturnStatement pos mResult
  where
    pos = mconcat
      [ tokenAnno returnKeyword
      , foldMap expressionAnno mResult
      , tokenAnno semicolon
      ]

varStatement
  :: Token SourceSpan
  -> NonEmpty (Binding SourceSpan)
  -> Token SourceSpan
  -> Statement SourceSpan
varStatement varKeyword bindings semicolon
  = VarStatement pos bindings
  where
    pos = mconcat
      [ tokenAnno varKeyword
      , foldMap bindingAnno bindings
      , tokenAnno semicolon
      ]

whenStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Expression SourceSpan
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
whenStatement whenKeyword
  leftParenthesis condition rightParenthesis body
  = WhenStatement pos condition body
  where
    pos = mconcat
      [ tokenAnno whenKeyword
      , tokenAnno leftParenthesis
      , expressionAnno condition
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

wheneverStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Expression SourceSpan
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
wheneverStatement wheneverKeyword
  leftParenthesis condition rightParenthesis body
  = WheneverStatement pos condition body
  where
    pos = mconcat
      [ tokenAnno wheneverKeyword
      , tokenAnno leftParenthesis
      , expressionAnno condition
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

whileStatement
  :: Token SourceSpan
  -> Token SourceSpan
  -> Expression SourceSpan
  -> Token SourceSpan
  -> Statement SourceSpan
  -> Statement SourceSpan
whileStatement whileKeyword
  leftParenthesis condition rightParenthesis body
  = WhileStatement pos condition body
  where
    pos = mconcat
      [ tokenAnno whileKeyword
      , tokenAnno leftParenthesis
      , expressionAnno condition
      , tokenAnno rightParenthesis
      , statementAnno body
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

binding
  :: (SourceSpan, Identifier)
  -> Maybe (Signature SourceSpan)
  -> Maybe (Expression SourceSpan)
  -> Binding SourceSpan
binding (namePos, name) mSignature mInitializer = Binding
  -- TODO: Differentiate source spans of just name vs. whole binding?
  { bindingAnno        = namePos
  , bindingName        = name
  , bindingSignature   = mSignature
  , bindingInitializer = mInitializer
  }

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

booleanExpression :: (SourceSpan, Bool) -> Expression SourceSpan
booleanExpression (pos, value) = LiteralExpression pos (BooleanLiteral value)

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

identifierExpression :: (SourceSpan, Identifier) -> Expression SourceSpan
identifierExpression (pos, identifier) = IdentifierExpression pos identifier

identifierParts
  :: (SourceSpan, Text) -> [(SourceSpan, Text)] -> (SourceSpan, Identifier)
identifierParts
  (startSpan, startPart)
  (unzip -> (continueSpans, continueParts))
  =
  ( sconcat (startSpan :| continueSpans)
  , Identifier (startPart :| continueParts)
  )

integerExpression :: (SourceSpan, Integer) -> Expression SourceSpan
integerExpression (pos, value) = LiteralExpression pos (IntegerLiteral value)

nullExpression :: SourceSpan -> Expression SourceSpan
nullExpression pos = LiteralExpression pos NullLiteral

}
