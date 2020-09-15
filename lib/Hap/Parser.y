{

{-# LANGUAGE OverloadedStrings #-}

module Hap.Parser
  ( programParser
  , tokensParser
  ) where

import Control.Arrow ((***))
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Hap.Language
  ( BinaryOperator(..)
  , Identifier(..)
  , UnaryOperator(..)
  , bindingAnno
  , decimalDigitString
  , decimalFractionAnno
  , decimalIntegerPartAnno
  , decimalIntegerParts
  , expressionAnno
  , signatureAnno
  , spliceAnno
  , statementAnno
  )
import Hap.Parse (lexToken)
import Hap.ParserMonad (Parser, parseFailure)
import Hap.Token
  ( DecimalDigit
  , Keyword(..)
  , Quote(..)
  , SourceSpan(..)
  , tokenAnno
  )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Hap.Language as Language
import qualified Hap.Token as Token

}

%name             programParser Program
%name             tokensParser Tokens
%tokentype        { Token }
%monad            { Parser }
%lexer            { lexToken } { Token.EofToken }
%error            { parseFailure }
%errorhandlertype explist

%token

--------------------------------------------------------------------------------
-- Primitive Tokens
--------------------------------------------------------------------------------

  word        { Token.WordToken   $$                                          }
  digits      { Token.DigitsToken (uncurry Language.DecimalIntegerPart -> $$) }
  text        { Token.TextToken   DoubleQuote DoubleQuote $$                  }
  left_text   { Token.TextToken   DoubleQuote SingleQuote $$                  }
  right_text  { Token.TextToken   SingleQuote DoubleQuote $$                  }
  middle_text { Token.TextToken   SingleQuote SingleQuote $$                  }

--------------------------------------------------------------------------------
-- Symbol Tokens
--------------------------------------------------------------------------------

  '('  { Token.LeftParenthesisToken    _ }
  ')'  { Token.RightParenthesisToken   _ }
  '!'  { Token.BangToken               _ }
  '#'  { Token.NumberToken             _ }
  '%'  { Token.PercentToken            _ }
  '&'  { Token.AndToken                _ }
  '*'  { Token.StarToken               _ }
  '+'  { Token.PlusToken               _ }
  ','  { Token.CommaToken              _ }
  '-'  { Token.MinusToken              _ }
  '->' { Token.RightArrowToken         _ }
  '=>' { Token.RightDoubleArrowToken   _ }
  '.'  { Token.DotToken                _ }
  '/'  { Token.SlashToken              _ }
  ':'  { Token.ColonToken              _ }
  ':=' { Token.ColonEqualToken         _ }
  ';'  { Token.SemicolonToken          _ }
  '<'  { Token.LessThanToken           _ }
  '<=' { Token.LessThanOrEqualToken    _ }
  '<>' { Token.NotEqualToken           _ }
  '='  { Token.EqualToken              _ }
  '>'  { Token.GreaterThanToken        _ }
  '>=' { Token.GreaterThanOrEqualToken _ }
  '?'  { Token.QuestionToken           _ }
  '@'  { Token.AtToken                 _ }
  '['  { Token.LeftSquareBracketToken  _ }
  '\\' { Token.BackslashToken          _ }
  ']'  { Token.RightSquareBracketToken _ }
  '^'  { Token.CaretToken              _ }
  '_'  { Token.UnderscoreToken         _ }
  '{'  { Token.LeftCurlyBraceToken     _ }
  '|'  { Token.PipeToken               _ }
  '}'  { Token.RightCurlyBraceToken    _ }
  '~'  { Token.TildeToken              _ }

--------------------------------------------------------------------------------
-- Keyword Tokens
--------------------------------------------------------------------------------

  -- Primary keywords, which may not start a name.
  after    { Token.KeywordToken (_, AfterKeyword)    }
  as       { Token.KeywordToken (_, AsKeyword)       }
  async    { Token.KeywordToken (_, AsyncKeyword)    }
  atomic   { Token.KeywordToken (_, AtomicKeyword)   }
  before   { Token.KeywordToken (_, BeforeKeyword)   }
  entity   { Token.KeywordToken (_, EntityKeyword)   }
  for      { Token.KeywordToken (_, ForKeyword)      }
  function { Token.KeywordToken (_, FunctionKeyword) }
  if       { Token.KeywordToken (_, IfKeyword)       }
  last     { Token.KeywordToken (_, LastKeyword)     }
  next     { Token.KeywordToken (_, NextKeyword)     }
  on       { Token.KeywordToken (_, OnKeyword)       }
  redo     { Token.KeywordToken (_, RedoKeyword)     }
  return   { Token.KeywordToken (_, ReturnKeyword)   }
  until    { Token.KeywordToken (_, UntilKeyword)    }
  var      { Token.KeywordToken (_, VarKeyword)      }
  when     { Token.KeywordToken (_, WhenKeyword)     }
  whenever { Token.KeywordToken (_, WheneverKeyword) }
  while    { Token.KeywordToken (_, WhileKeyword)    }

  -- Secondary keywords, which may start a name.
  add      { Token.KeywordToken (_, AddKeyword)      }
  all      { Token.KeywordToken (_, AllKeyword)      }
  change   { Token.KeywordToken (_, ChangeKeyword)   }
  each     { Token.KeywordToken (_, EachKeyword)     }
  else     { Token.KeywordToken (_, ElseKeyword)     }
  has      { Token.KeywordToken (_, HasKeyword)      }
  long     { Token.KeywordToken (_, LongKeyword)     }
  needs    { Token.KeywordToken (_, NeedsKeyword)    }
  remove   { Token.KeywordToken (_, RemoveKeyword)   }
  set      { Token.KeywordToken (_, SetKeyword)      }

--------------------------------------------------------------------------------
-- Any Token (MUST BE LAST)
--------------------------------------------------------------------------------

  token { $$ }

--------------------------------------------------------------------------------
-- Precedences
--------------------------------------------------------------------------------

%nonassoc NO_ELSE
%nonassoc else

%right ':='
%right '->'
%right '|'
%right '&'
-- TODO: Chained relations.
%nonassoc '<' '>=' '>' '<=' '=' '<>' ':'
%left '+' '-'
%left '*' '/'
%right UNARY_PREFIX
%left UNARY_POSTFIX

%%

--------------------------------------------------------------------------------
-- Grammar
--------------------------------------------------------------------------------

-- Parse just a list of tokens, for debugging.
Tokens :: { [Token] }
  : many(token) { $1 }

-- Main entry point.
Program :: { Program }
  : many(Statement) { Language.Program $1 }

  Statement :: { Statement }
    : AtomicStatement     { $1 }
    | AfterStatement      { $1 }
    | AsLongAsStatement   { $1 }
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

    AtomicStatement :: { Statement }
      : atomic Block
      { atomicStatement $1 $2 }

    AfterStatement :: { Statement }
      : after '(' Expression ')' Block
      { afterStatement $1 $2 $3 $4 $5 }

    AsLongAsStatement
      : as long as '(' Expression ')' Block
      { asLongAsStatement $1 $2 $3 $4 $5 $6 $7 }

    EmptyStatement :: { Statement }
      : ';'
      { emptyStatement $1 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    ForAllStatement :: { Statement }
      : for all '(' Identifier ':' Expression ')' Block
      { forAllStatement $1 $2 $3 $4 $5 $6 $7 $8 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    ForEachStatement :: { Statement }
      : for each '(' Identifier ':' Expression ')' Block
      { forEachStatement $1 $2 $3 $4 $5 $6 $7 $8 }

    FunctionStatement :: { Statement }
      : function Identifier ParameterList opt(TypeAnnotation)
        '{' many(Statement) '}'
      { functionStatement $1 $2 $3 $4 $5 (Right $6) $7 }
      | function Identifier ParameterList opt(TypeAnnotation)
        '->' Expression ';'
      { functionStatement $1 $2 $3 $4 $5 (Left $6) $7 }

      -- TODO: Preserve source spans of parentheses and separators?
      ParameterList :: { [Binding] }
        : '(' sepEnd(',', Binding) ')'
        { $2 }

      -- See [Binding].

    IfStatement :: { Statement }
      : if '(' Expression ')' Block
        %prec NO_ELSE
        { ifStatement $1 $2 $3 $4 $5 Nothing }
      | if '(' Expression ')' Block ElseClause
        { ifStatement $1 $2 $3 $4 $5 (Just $6) }

      ElseClause :: { (SourceSpan, Statement) }
        : else Block       { elseClause $1 $2 }
        | else IfStatement { elseClause $1 $2 }

    LastStatement :: { Statement }
      : last opt(Identifier) ';'
      { lastStatement $1 $2 $3 }

    NextStatement :: { Statement }
      : next opt(Identifier) ';'
      { nextStatement $1 $2 $3 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    OnAddStatement :: { Statement }
      : on add '(' Identifier ':' Expression ')' Block
      { onAddStatement $1 $2 $3 $4 $5 $6 $7 $8 }

    OnChangeStatement :: { Statement }
      : on change '(' sepEnd1(',', Identifier) ')' Block
      { onChangeStatement $1 $2 $3 $4 $5 $6 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    OnRemoveStatement :: { Statement }
      : on remove '(' Identifier ':' Expression ')' Block
      { onRemoveStatement $1 $2 $3 $4 $5 $6 $7 $8 }

    OnSetStatement :: { Statement }
      : on set '(' sepEnd1(',', Identifier) ')' Block
      { onSetStatement $1 $2 $3 $4 $5 $6 }

    RedoStatement :: { Statement }
      : redo opt(Identifier) ';'
      { redoStatement $1 $2 $3 }

    ReturnStatement :: { Statement }
      : return opt(Expression) ';'
      { returnStatement $1 $2 $3 }

    VarStatement :: { Statement }
      : var sepEnd1(',', Binding) ';'
      { varStatement $1 $2 $3 }

      -- See [Binding].

    WhenStatement :: { Statement }
      : when '(' Expression ')' Block
      { whenStatement $1 $2 $3 $4 $5 }

    WheneverStatement :: { Statement }
      : whenever '(' Expression ')' Block
      { wheneverStatement $1 $2 $3 $4 $5 }

    WhileStatement :: { Statement }
      : while '(' Expression ')' Block
      { whileStatement $1 $2 $3 $4 $5 }

    ExpressionStatement :: { Statement }
      : Expression ';'
      { expressionStatement $1 $2 }

      -- See [Expression].

    Block :: { Statement }
      : '{' many(Statement) '}' { blockStatement $1 $2 $3 }

  Binding
    :: { Binding }
    : Identifier opt(TypeAnnotation) opt(Initializer)
    { binding $1 $2 $3 }

  -- TODO: Preserve source span of colon.
  TypeAnnotation
    :: { Signature }
    : ':' Signature { $2 }

  -- TODO: Preserve source span of equals.
  -- TODO: Decide on equals symbol for initializer.
  Initializer
    :: { Expression }
    : '=' Expression { $2 }

  Expression :: { Expression }
    : Term   { $1 }
    | Unary  { $1 }
    | Binary { $1 }

    Unary :: { Expression }
      : '+'  Expression %prec UNARY_PREFIX { unaryOp UnaryPlus       $1 $2 }
      | '-'  Expression %prec UNARY_PREFIX { unaryOp UnaryMinus      $1 $2 }
      | '<'  Expression %prec UNARY_PREFIX { unaryOp UnaryLess       $1 $2 }
      | '<=' Expression %prec UNARY_PREFIX { unaryOp UnaryNotGreater $1 $2 }
      | '<>' Expression %prec UNARY_PREFIX { unaryOp UnaryNotEqual   $1 $2 }
      | '='  Expression %prec UNARY_PREFIX { unaryOp UnaryEqual      $1 $2 }
      | '>'  Expression %prec UNARY_PREFIX { unaryOp UnaryGreater    $1 $2 }
      | '>=' Expression %prec UNARY_PREFIX { unaryOp UnaryNotLess    $1 $2 }
      | '~'  Expression %prec UNARY_PREFIX { unaryOp UnaryNot        $1 $2 }

    Binary :: { Expression }
      : Expression '*'  Expression { binaryOp BinaryMultiply   $1 $2 $3 }
      | Expression '/'  Expression { binaryOp BinaryDivide     $1 $2 $3 }
      -- TODO: Modulus as context, not operation?
      | Expression '+'  Expression { binaryOp BinaryAdd        $1 $2 $3 }
      | Expression '-'  Expression { binaryOp BinarySubtract   $1 $2 $3 }
      | Expression '<'  Expression { binaryOp BinaryLess       $1 $2 $3 }
      | Expression '>=' Expression { binaryOp BinaryNotLess    $1 $2 $3 }
      | Expression '>'  Expression { binaryOp BinaryGreater    $1 $2 $3 }
      | Expression '<=' Expression { binaryOp BinaryNotGreater $1 $2 $3 }
      | Expression '='  Expression { binaryOp BinaryEqual      $1 $2 $3 }
      | Expression '<>' Expression { binaryOp BinaryNotEqual   $1 $2 $3 }
      | Expression ':'  Expression { binaryOp BinaryElement    $1 $2 $3 }
      | Expression '&'  Expression { binaryOp BinaryAnd        $1 $2 $3 }
      | Expression '|'  Expression { binaryOp BinaryOr         $1 $2 $3 }
      | Expression '->' Expression { binaryOp BinaryImplies    $1 $2 $3 }
      -- TODO: Sort out equality operators.
      | Expression ':=' Expression { binaryOp BinaryAssign     $1 $2 $3 }

    -- TODO: Preserve source spans of parentheses and separators?
    CallSuffix :: { [Expression] }
      : '(' sepEnd(',', Expression) ')' { $2 }

    Term :: { Expression }
      : Identifier         { identifierExpression $1 }
      | List               { $1 }
      | Map                { $1 }
      | Number             { $1 }
      | Set                { $1 }
      | Text               { $1 }
      | Term CallSuffix    { callExpression $1 $2 }
      | '(' Expression ')' { groupExpression $1 $2 $3 }

      Identifier :: { (SourceSpan, Identifier) }
        : IdentifierStart many(IdentifierContinue)
        { identifierParts $1 $2 }

        IdentifierStart :: { (SourceSpan, Text) }
          : word             { $1 }
          | SecondaryKeyword { $1 }

        IdentifierContinue :: { (SourceSpan, Text) }
          : word              { $1 }
          | PrimaryKeyword    { $1 }
          | SecondaryKeyword  { $1 }
          | digits            { identifierContinueDigits $1 }

      -- TODO: Preserve source spans of separators?
      List :: { Expression }
        : '[' sepEnd(',', Expression) ']' { listExpression $1 $2 $3 }

      -- Note that 'Map' uses 'sepEnd1' so that '{}' is parsed unambiguously as
      -- an empty set rather than an empty map.
      --
      -- TODO: Preserve source spans of separators?
      -- TODO: Make map a set of key-value pairs with the '=>' kvp operator?
      Map :: { Expression }
        : '{' sepEnd1(',', KeyValuePair) '}' { mapExpression $1 $2 $3 }

        KeyValuePair :: { (Expression, Expression) }
          : Key '=>' Expression { ($1, $3) }

          Key :: { Expression }
            : Identifier         { identifierKey $1 }
            | Text               { $1 }
            -- TODO: Preserve source spans of parentheses.
            | '(' Expression ')' { $2 }

      Number :: { Expression }
        -- See note [Float Exponents].
        : opt(Integer) Fraction { floatExpression $1 $2 }
        | Integer               { integerExpression $1 }

        Integer :: { DecimalInteger }
          : some(digits) { Language.DecimalInteger $1 }

        Fraction :: { DecimalFraction }
          : '.' some(digits) { floatFraction $1 $2 }

        Sign :: { Sign }
          : '+' { Language.Plus (tokenAnno $1) }
          | '-' { Language.Minus (tokenAnno $1) }

      Set :: { Expression }
        : '{' sepEnd(',', Expression) '}' { setExpression $1 $2 $3 }

      Text :: { Expression }
        : text                         { textExpression $1 }
        | left_text Splices right_text { spliceExpression $1 $2 $3 }

        Splices :: { [Splice] }
          : Expression many(pair(middle_text, Expression)) { textSplices $1 $2 }

  -- TODO: Flesh out other types of signatures.
  Signature :: { Signature }
    : ConstructorSignature { $1 }

    ConstructorSignature :: { Signature }
      : Identifier { uncurry Language.ConstructorSignature $1 }

-- Note [Name-Expression Separator in Quantifiers]:
--
-- Currently the grammar uses a colon (':') for the separator between names and
-- expressions in statements that “quantify” a variable over a block in some
-- way, such as ‘for each’ and ‘on add’. Using a symbol is nice for avoiding
-- ambiguity with identifiers by using a keyword such as ‘in’, but it’s
-- arbitrary and not grounded in any specific usability arguments apart from
-- precedent in some other languages.

-- Note [Float Exponents]:
--
-- Most mainstream languages, especially of C style, support exponential
-- “scientific” notation for floating-point literals. Adding this to Hap would
-- complicate the lexer, going against the design principle of avoiding
-- “sublanguages” as much as possible. It also doesn’t seem necessary to allow
-- ‘2e+6’ when this can already be written with digit separators as ‘2 000 000’
-- or as an expression ‘2*10^6’. The ‘e’ notation is unfamiliar to many people
-- who haven’t programmed before, and in scientific contexts, it’s more commonly
-- typeset differently, with a small capital ᴇ (U+1D07) / ⏨ (U+23E8).

--------------------------------------------------------------------------------
-- Contextual Keywords
--------------------------------------------------------------------------------

-- A keyword that may appear within a name after the first part.
PrimaryKeyword :: { (SourceSpan, Text) }
  : after    { (tokenAnno $1, "after")    }
  | as       { (tokenAnno $1, "as")       }
  | async    { (tokenAnno $1, "async")    }
  | atomic   { (tokenAnno $1, "atomic")   }
  | before   { (tokenAnno $1, "before")   }
  | entity   { (tokenAnno $1, "entity")   }
  | for      { (tokenAnno $1, "for")      }
  | function { (tokenAnno $1, "function") }
  | if       { (tokenAnno $1, "if")       }
  | last     { (tokenAnno $1, "last")     }
  | next     { (tokenAnno $1, "next")     }
  | on       { (tokenAnno $1, "on")       }
  | redo     { (tokenAnno $1, "redo")     }
  | return   { (tokenAnno $1, "return")   }
  | until    { (tokenAnno $1, "until")    }
  | var      { (tokenAnno $1, "var")      }
  | when     { (tokenAnno $1, "when")     }
  | whenever { (tokenAnno $1, "whenever") }
  | while    { (tokenAnno $1, "while")    }

-- A keyword that may appear as the first part in a name.
SecondaryKeyword :: { (SourceSpan, Text) }
  : add    { (tokenAnno $1, "add")    }
  | all    { (tokenAnno $1, "all")    }
  | change { (tokenAnno $1, "change") }
  | each   { (tokenAnno $1, "each")   }
  | else   { (tokenAnno $1, "else")   }
  | has    { (tokenAnno $1, "has")    }
  | long   { (tokenAnno $1, "long")   }
  | needs  { (tokenAnno $1, "needs")  }
  | remove { (tokenAnno $1, "remove") }
  | set    { (tokenAnno $1, "set")    }

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

-- Pair of both.
pair(a, b)  -- :: { Parser a -> Parser b -> Parser (a, b) }
  : a b { ($1, $2) }

-- Zero or more, right-recursive.
rmany(p)  -- :: { Parser a -> Parser [a] }
  : p rmany(p) { $1 : $2 }
  |            { [] }

-- Zero or more, with separator between.
sep(s, p)  -- :: { Parser a -> Parser b -> Parser [b] }
  : opt(sep1(s, p)) { maybe [] NonEmpty.toList $1 }

-- One or more, with separator between.
sep1(s, p)  -- :: { Parser a -> Parser b -> Parser (NonEmpty b) }
  : p many(snd(s, p)) { $1 :| $2 }

-- Zero or more, with separator between and optionally after. Right-recursive to
-- avoid a shift/reduce conflict on the final comma.
--
-- TODO: Preserve source span of trailing separator?
sepEnd(s, p)  -- :: { Parser a -> Parser b -> Parser [b] }
  :                  { [] }
  | p                { [$1] }
  | p s sepEnd(s, p) { $1 : $3 }

-- One or more, with separator between and optionally after. Right-recursive to
-- avoid a shift/reduce conflict on the final comma.
--
-- TODO: Preserve source span of trailing separator?
sepEnd1(s, p)  -- :: { Parser a -> Parser b -> Parser (NonEmpty b) }
  : p                { $1 :| [] }
  | p s sepEnd(s, p) { $1 :| $3 }

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
-- AST
--------------------------------------------------------------------------------

type Binding            = Language.Binding            SourceSpan
type DecimalFloat       = Language.DecimalFloat       SourceSpan
type DecimalFraction    = Language.DecimalFraction    SourceSpan
type DecimalInteger     = Language.DecimalInteger     SourceSpan
type DecimalIntegerPart = Language.DecimalIntegerPart SourceSpan
type Expression         = Language.Expression         SourceSpan
type Literal            = Language.Literal            SourceSpan
type Program            = Language.Program            SourceSpan
type Sign               = Language.Sign               SourceSpan
type Signature          = Language.Signature          SourceSpan
type Splice             = Language.Splice             SourceSpan
type Statement          = Language.Statement          SourceSpan

type Token              = Token.Token                 SourceSpan

--------------------------------------------------------------------------------
-- Statement
--------------------------------------------------------------------------------

atomicStatement :: Token -> Statement -> Statement
atomicStatement atomicKeyword body = Language.AtomicStatement pos body
  where
    pos = mconcat
      [ tokenAnno atomicKeyword
      , statementAnno body
      ]

afterStatement
  :: Token -> Token -> Expression -> Token -> Statement -> Statement
afterStatement afterKeyword leftParenthesis condition rightParenthesis body
  = Language.AfterStatement pos condition body
  where
    pos = mconcat
      [ tokenAnno afterKeyword
      , tokenAnno leftParenthesis
      , expressionAnno condition
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

asLongAsStatement
  :: Token -> Token -> Token -> Token -> Expression -> Token -> Statement
  -> Statement
asLongAsStatement asKeyword longKeyword asKeyword' leftParenthesis condition
  rightParenthesis body
  = Language.AsLongAsStatement pos condition body
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

blockStatement :: Token -> [Statement] -> Token -> Statement
blockStatement openBrace statements closeBrace
  = Language.BlockStatement pos statements
  where
    pos = mconcat $ concat
      [ [tokenAnno openBrace]
      , fmap statementAnno statements
      , [tokenAnno closeBrace]
      ]

emptyStatement :: Token -> Statement
emptyStatement semicolon = Language.EmptyStatement pos
  where
    pos = tokenAnno semicolon

forAllStatement
  :: Token -> Token -> Token -> (SourceSpan, Identifier) -> Token -> Expression
  -> Token -> Statement -> Statement
forAllStatement forKeyword allKeyword leftParenthesis (variablePos, variable)
  colon container rightParenthesis body
  = Language.ForAllStatement pos variable container body
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
  :: Token -> Token -> Token -> (SourceSpan, Identifier) -> Token -> Expression
  -> Token -> Statement -> Statement
forEachStatement forKeyword eachKeyword leftParenthesis (variablePos, variable)
  colon container rightParenthesis body
  = Language.ForEachStatement pos variable container body
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
  :: Token -> (SourceSpan, Identifier) -> [Binding] -> Maybe (Signature)
  -> Token -> Either (Expression) [Statement] -> Token -> Statement
functionStatement functionKeyword (namePos, name) params returnType
  begin body end
  = Language.FunctionStatement pos name params returnType
  $ either
    (Language.ExpressionStatement bodyPos)
    (Language.BlockStatement bodyPos)
    body
  where
    pos = mconcat
      [ tokenAnno functionKeyword
      , namePos
      , foldMap bindingAnno params
      , foldMap signatureAnno returnType
      , bodyPos
      ]
    bodyPos = mconcat
      [ tokenAnno begin
      , either expressionAnno (foldMap statementAnno) body
      , tokenAnno end
      ]

ifStatement
  :: Token -> Token -> Expression -> Token -> Statement
  -> Maybe (SourceSpan, Statement) -> Statement
ifStatement ifKeyword
  leftParenthesis condition rightParenthesis
  thenClause elseClause
  = Language.IfStatement pos condition thenClause (snd <$> elseClause)
  where
    pos = mconcat
      [ tokenAnno ifKeyword
      , tokenAnno leftParenthesis
      , expressionAnno condition
      , tokenAnno rightParenthesis
      , statementAnno thenClause
      , foldMap fst elseClause
      ]

elseClause :: Token -> Statement -> (SourceSpan, Statement)
elseClause elseKeyword body = (pos, body)
  where
    pos = mconcat
      [ tokenAnno elseKeyword
      , statementAnno body
      ]

lastStatement :: Token -> Maybe (SourceSpan, Identifier) -> Token -> Statement
lastStatement lastKeyword mIdentifier semicolon
  = Language.LastStatement pos (snd <$> mIdentifier)
  where
    pos = mconcat
      [ tokenAnno lastKeyword
      , foldMap fst mIdentifier
      , tokenAnno semicolon
      ]

nextStatement :: Token -> Maybe (SourceSpan, Identifier) -> Token -> Statement
nextStatement nextKeyword mIdentifier semicolon
  = Language.NextStatement pos (snd <$> mIdentifier)
  where
    pos = mconcat
      [ tokenAnno nextKeyword
      , foldMap fst mIdentifier
      , tokenAnno semicolon
      ]

onAddStatement
  :: Token -> Token -> Token -> (SourceSpan, Identifier) -> Token -> Expression
  -> Token -> Statement -> Statement
onAddStatement onKeyword addKeyword leftParenthesis (variablePos, variable)
  colon container rightParenthesis body
  = Language.OnAddStatement pos variable container body
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
  :: Token -> Token -> Token -> NonEmpty (SourceSpan, Identifier) -> Token
  -> Statement -> Statement
onChangeStatement onKeyword changeKeyword leftParenthesis variables
  rightParenthesis body
  = Language.OnChangeStatement pos (snd <$> variables) body
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
  :: Token -> Token -> Token -> (SourceSpan, Identifier) -> Token
  -> Expression -> Token -> Statement -> Statement
onRemoveStatement onKeyword removeKeyword leftParenthesis
  (variablePos, variable) colon container rightParenthesis body
  = Language.OnRemoveStatement pos variable container body
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
  :: Token -> Token -> Token -> NonEmpty (SourceSpan, Identifier) -> Token
  -> Statement -> Statement
onSetStatement onKeyword setKeyword leftParenthesis variables
  rightParenthesis body
  = Language.OnSetStatement pos (snd <$> variables) body
  where
    pos = mconcat
      [ tokenAnno onKeyword
      , tokenAnno setKeyword
      , tokenAnno leftParenthesis
      , foldMap fst variables
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

redoStatement :: Token -> Maybe (SourceSpan, Identifier) -> Token -> Statement
redoStatement redoKeyword mIdentifier semicolon
  = Language.RedoStatement pos (snd <$> mIdentifier)
  where
    pos = mconcat
      [ tokenAnno redoKeyword
      , foldMap fst mIdentifier
      , tokenAnno semicolon
      ]

returnStatement :: Token -> Maybe Expression -> Token -> Statement
returnStatement returnKeyword mResult semicolon
  = Language.ReturnStatement pos mResult
  where
    pos = mconcat
      [ tokenAnno returnKeyword
      , foldMap expressionAnno mResult
      , tokenAnno semicolon
      ]

varStatement :: Token -> NonEmpty Binding -> Token -> Statement
varStatement varKeyword bindings semicolon
  = Language.VarStatement pos bindings
  where
    pos = mconcat
      [ tokenAnno varKeyword
      , foldMap bindingAnno bindings
      , tokenAnno semicolon
      ]

whenStatement :: Token -> Token -> Expression -> Token -> Statement -> Statement
whenStatement whenKeyword
  leftParenthesis condition rightParenthesis body
  = Language.WhenStatement pos condition body
  where
    pos = mconcat
      [ tokenAnno whenKeyword
      , tokenAnno leftParenthesis
      , expressionAnno condition
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

wheneverStatement
  :: Token -> Token -> Expression -> Token -> Statement -> Statement
wheneverStatement wheneverKeyword
  leftParenthesis condition rightParenthesis body
  = Language.WheneverStatement pos condition body
  where
    pos = mconcat
      [ tokenAnno wheneverKeyword
      , tokenAnno leftParenthesis
      , expressionAnno condition
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

whileStatement
  :: Token -> Token -> Expression -> Token -> Statement -> Statement
whileStatement whileKeyword
  leftParenthesis condition rightParenthesis body
  = Language.WhileStatement pos condition body
  where
    pos = mconcat
      [ tokenAnno whileKeyword
      , tokenAnno leftParenthesis
      , expressionAnno condition
      , tokenAnno rightParenthesis
      , statementAnno body
      ]

expressionStatement :: Expression -> Token -> Statement
expressionStatement body semicolon = Language.ExpressionStatement pos body
  where
    pos = mconcat
      [ expressionAnno body
      , tokenAnno semicolon
      ]

binding
  :: (SourceSpan, Identifier) -> Maybe Signature -> Maybe Expression -> Binding
binding (namePos, name) mSignature mInitializer = Language.Binding
  -- TODO: Differentiate source spans of just name vs. whole binding?
  { bindingAnno        = namePos
  , bindingName        = name
  , bindingSignature   = mSignature
  , bindingInitializer = mInitializer
  }

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

callExpression :: Expression -> [Expression] -> Expression
callExpression function arguments
  = Language.CallExpression pos function arguments
  where
    pos = mconcat
      [ expressionAnno function
      , foldMap expressionAnno arguments
      ]

groupExpression :: Token -> Expression -> Token -> Expression
groupExpression leftParenthesis body rightParenthesis
  = Language.GroupExpression pos body
  where
    pos = mconcat
      [ tokenAnno leftParenthesis
      , expressionAnno body
      , tokenAnno rightParenthesis
      ]

identifierExpression :: (SourceSpan, Identifier) -> Expression
identifierExpression (pos, identifier)
  = Language.IdentifierExpression pos identifier

identifierParts
  :: (SourceSpan, Text) -> [(SourceSpan, Text)] -> (SourceSpan, Identifier)
identifierParts
  (startSpan, startPart)
  (unzip -> (continueSpans, continueParts))
  =
  ( sconcat (startSpan :| continueSpans)
  , Identifier (startPart :| continueParts)
  )

identifierContinueDigits :: DecimalIntegerPart -> (SourceSpan, Text)
identifierContinueDigits (Language.DecimalIntegerPart pos digits)
  = (pos, Text.pack (decimalDigitString digits))

floatExpression
  :: Maybe DecimalInteger
  -> DecimalFraction
  -- Maybe DecimalExponent
  -> Expression
floatExpression mInteger fraction -- mExponent
  = Language.LiteralExpression pos
  $ Language.DecimalFloatLiteral
  $ Language.DecimalFloat mInteger fraction -- mExponent
  where
    pos = mconcat
      [ foldMap (foldMap decimalIntegerPartAnno . decimalIntegerParts) mInteger
      , decimalFractionAnno fraction
      -- foldMap decimalExponentAnno mExponent
      ]

-- See note [Float Exponents].
--
-- floatExponent
--   :: Token
--   -> Maybe Sign
--   -> DecimalIntegerPart
--   -> DecimalExponent
-- floatExponent e mSign digits
--   = Language.DecimalExponent pos ePos mSign digits
--   where
--     pos = mconcat
--       [ ePos
--       , foldMap signAnno mSign
--       , foldMap decimalIntegerPartAnno digits
--       ]

floatFraction
  :: Token
  -> NonEmpty DecimalIntegerPart
  -> DecimalFraction
floatFraction dot digits
  = Language.DecimalFraction pos dotPos digits
  where
    pos = mconcat
      [ dotPos
      , foldMap decimalIntegerPartAnno digits
      ]
    dotPos = tokenAnno dot

integerExpression :: DecimalInteger -> Expression
integerExpression digits
  = Language.LiteralExpression pos
  $ Language.DecimalIntegerLiteral digits
  where
    pos = foldMap decimalIntegerPartAnno $ decimalIntegerParts digits

listExpression :: Token -> [Expression] -> Token -> Expression
listExpression leftBracket elements rightBracket
  = Language.LiteralExpression pos
  $ Language.ListLiteral elements
  where
    pos = mconcat
      [ tokenAnno leftBracket
      , foldMap expressionAnno elements
      , tokenAnno rightBracket
      ]

mapExpression
  :: Token -> NonEmpty (Expression, Expression) -> Token -> Expression
mapExpression leftBrace keyValuePairs rightBrace
  = Language.LiteralExpression pos
  $ Language.MapLiteral (NonEmpty.toList keyValuePairs)
  where
    pos = mconcat
      [ tokenAnno leftBrace
      , foldMap (uncurry ((<>) `on` expressionAnno)) keyValuePairs
      , tokenAnno rightBrace
      ]

identifierKey :: (SourceSpan, Identifier) -> Expression
identifierKey = textExpression . fmap Language.identifierText

setExpression :: Token -> [Expression] -> Token -> Expression
setExpression leftBrace elements rightBrace
  = Language.LiteralExpression pos
  $ Language.SetLiteral elements
  where
    pos = mconcat
      [ tokenAnno leftBrace
      , foldMap expressionAnno elements
      , tokenAnno rightBrace
      ]

spliceExpression
  :: (SourceSpan, Text) -> [Splice] -> (SourceSpan, Text) -> Expression
spliceExpression leftText@(leftTextPos, _) splices rightText@(rightTextPos, _)
  = Language.SpliceExpression pos
    (Language.TextSplice leftText :| splices <> [Language.TextSplice rightText])
  where
    pos = mconcat
      [ leftTextPos
      , foldMap spliceAnno splices
      , rightTextPos
      ]

textExpression :: (SourceSpan, Text) -> Expression
textExpression (pos, text) = Language.LiteralExpression pos
  $ Language.TextLiteral text

textSplices :: Expression -> [((SourceSpan, Text), Expression)] -> [Splice]
textSplices initialExpression splices
  = Language.ExpressionSplice initialExpression
  : concatMap
    (unpair . (Language.TextSplice *** Language.ExpressionSplice))
    splices
  where
    unpair (a, b) = [a, b]

-- Operators

binaryOp :: BinaryOperator -> Expression -> Token -> Expression -> Expression
binaryOp operation leftOperand operator rightOperand
  = Language.BinaryExpression pos operation leftOperand rightOperand
  where
    pos = mconcat
      [ expressionAnno leftOperand
      , tokenAnno operator
      , expressionAnno rightOperand
      ]

unaryOp :: UnaryOperator -> Token -> Expression -> Expression
unaryOp operation operator operand
  = Language.UnaryExpression pos operation operand
  where
    pos = mconcat
      [ tokenAnno operator
      , expressionAnno operand
      ]

}
