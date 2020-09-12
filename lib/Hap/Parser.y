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
  ( Identifier(..)
  , bindingAnno
  , decimalDigitString
  , expressionAnno
  , signatureAnno
  , statementAnno
  )
import Hap.Parse (lexToken)
import Hap.ParserMonad (Parser, parseFailure)
import Hap.Token
  ( DecimalDigit
  , Keyword(..)
  , SourceSpan(..)
  , tokenAnno
  )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Hap.Language as Language
import qualified Hap.Token as Token

}

%name      programParser Program
%name      tokensParser Tokens
%tokentype { Token }
%monad     { Parser }
%lexer     { lexToken } { Token.EofToken }
%error     { parseFailure }

%token

--------------------------------------------------------------------------------
-- Primitive Tokens
--------------------------------------------------------------------------------

  word   { Token.WordToken   $$ }
  digits { Token.DigitsToken $$ }

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
  '.'  { Token.DotToken                _ }
  '/'  { Token.SlashToken              _ }
  ':'  { Token.ColonToken              _ }
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

  -- TODO: Differentiate primary and secondary/contextual-only keywords.
  add      { Token.KeywordToken (_, AddKeyword)      }
  after    { Token.KeywordToken (_, AfterKeyword)    }
  all      { Token.KeywordToken (_, AllKeyword)      }
  as       { Token.KeywordToken (_, AsKeyword)       }
  async    { Token.KeywordToken (_, AsyncKeyword)    }
  atomic   { Token.KeywordToken (_, AtomicKeyword)   }
  before   { Token.KeywordToken (_, BeforeKeyword)   }
  change   { Token.KeywordToken (_, ChangeKeyword)   }
  each     { Token.KeywordToken (_, EachKeyword)     }
  else     { Token.KeywordToken (_, ElseKeyword)     }
  entity   { Token.KeywordToken (_, EntityKeyword)   }
  every    { Token.KeywordToken (_, EveryKeyword)    }
  false    { Token.KeywordToken (_, FalseKeyword)    }
  for      { Token.KeywordToken (_, ForKeyword)      }
  function { Token.KeywordToken (_, FunctionKeyword) }
  has      { Token.KeywordToken (_, HasKeyword)      }
  if       { Token.KeywordToken (_, IfKeyword)       }
  in       { Token.KeywordToken (_, InKeyword)       }
  last     { Token.KeywordToken (_, LastKeyword)     }
  long     { Token.KeywordToken (_, LongKeyword)     }
  needs    { Token.KeywordToken (_, NeedsKeyword)    }
  next     { Token.KeywordToken (_, NextKeyword)     }
  null     { Token.KeywordToken (_, NullKeyword)     }
  on       { Token.KeywordToken (_, OnKeyword)       }
  redo     { Token.KeywordToken (_, RedoKeyword)     }
  remove   { Token.KeywordToken (_, RemoveKeyword)   }
  return   { Token.KeywordToken (_, ReturnKeyword)   }
  set      { Token.KeywordToken (_, SetKeyword)      }
  true     { Token.KeywordToken (_, TrueKeyword)     }
  until    { Token.KeywordToken (_, UntilKeyword)    }
  var      { Token.KeywordToken (_, VarKeyword)      }
  when     { Token.KeywordToken (_, WhenKeyword)     }
  whenever { Token.KeywordToken (_, WheneverKeyword) }
  where    { Token.KeywordToken (_, WhereKeyword)    }
  which    { Token.KeywordToken (_, WhichKeyword)    }
  while    { Token.KeywordToken (_, WhileKeyword)    }

--------------------------------------------------------------------------------
-- Any Token (MUST BE LAST)
--------------------------------------------------------------------------------

  token { $$ }

--------------------------------------------------------------------------------
-- Precedences
--------------------------------------------------------------------------------

%nonassoc NO_ELSE
%nonassoc else
%right word

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

    AtomicStatement :: { Statement }
      : atomic Statement
      { atomicStatement $1 $2 }

    AfterStatement :: { Statement }
      : after '(' Expression ')' Statement
      { afterStatement $1 $2 $3 $4 $5 }

    AsLongAsStatement
      : as long as '(' Expression ')' Statement
      { asLongAsStatement $1 $2 $3 $4 $5 $6 $7 }

    BlockStatement :: { Statement }
      : '{' many(Statement) '}'
      { blockStatement $1 $2 $3 }

    EmptyStatement :: { Statement }
      : ';'
      { emptyStatement $1 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    ForAllStatement :: { Statement }
      : for all '(' Identifier ':' Expression ')' Statement
      { forAllStatement $1 $2 $3 $4 $5 $6 $7 $8 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    ForEachStatement :: { Statement }
      : for each '(' Identifier ':' Expression ')' Statement
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
      : if '(' Expression ')' Statement
        %prec NO_ELSE
        { ifStatement $1 $2 $3 $4 $5 Nothing }
      | if '(' Expression ')' Statement ElseClause
        { ifStatement $1 $2 $3 $4 $5 (Just $6) }

      ElseClause :: { (SourceSpan, Statement) }
        : else Statement
        { elseClause $1 $2 }

    LastStatement :: { Statement }
      : last opt(Identifier) ';'
      { lastStatement $1 $2 $3 }

    NextStatement :: { Statement }
      : next opt(Identifier) ';'
      { nextStatement $1 $2 $3 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    OnAddStatement :: { Statement }
      : on add '(' Identifier ':' Expression ')' Statement
      { onAddStatement $1 $2 $3 $4 $5 $6 $7 $8 }

    OnChangeStatement :: { Statement }
      : on change '(' sepEnd1(',', Identifier) ')' Statement
      { onChangeStatement $1 $2 $3 $4 $5 $6 }

    -- TODO: See note [Name-Expression Separator in Quantifiers].
    OnRemoveStatement :: { Statement }
      : on remove '(' Identifier ':' Expression ')' Statement
      { onRemoveStatement $1 $2 $3 $4 $5 $6 $7 $8 }

    OnSetStatement :: { Statement }
      : on set '(' sepEnd1(',', Identifier) ')' Statement
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
      : when '(' Expression ')' Statement
      { whenStatement $1 $2 $3 $4 $5 }

    WheneverStatement :: { Statement }
      : whenever '(' Expression ')' Statement
      { wheneverStatement $1 $2 $3 $4 $5 }

    WhileStatement :: { Statement }
      : while '(' Expression ')' Statement
      { whileStatement $1 $2 $3 $4 $5 }

    ExpressionStatement :: { Statement }
      : Expression ';'
      { expressionStatement $1 $2 }

      -- See [Expression].

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
    : Boolean            { booleanExpression $1 }
    | Null               { nullExpression $1 }
    | '(' Expression ')' { groupExpression $1 $2 $3 }

    | Identifier         { identifierExpression $1 }
    | some(digits)       { integerExpression $1 }

    Boolean :: { (SourceSpan, Bool) }
      : true  { (tokenAnno $1, True) }
      | false { (tokenAnno $1, False) }

    Identifier :: { (SourceSpan, Identifier) }
      : IdentifierStart many(IdentifierContinue)
      { identifierParts $1 $2 }

      IdentifierStart :: { (SourceSpan, Text) }
        : word { $1 }

      IdentifierContinue :: { (SourceSpan, Text) }
        : word           { $1 }
        | ContextualWord { $1 }
        | digits         { identifierContinueDigits $1 }

    Null :: { SourceSpan }
      : null  { tokenAnno $1 }

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

--------------------------------------------------------------------------------
-- Contextual Keywords
--------------------------------------------------------------------------------

-- A keyword contextually interpreted as a name part.
ContextualWord :: { (SourceSpan, Text) }
  : add      { (tokenAnno $1, "add")      }
  | after    { (tokenAnno $1, "after")    }
  | all      { (tokenAnno $1, "all")      }
  | as       { (tokenAnno $1, "as")       }
  | async    { (tokenAnno $1, "async")    }
  | atomic   { (tokenAnno $1, "atomic")   }
  | before   { (tokenAnno $1, "before")   }
  | change   { (tokenAnno $1, "change")   }
  | each     { (tokenAnno $1, "each")     }
  | else     { (tokenAnno $1, "else")     }
  | entity   { (tokenAnno $1, "entity")   }
  | every    { (tokenAnno $1, "every")    }
  | false    { (tokenAnno $1, "false")    }
  | for      { (tokenAnno $1, "for")      }
  | function { (tokenAnno $1, "function") }
  | has      { (tokenAnno $1, "has")      }
  | if       { (tokenAnno $1, "if")       }
  | in       { (tokenAnno $1, "in")       }
  | last     { (tokenAnno $1, "last")     }
  | long     { (tokenAnno $1, "long")     }
  | needs    { (tokenAnno $1, "needs")    }
  | next     { (tokenAnno $1, "next")     }
  | null     { (tokenAnno $1, "null")     }
  | on       { (tokenAnno $1, "on")       }
  | redo     { (tokenAnno $1, "redo")     }
  | remove   { (tokenAnno $1, "remove")   }
  | return   { (tokenAnno $1, "return")   }
  | set      { (tokenAnno $1, "set")      }
  | true     { (tokenAnno $1, "true")     }
  | until    { (tokenAnno $1, "until")    }
  | var      { (tokenAnno $1, "var")      }
  | when     { (tokenAnno $1, "when")     }
  | whenever { (tokenAnno $1, "whenever") }
  | where    { (tokenAnno $1, "where")    }
  | which    { (tokenAnno $1, "which")    }
  | while    { (tokenAnno $1, "while")    }

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

type Binding    = Language.Binding    SourceSpan
type Expression = Language.Expression SourceSpan
type Literal    = Language.Literal    SourceSpan
type Program    = Language.Program    SourceSpan
type Signature  = Language.Signature  SourceSpan
type Statement  = Language.Statement  SourceSpan

type Token      = Token.Token         SourceSpan

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

booleanExpression :: (SourceSpan, Bool) -> Expression
booleanExpression (pos, value)
  = Language.LiteralExpression pos (Language.BooleanLiteral value)

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

identifierContinueDigits
  :: (SourceSpan, NonEmpty DecimalDigit) -> (SourceSpan, Text)
identifierContinueDigits (pos, digits)
  = (pos, Text.pack (decimalDigitString digits))

integerExpression
  :: NonEmpty (SourceSpan, NonEmpty DecimalDigit) -> Expression
integerExpression digits
  = Language.LiteralExpression
    (foldMap fst digits)
    (Language.DecimalIntegerLiteral digits)

nullExpression :: SourceSpan -> Expression
nullExpression pos = Language.LiteralExpression pos Language.NullLiteral

}
