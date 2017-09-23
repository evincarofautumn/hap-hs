module Hap.Language
  ( BinaryOperator(..)
  , Expression(..)
  , Identifier(..)
  , Literal(..)
  , Program(..)
  , Signature(..)
  , Statement(..)
  , TernaryOperator(..)
  , UnaryOperator(..)
  , parseProgram
  ) where

import Control.Applicative
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Either (partitionEithers)
import Data.Foldable (asum)
import Data.Functor.Identity (Identity)
import Data.List (foldl')
import Data.Text (Text)
import Hap.Operators
import Text.Parsec (ParseError)
import Text.Parsec.Expr (buildExpressionParser)
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.String (Parser)
import qualified Data.Text as Text
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as Expr

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

data Program = Program [Statement]
  deriving (Eq, Show)

data Statement
  = AtomicStatement !SourcePos !Statement
  | AfterStatement !SourcePos !Expression !Statement
  | AsLongAsStatement !SourcePos !Expression !Statement
  | BlockStatement !SourcePos [Statement]
  | EmptyStatement !SourcePos
  | ExpressionStatement !SourcePos !Expression
  | ForAllStatement !SourcePos !Identifier !Expression !Statement
  | ForEachStatement !SourcePos !Identifier !Expression !Statement
  | FunctionStatement !SourcePos !Identifier [(Identifier, Maybe Signature)] Statement
  | IfStatement !SourcePos !Expression !Statement !(Maybe Statement)
  | LastStatement !SourcePos !(Maybe Identifier)
  | NextStatement !SourcePos !(Maybe Identifier)
  | OnChangeStatement !SourcePos [Identifier] !Statement
  | OnSetStatement !SourcePos [Identifier] !Statement
  -- OnAddStatement !Identifier !Identifier !Statement
  -- OnRemoveStatement !Identifier !Identifier !Statement
  | RedoStatement !SourcePos !(Maybe Identifier)
  | ReturnStatement !SourcePos !(Maybe Expression)
  | VarStatement !SourcePos [(Identifier, Maybe Signature, Maybe Expression)]
  | WheneverStatement !SourcePos !Expression !Statement
  | WhileStatement !SourcePos !Expression !Statement
  deriving (Eq, Show)

data Expression
  = LiteralExpression !SourcePos !Literal
  | IdentifierExpression !SourcePos !Identifier
  | SubscriptExpression !SourcePos !Expression [Expression]
  | DotExpression !SourcePos !Expression !Identifier
  | CallExpression !SourcePos !Expression [Expression]
  | LetExpression !SourcePos [(Identifier, Maybe Signature, Expression)] !Expression
  | GroupExpression !SourcePos !Expression
  | UnaryExpression !SourcePos !UnaryOperator !Expression
  | BinaryExpression !SourcePos !BinaryOperator !Expression !Expression
  | TernaryExpression !SourcePos !TernaryOperator !Expression !Expression !Expression
  deriving (Eq, Show)

data Literal
  = BooleanLiteral !Bool
  | FloatLiteral !Double
  | IntegerLiteral !Integer
  | TextLiteral !Text
  | NullLiteral
  | ListLiteral [Expression]
  | MapLiteral [(Expression, Expression)]
  | SetLiteral [Expression]
  deriving (Eq, Show)

data UnaryOperator
  = UnaryAddress
  | UnaryDecrement
  | UnaryDereference
  | UnaryEach
  | UnaryEvery
  | UnaryIncrement
  | UnaryMinus
  | UnaryNot
  | UnaryPlus
  deriving (Eq, Show)

data BinaryOperator
  -- Multiplicative
  = BinaryMultiply
  | BinaryDivide
  | BinaryModulus
  -- Additive
  | BinaryAdd
  | BinarySubtract
  -- Relational
  | BinaryLess
  | BinaryNotLess
  | BinaryGreater
  | BinaryNotGreater
  | BinaryEqual
  | BinaryNotEqual
  | BinaryElement
  -- Conjunctive
  | BinaryAnd
  | BinaryIntersect
  -- Disjunctive
  | BinaryOr
  | BinaryXor
  | BinaryUnion
  -- Implicative
  | BinaryImplies
  -- Assignment
  | BinaryAssign
  deriving (Eq, Show)

data TernaryOperator
  = TernaryConditional
  deriving (Eq, Show)

data Signature
  = ConstructorSignature !Identifier
  | FunctionSignature [Signature] !Signature
  | ApplicationSignature !Signature !Signature
  deriving (Eq, Show)

newtype Identifier = Identifier Text
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parseProgram :: FilePath -> String -> Either ParseError Program
parseProgram path source = Parsec.parse programParser path source
  where
    programParser :: Parser Program
    programParser = Program
      <$> Parsec.between Parsec.spaces Parsec.eof (many statementParser)

    token :: String -> Parser ()
    token string = Parsec.try (Parsec.string string) *> Parsec.spaces

    grouped :: Parser a -> Parser a
    grouped = Parsec.between (token "(") (token ")")

    bracketed :: Parser a -> Parser a
    bracketed = Parsec.between (token "[") (token "]")

    blocked :: Parser a -> Parser a
    blocked = Parsec.between (token "{") (token "}")

    quoted :: Parser a -> Parser a
    quoted = Parsec.between (Parsec.char '"') (Parsec.char '"')

    statementParser :: Parser Statement
    statementParser = asum
      [ AtomicStatement
        <$> (getSourcePos <* token "atomic")
        <*> statementParser

      , AfterStatement
        <$> (getSourcePos <* token "after")
        <*> grouped expressionParser
        <*> statementParser

      , AsLongAsStatement
        <$> (getSourcePos <* token "as" <* token "long" <* token "as")
        <*> grouped expressionParser
        <*> statementParser

      , BlockStatement <$> getSourcePos <*> blocked (many statementParser)

      , EmptyStatement
        <$> (getSourcePos <* token ";")

      , do
        pos <- getSourcePos <* token "for"
        constructor <- asum
          [ ForAllStatement <$ token "all"
          , ForEachStatement <$ token "each"
          ]
        identifier <- identifierParser
        container <- grouped expressionParser
        body <- statementParser
        pure $ constructor pos identifier container body

      , FunctionStatement
        <$> (getSourcePos <* token "function")
        <*> identifierParser
        <*> Parsec.sepEndBy
          ((,)
            <$> identifierParser
            <*> Parsec.optionMaybe signatureParser)
          (token ",")
        <*> statementParser

      , IfStatement
        <$> (getSourcePos <* token "if")
        <*> grouped expressionParser
        <*> statementParser
        <*> Parsec.optionMaybe (token "else" *> statementParser)

      , LastStatement
        <$> (getSourcePos <* token "last")
        <*> (Parsec.optionMaybe identifierParser <* token ";")

      , NextStatement
        <$> (getSourcePos <* token "next")
        <*> (Parsec.optionMaybe identifierParser <* token ";")

      , do
        pos <- getSourcePos <* token "on"
        constructor <- asum
          [ OnChangeStatement <$ token "change"
          , OnSetStatement <$ token "set"
          -- TODO: add, remove
          ]
        variables <- grouped (Parsec.sepEndBy identifierParser (token ","))
        body <- statementParser
        pure $ constructor pos variables body

      , RedoStatement
        <$> (getSourcePos <* token "redo")
        <*> (Parsec.optionMaybe identifierParser <* token ";")

      , ReturnStatement
        <$> (getSourcePos <* token "return")
        <*> (Parsec.optionMaybe expressionParser <* token ";")

      , VarStatement
        <$> (getSourcePos <* token "var")
        <*> Parsec.sepBy
          ((,,)
            <$> identifierParser
            <*> Parsec.optionMaybe (token ":" *> signatureParser)
            <*> Parsec.optionMaybe (token "=" *> expressionParser))
          (token ",")

      , WheneverStatement
        <$> (getSourcePos <* token "whenever")
        <*> grouped expressionParser
        <*> statementParser

      , WhileStatement
        <$> (getSourcePos <* token "while")
        <*> grouped expressionParser
        <*> statementParser

      -- Must appear last.
      , ExpressionStatement
        <$> getSourcePos
        <*> (expressionParser <* token ";")

      ]

    expressionParser :: Parser Expression
    expressionParser = buildExpressionParser operatorTable termParser
      where

        operatorTable :: [[Expr.Operator String () Identity Expression]]
        operatorTable =
          -- Prefix
          [ [ prefix "each" UnaryEach
            , prefix "every" UnaryEvery
            , prefix "-" UnaryMinus
            , prefix "not" UnaryNot
            , prefix "+" UnaryPlus
            ]

          -- Multiplicative
          , [ binary "*" Expr.AssocLeft BinaryMultiply
            , binary "/" Expr.AssocLeft BinaryDivide
            , binary "mod" Expr.AssocLeft BinaryModulus
            ]

          -- Additive
          , [ binary "+" Expr.AssocLeft BinaryAdd
            , binary "-" Expr.AssocLeft BinarySubtract
            ]

          -- Relational
          , [ binary "<" Expr.AssocLeft BinaryLess
            , binary ">=" Expr.AssocLeft BinaryNotLess
            , binary ">" Expr.AssocLeft BinaryGreater
            , binary "<=" Expr.AssocLeft BinaryNotGreater
            , binary "=" Expr.AssocLeft BinaryEqual
            , binary "<>" Expr.AssocLeft BinaryNotEqual
            , binary "in" Expr.AssocNone BinaryElement
            ]

          -- Conjunctive
          , [ binary "and" Expr.AssocRight BinaryAnd
            ]

          -- Disjunctive
          , [ binary "or" Expr.AssocRight BinaryOr
            , binary "xor" Expr.AssocRight BinaryXor
            ]

          -- Implicative
          , [ binary "implies" Expr.AssocRight BinaryImplies
            ]

          -- Assignment
          , [ binary "<-" Expr.AssocNone BinaryAssign
            ]

          ]
          where

            prefix
              :: String
              -> UnaryOperator
              -> Expr.Operator String () Identity Expression
            prefix name operator = Expr.Prefix $ do
              pos <- getSourcePos <* token name
              pure $ UnaryExpression pos operator

            binary
              :: String
              -> Expr.Assoc
              -> BinaryOperator
              -> Expr.Operator String () Identity Expression
            binary name associativity operator
              = flip Expr.Infix associativity $ do
              pos <- getSourcePos <* token name
              pure $ BinaryExpression pos operator

        termParser :: Parser Expression
        termParser = do
          prefix <- asum

            [ LiteralExpression
              <$> getSourcePos
              <*> literalParser

            , LetExpression
              <$> (getSourcePos <* token "let")
              <*> Parsec.sepBy
                ((,,)
                  <$> identifierParser
                  <*> Parsec.optionMaybe (token ":" *> signatureParser)
                  <*> (token "=" *> expressionParser))
                (token ",")
              <*> (token "in" *> expressionParser)

            , IdentifierExpression
              <$> getSourcePos
              <*> identifierParser

            , GroupExpression <$> getSourcePos <*> grouped expressionParser
            ]

          suffixes <- many suffixParser
          pure $ foldl' (.) id suffixes prefix

        character :: Parser Char
        character = Parsec.noneOf "\\\""

        escape :: Parser Char
        escape = Parsec.char '\\' *> asum
          [ '"' <$ Parsec.char '"'
          , '\\' <$ Parsec.char '\\'
          , do
            unknown <- Parsec.anyChar
            Parsec.unexpected $ "unknown escape '" ++ [unknown] ++ "'"
          ]

        suffixParser :: Parser (Expression -> Expression)
        suffixParser = do
          pos <- getSourcePos
          asum
            [ do
              subscripts <- bracketed $ Parsec.sepBy expressionParser (token ",")
              pure $ \ prefix -> SubscriptExpression pos prefix subscripts
            , do
              identifier <- identifierParser
              pure $ \ prefix -> DotExpression pos prefix identifier
            , do
              arguments <- grouped $ Parsec.sepEndBy expressionParser (token ",")
              pure $ \ prefix -> CallExpression pos prefix arguments
            ]

        literalParser :: Parser Literal
        literalParser = asum
          [ BooleanLiteral <$> asum
            [ True <$ token "true"
            , False <$ token "false"
            ]

          , do
            integerPart <- Parsec.many1 Parsec.digit
            mFractionalPart <- Parsec.optionMaybe
              (Parsec.char '.' *> Parsec.many1 Parsec.digit)
              <* Parsec.spaces
            pure $ case mFractionalPart of
              Just fractionalPart -> FloatLiteral
                $ read $ integerPart ++ "." ++ fractionalPart
              Nothing -> IntegerLiteral
                $ read integerPart

          , TextLiteral <$> textLiteralParser

          , NullLiteral <$ token "null"

          , ListLiteral
            <$> bracketed (Parsec.sepEndBy expressionParser (token ","))

          , token "{" *> do
            keyValuePairs <- Parsec.sepEndBy keyValuePair (token ",")
              <* token "}"
            let
              (setKeys, mapKeyValues) = partitionEithers
                $ map (\ (key, mValue) -> case mValue of
                  Just value -> Right (key, value)
                  Nothing -> Left key) keyValuePairs
            case (null setKeys, null mapKeyValues) of
              (True, True) -> pure $ SetLiteral []
              (True, False) -> pure $ MapLiteral mapKeyValues
              (False, True) -> pure $ SetLiteral setKeys
              (False, False)
                -> Parsec.unexpected "map missing values for some keys"
          ]
          where

            textLiteralParser :: Parser Text
            textLiteralParser = Text.pack
              <$> (quoted (many (character <|> escape)) <* Parsec.spaces)

            keyValuePair :: Parser (Expression, Maybe Expression)
            keyValuePair = do
              pos <- getSourcePos
              asum
                [ Parsec.try $ do
                  key <- asum
                    [ IdentifierExpression pos <$> identifierParser
                    , LiteralExpression pos . TextLiteral <$> textLiteralParser
                    ]
                  value <- token ":" *> expressionParser
                  pure (key, Just value)
                , do
                  key <- grouped expressionParser
                  mValue <- Parsec.optionMaybe $ token ":" *> expressionParser
                  pure (key, mValue)
                , do
                  key <- expressionParser
                  pure (key, Nothing)
                ]

    getSourcePos :: Parser SourcePos
    getSourcePos = Parsec.statePos <$> Parsec.getParserState

    identifierParser :: Parser Identifier
    identifierParser = Identifier . Text.pack
      <$> ((:)
        <$> Parsec.satisfy startsIdentifier
        <*> (many (Parsec.satisfy continuesIdentifier) <* Parsec.spaces))
      where
        startsIdentifier = isAsciiLower .|| isAsciiUpper .|| (== '_')
        continuesIdentifier = startsIdentifier .|| isDigit

    signatureParser :: Parser Signature
    signatureParser = fail "TODO: signatureParser"
