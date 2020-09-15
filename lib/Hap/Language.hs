{-# LANGUAGE OverloadedStrings #-}

module Hap.Language
  ( BinaryOperator(..)
  , Binding(..)
  , DecimalFloat(..)
  , DecimalFraction(..)
  , DecimalInteger(..)
  , DecimalIntegerPart(..)
  , Expression(..)
  , Identifier(..)
  , Literal(..)
  , NativeId
  , Program(..)
  , Sign(..)
  , Signature(..)
  , Splice(..)
  , Statement(..)
  , UnaryOperator(..)
  , Value(..)
  , decimalDigitString
  , decimalIntegerString
  , expressionAnno
  , identifierText
  , nativeFunction
  , nativeId
  , nativeName
  , signAnno
  , signatureAnno
  , spliceAnno
  , statementAnno
  ) where

import Control.Concurrent.STM (atomically, writeTChan)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (for_, traverse_)
import Data.IntMap (IntMap)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))
import GHC.Exts (IsString(..))
import Hap.Runtime (Env(..))
import Hap.Token (DecimalDigit, decimalDigitChar)
import SDL (($=))
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified SDL

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

data Program anno = Program [Statement anno]
  deriving stock (Eq, Functor, Show)

instance Semigroup (Program anno) where
  Program a <> Program b = Program (a <> b)

instance Monoid (Program anno) where
  mempty = Program mempty

-- TODO: Merge statements and expressions, or add statement expressions.
--
-- TODO: Use wrappers to make it easier/safer to match, e.g.:
--
--     data Annotated anno a = Annotated
--       { annotatedAnno :: anno
--       , annotatedItem :: a
--       }
--
--     data Binding anno = Binding
--       { bindingName :: Identifier
--       , bindingSignature :: Maybe (Signature anno)
--       , bindingInitializer :: Maybe (Expression anno)
--       }
--
--     data Statement anno
--       ...
--       | VarStatement (Annotated anno [Binding anno])
--       ...
--
data Statement anno
  = AtomicStatement anno !(Statement anno)
  -- AsyncStatement anno
  | AfterStatement anno !(Expression anno) !(Statement anno)
  | AsLongAsStatement anno !(Expression anno) !(Statement anno)
  | BlockStatement anno [Statement anno]
  | EmptyStatement anno
  | ExpressionStatement anno !(Expression anno)
  | ForAllStatement anno !Identifier !(Expression anno) !(Statement anno)
  | ForEachStatement anno !Identifier !(Expression anno) !(Statement anno)
  | FunctionStatement anno
    !Identifier
    [Binding anno]
    !(Maybe (Signature anno))
    !(Statement anno)
  | IfStatement anno
    !(Expression anno)
    !(Statement anno)
    !(Maybe (Statement anno))
  | LastStatement anno !(Maybe Identifier)
  | NextStatement anno !(Maybe Identifier)
  | OnAddStatement anno !Identifier !(Expression anno) !(Statement anno)
  | OnChangeStatement anno (NonEmpty Identifier) !(Statement anno)
  | OnRemoveStatement anno !Identifier !(Expression anno) !(Statement anno)
  | OnSetStatement anno (NonEmpty Identifier) !(Statement anno)
  | RedoStatement anno !(Maybe Identifier)
  | ReturnStatement anno !(Maybe (Expression anno))
  | VarStatement anno (NonEmpty (Binding anno))
  | WhenStatement anno !(Expression anno) !(Statement anno)
  | WheneverStatement anno !(Expression anno) !(Statement anno)
  | WhileStatement anno !(Expression anno) !(Statement anno)
  deriving stock (Eq, Functor, Show)

statementAnno :: Statement anno -> anno
statementAnno = \ case
  AtomicStatement     anno _       -> anno
  AfterStatement      anno _ _     -> anno
  AsLongAsStatement   anno _ _     -> anno
  BlockStatement      anno _       -> anno
  EmptyStatement      anno         -> anno
  ExpressionStatement anno _       -> anno
  ForAllStatement     anno _ _ _   -> anno
  ForEachStatement    anno _ _ _   -> anno
  FunctionStatement   anno _ _ _ _ -> anno
  IfStatement         anno _ _ _   -> anno
  LastStatement       anno _       -> anno
  NextStatement       anno _       -> anno
  OnAddStatement      anno _ _ _   -> anno
  OnChangeStatement   anno _ _     -> anno
  OnRemoveStatement   anno _ _ _   -> anno
  OnSetStatement      anno _ _     -> anno
  RedoStatement       anno _       -> anno
  ReturnStatement     anno _       -> anno
  VarStatement        anno _       -> anno
  WhenStatement       anno _ _     -> anno
  WheneverStatement   anno _ _     -> anno
  WhileStatement      anno _ _     -> anno

data Binding anno = Binding
  { bindingAnno        :: anno
  , bindingName        :: !Identifier
  , bindingSignature   :: !(Maybe (Signature anno))
  , bindingInitializer :: !(Maybe (Expression anno))
  }
  deriving stock (Eq, Functor, Show)

data Expression anno
  = LiteralExpression anno !(Literal anno)
  | IdentifierExpression anno !Identifier
  | SubscriptExpression anno !(Expression anno) [Expression anno]
  | DotExpression anno !(Expression anno) !Identifier
  | CallExpression anno !(Expression anno) [Expression anno]
  | LetExpression anno
    [(Identifier, Maybe (Signature anno), (Expression anno))]
    !(Expression anno)
  | GroupExpression anno !(Expression anno)
  | UnaryExpression anno !UnaryOperator !(Expression anno)
  | BinaryExpression anno !BinaryOperator !(Expression anno) !(Expression anno)
  | IfExpression anno !(Expression anno) !(Expression anno) !(Expression anno)
  | SpliceExpression anno !(NonEmpty (Splice anno))
  deriving stock (Eq, Functor, Show)

expressionAnno :: Expression anno -> anno
expressionAnno = \ case
  LiteralExpression    anno _     -> anno
  IdentifierExpression anno _     -> anno
  SubscriptExpression  anno _ _   -> anno
  DotExpression        anno _ _   -> anno
  CallExpression       anno _ _   -> anno
  LetExpression        anno _ _   -> anno
  GroupExpression      anno _     -> anno
  UnaryExpression      anno _ _   -> anno
  BinaryExpression     anno _ _ _ -> anno
  IfExpression         anno _ _ _ -> anno
  SpliceExpression     anno _     -> anno

data Splice anno
  = TextSplice (anno, Text)
  | ExpressionSplice (Expression anno)
  deriving stock (Eq, Functor, Show)

spliceAnno :: Splice anno -> anno
spliceAnno = \ case
  TextSplice (anno, _)        -> anno
  ExpressionSplice expression -> expressionAnno expression

-- TODO: Keep structured literals around here instead of just values.
data Literal anno
  = BooleanLiteral !Bool
  | DecimalFloatLiteral !(DecimalFloat anno)
  | DecimalIntegerLiteral !(DecimalInteger anno)
  | FunctionLiteral
    !(Maybe Identifier)
    [(Identifier, Maybe (Signature anno), Maybe (Expression anno))]
    !(Maybe (Signature anno))
    !(Statement anno)
  | ListLiteral [Expression anno]
  | MapLiteral [(Expression anno, Expression anno)]
  | NullLiteral
  | SetLiteral [Expression anno]
  | TextLiteral !Text
  deriving stock (Eq, Functor, Show)

data DecimalFloat anno = DecimalFloat
  { decimalFloatInteger  :: !(Maybe (DecimalInteger anno))
  , decimalFloatFraction :: !(DecimalFraction anno)
  -- See note [Float Exponents].
  -- decimalFloatExponent :: !(Maybe (DecimalExponent anno))
  }
  deriving stock (Eq, Functor, Show)

data DecimalFraction anno = DecimalFraction
  { decimalFractionAnno   :: anno
  , decimalFractionDot    :: anno
  , decimalFractionDigits :: !(NonEmpty (DecimalIntegerPart anno))
  }
  deriving stock (Eq, Functor, Show)

-- See note [Float Exponents].
--
-- data DecimalExponent anno = DecimalExponent
--   { decimalExponentAnno   :: anno
--   , decimalExponentE      :: anno
--   , decimalExponentSign   :: !(Maybe (Sign anno))
--   , decimalExponentDigits :: !(NonEmpty (DecimalDigit))
--   }
--   deriving stock (Eq, Functor, Show)

data Sign anno
  = Plus anno
  | Minus anno
  deriving stock (Eq, Functor, Show)

signAnno :: Sign anno -> anno
signAnno = \ case
  Plus  anno -> anno
  Minus anno -> anno

newtype DecimalInteger anno = DecimalInteger
  { decimalIntegerParts :: NonEmpty (DecimalIntegerPart anno) }
  deriving stock (Eq, Functor, Show)

data DecimalIntegerPart anno = DecimalIntegerPart
  { decimalIntegerPartAnno   :: anno
  , decimalIntegerPartDigits :: !(NonEmpty DecimalDigit)
  }
  deriving stock (Eq, Functor, Show)

decimalDigitString :: NonEmpty DecimalDigit -> String
decimalDigitString = fmap decimalDigitChar . NonEmpty.toList

decimalIntegerString :: DecimalInteger anno -> String
decimalIntegerString
  = decimalDigitString
  . join . fmap decimalIntegerPartDigits
  . decimalIntegerParts

data UnaryOperator
  = UnaryAll
  | UnaryEach
  | UnaryEqual
  | UnaryEvery
  | UnaryGreater
  | UnaryHowMany
  | UnaryLess
  | UnaryMinus
  | UnaryNone
  | UnaryNot
  | UnaryNotEqual
  | UnaryNotGreater
  | UnaryNotLess
  | UnaryPlus
  | UnarySome
  | UnaryWhere
  | UnaryWhich
  deriving stock (Eq, Show)

-- Note [Each and Every]:
--
-- The 'each' and 'every' operators are syntactic sugar for common types of
-- loops; when used in an expression, they cause the expression to be desugared
-- to a loop over their arguments. Semantically, multiple 'each' operators
-- produce a single loop over the contents of their operands zipped together in
-- the order they appear in the source, while multiple 'every' operators
-- generate nested loops, also in order of appearance. If only a single 'each'
-- or 'every' appears, the two are otherwise equivalent. For example:
--
--     output(each xs);
--     =>
--     for each _x in (xs) output(_x);
--
--     output(each xs + each ys);
--     =>
--     for each (_x, _y) in (xs, ys) output(_x + _y);
--
--     output(each xs + each ys + each zs);
--     =>
--     for each (_x, _y, _z) in (xs, ys, zs) output(_x + _y + _z);
--
--     output(every xs);
--     =>
--     for each _x in (xs) output(_x);
--
--     output(every xs * every ys);
--     =>
--     for each _x in (xs)
--       for each _y in (ys)
--         output(_x * _y);
--
--     output(every xs * every ys + every zs);
--     =>
--     for each _x in (xs)
--       for each _y in (ys)
--         for each _z in (zs)
--           output(_x * _y + _z);
--
-- TODO: The compiler should perform this translation during a desugaring step.

data BinaryOperator
  -- TODO: Exponent

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
  -- Disjunctive
  | BinaryOr
  -- Implicative
  | BinaryImplies
  -- Assignment
  | BinaryAssign
  deriving stock (Eq, Show)

data Signature anno
  = ApplicationSignature anno !(Signature anno) [Signature anno]
  | ConstructorSignature anno !Identifier
  | FunctionSignature anno [Signature anno] !(Signature anno)
  deriving stock (Eq, Functor, Show)

signatureAnno :: Signature anno -> anno
signatureAnno = \ case
  ApplicationSignature anno _ _ -> anno
  ConstructorSignature anno _   -> anno
  FunctionSignature    anno _ _ -> anno

newtype Identifier = Identifier { getIdentifier :: NonEmpty Text }
  deriving stock (Eq, Ord)

identifierText :: Identifier -> Text
identifierText = Text.intercalate " " . NonEmpty.toList . getIdentifier

instance Show Identifier where
  show = show . Text.unpack . identifierText

instance IsString Identifier where
  -- TODO: Avoid partiality?
  fromString
    = Identifier
    . NonEmpty.fromList
    . filter (not . Text.null)
    . Text.split (== ' ')
    . fromString

instance Pretty Identifier where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

{-
parseProgram :: FilePath -> String -> Either ParseError (Program SourcePos)
parseProgram path source = Parsec.parse programParser path source
  where
    programParser :: Parser (Program SourcePos)
    programParser = Program
      <$> Parsec.between Parsec.spaces Parsec.eof (many statementParser)

    -- A symbol that may appear adjacent to operator characters.
    symbol :: String -> Parser ()
    symbol string = Parsec.try (Parsec.string string) *> Parsec.spaces

    -- A keyword that must not be followed by an identifier character.
    keyword :: String -> Parser ()
    keyword string = Parsec.try
      $ Parsec.string string
      *> Parsec.notFollowedBy (Parsec.satisfy continuesIdentifier)
      *> Parsec.spaces

    -- An operator that must not be followed by an operator character. This
    -- makes all combinations of operator characters reserved for future use.
    operator :: String -> Parser ()
    operator string = Parsec.try
      $ Parsec.string string
      *> Parsec.notFollowedBy (Parsec.satisfy isOperator)
      *> Parsec.spaces

    grouped :: Parser a -> Parser a
    grouped = Parsec.between (symbol "(") (symbol ")")

    namedGrouped :: String -> String -> Parser a -> Parser a
    namedGrouped begin end = Parsec.between
      (symbol "(" <?> begin)
      (symbol ")" <?> end)

    namedBracketed :: String -> String -> Parser a -> Parser a
    namedBracketed begin end = Parsec.between
      (symbol "[" <?> begin)
      (symbol "]" <?> end)

    namedBlocked :: String -> String -> Parser a -> Parser a
    namedBlocked begin end = Parsec.between
      (symbol "{" <?> begin)
      (symbol "}" <?> end)

    quoted :: Parser a -> Parser a
    quoted = Parsec.between (Parsec.char '"') (Parsec.char '"')

    statementParser :: Parser (Statement SourcePos)
    statementParser = (<?> "statement") $ asum

      [ AtomicStatement
        <$> (getSourcePos <* keyword "atomic")
        <*> statementParser

      , AfterStatement
        <$> (getSourcePos <* keyword "after")
        <*> (grouped expressionParser <?> "'after' statement condition")
        <*> statementParser

      , AsLongAsStatement
        <$> (getSourcePos <* keyword "as" <* keyword "long" <* keyword "as")
        <*> (grouped expressionParser <?> "'as long as' statement condition")
        <*> statementParser

      , BlockStatement
        <$> getSourcePos
        <*> namedBlocked "beginning of block" "end of block"
          (many statementParser)

      , EmptyStatement
        <$> (getSourcePos <* symbol ";")

      , do
        pos <- getSourcePos <* keyword "for"
        constructor <- asum
          [ ForAllStatement <$ keyword "all"
          , ForEachStatement <$ keyword "each"
          ] <?> "'for' loop type ('all' or 'each')"
        identifier <- (identifierParser <?> "loop variable name")
          <* keyword "in"
        container <- grouped expressionParser <?> "loop container expression"
        body <- statementParser
        pure $ constructor pos identifier container body

      , FunctionStatement
        <$> (getSourcePos <* keyword "function")
        <*> (identifierParser <?> "function name")
        <*> parameterListParser
        <*> Parsec.optionMaybe (symbol ":" *> signatureParser)
        <*> statementParser

{-
      , IfStatement
        <$> (getSourcePos <* keyword "if")
        <*> (grouped expressionParser <?> "'if' statement condition")
        <*> statementParser
        <*> Parsec.optionMaybe (keyword "else" *> statementParser)
-}

      , LastStatement
        <$> (getSourcePos <* keyword "last")
        <*> (Parsec.optionMaybe (identifierParser <?> "loop label")
          <* symbol ";")

      , NextStatement
        <$> (getSourcePos <* keyword "next")
        <*> (Parsec.optionMaybe (identifierParser <?> "loop label")
          <* symbol ";")

      , do
        pos <- getSourcePos <* keyword "on"
        constructor <- asum
          [ OnChangeStatement <$ keyword "change"
          , OnSetStatement <$ keyword "set"
          -- TODO: add, remove
          ] <?> "event name ('change' or 'set')"
        variables <- grouped (Parsec.sepEndBy identifierParser (symbol ","))
          <?> "event variables"
        body <- statementParser
        pure $ constructor pos variables body

      , RedoStatement
        <$> (getSourcePos <* keyword "redo")
        <*> (Parsec.optionMaybe (identifierParser <?> "loop label")
          <* symbol ";")

      , ReturnStatement
        <$> (getSourcePos <* keyword "return")
        <*> (Parsec.optionMaybe expressionParser <* keyword ";")

      , VarStatement
        <$> (getSourcePos <* keyword "var")
        <*> Parsec.sepBy
          ((,,)
            <$> identifierParser
            <*> Parsec.optionMaybe (symbol ":" *> signatureParser)
            <*> Parsec.optionMaybe (operator "=" *> expressionParser)
            <?> "variable initializer")
          (symbol ",")
        <* symbol ";"

      , WheneverStatement
        <$> (getSourcePos <* keyword "whenever")
        <*> (grouped expressionParser <?> "'whenever' statement condition")
        <*> statementParser

      , WhileStatement
        <$> (getSourcePos <* keyword "while")
        <*> (grouped expressionParser <?> "'while' statement condition")
        <*> statementParser

      -- Must appear last.
      , ExpressionStatement
        <$> getSourcePos
        <*> (expressionParser <* symbol ";")

      ]

    expressionParser :: Parser (Expression SourcePos)
    expressionParser = buildExpressionParser operatorTable termParser
      where

        operatorTable :: [[Expr.Operator String () Identity (Expression SourcePos)]]
        operatorTable =
          -- Prefix
          [ [ prefix (keyword "each") UnaryEach
            , prefix (keyword "every") UnaryEvery
            , prefix (operator "-") UnaryMinus
            , prefix (keyword "not") UnaryNot
            , prefix (operator "+") UnaryPlus
            ]

          -- Multiplicative
          , [ binary (operator "*") Expr.AssocLeft BinaryMultiply
            , binary (operator "/") Expr.AssocLeft BinaryDivide
            , binary (keyword "mod") Expr.AssocLeft BinaryModulus
            ]

          -- Additive
          , [ binary (operator "+") Expr.AssocLeft BinaryAdd
            , binary (operator "-") Expr.AssocLeft BinarySubtract
            ]

          -- Relational
          -- See note [Compound Comparisons].
          , [ binary (operator "<") Expr.AssocLeft BinaryLess
            , binary (operator ">=") Expr.AssocLeft BinaryNotLess
            , binary (operator ">") Expr.AssocLeft BinaryGreater
            , binary (operator "<=") Expr.AssocLeft BinaryNotGreater
            , binary (operator "=") Expr.AssocLeft BinaryEqual
            , binary (operator "<>") Expr.AssocLeft BinaryNotEqual
            , binary
              (Parsec.try (keyword "is" *> keyword "in"))
              Expr.AssocNone BinaryElement
            , binary
              (Parsec.try (keyword "is" *> keyword "not" *> keyword "in"))
              Expr.AssocNone BinaryNotElement
            ]

          -- Conjunctive
          , [ binary (keyword "and") Expr.AssocRight BinaryAnd
            ]

          -- Disjunctive
          , [ binary (keyword "or") Expr.AssocRight BinaryOr
            , binary (keyword "xor") Expr.AssocRight BinaryXor
            ]

          -- Implicative
          , [ binary (keyword "implies") Expr.AssocRight BinaryImplies
            ]

          -- Assignment
          , [ binary (operator "<-") Expr.AssocNone BinaryAssign
            ]

          ]
          where

            prefix
              :: Parser a
              -> UnaryOperator
              -> Expr.Operator String () Identity (Expression SourcePos)
            prefix parser unaryOperator = Expr.Prefix do
              pos <- getSourcePos <* parser
              pure $ UnaryExpression pos unaryOperator

            binary
              :: Parser a
              -> Expr.Assoc
              -> BinaryOperator
              -> Expr.Operator String () Identity (Expression SourcePos)
            binary parser associativity binaryOperator
              = flip Expr.Infix associativity do
              pos <- getSourcePos <* parser
              pure $ BinaryExpression pos binaryOperator

        termParser :: Parser (Expression SourcePos)
        termParser = (<?> "expression term") do
          prefix <- asum

            [ LiteralExpression
              <$> getSourcePos
              <*> literalParser

            , LetExpression
              <$> (getSourcePos <* keyword "let")
              <*> Parsec.sepBy
                ((,,)
                  <$> identifierParser
                  <*> Parsec.optionMaybe (symbol ":" *> signatureParser)
                  <*> (operator "=" *> expressionParser))
                (symbol ",")
              <*> (keyword "in" *> expressionParser)

            , IfExpression
              <$> (getSourcePos <* keyword "if")
              <*> (grouped expressionParser <?> "'if' expression condition")
              <*> (expressionParser <?> "'if' expression true branch")
              <*> (keyword "else"
                *> (expressionParser <?> "'if' expression false branch"))

            , IdentifierExpression
              <$> getSourcePos
              <*> identifierParser

            , GroupExpression <$> getSourcePos <*> grouped expressionParser
            ]

          suffixes <- many expressionSuffixParser
          pure $ applySuffixes suffixes prefix

        character :: Parser Char
        character = (<?> "character") $ Parsec.noneOf "\\\""

        escape :: Parser Char
        escape = (<?> "escape") $ Parsec.char '\\' *> asum
          [ '"' <$ Parsec.char '"'
          , '\\' <$ Parsec.char '\\'
          , '\n' <$ Parsec.char 'n'
          , do
            unknown <- Parsec.anyChar
            Parsec.unexpected $ "unknown escape '" ++ [unknown] ++ "'"
          ]

        expressionSuffixParser :: Parser (Expression SourcePos -> Expression SourcePos)
        expressionSuffixParser = do
          pos <- getSourcePos
          asum
            [ (<?> "subscript suffix") do
              subscripts <- namedBracketed "start of subscript" "end of subscript"
                $ Parsec.sepBy expressionParser (symbol ",")
              pure \ prefix -> SubscriptExpression pos prefix subscripts
            , (<?> "member lookup suffix") do
              identifier <- symbol "." *> identifierParser
              pure \ prefix -> DotExpression pos prefix identifier
            , (<?> "call suffix") do
              arguments <- grouped
                $ Parsec.sepEndBy expressionParser (symbol ",")
              pure \ prefix -> CallExpression pos prefix arguments
            ]

        literalParser :: Parser (Literal SourcePos)
        literalParser = (<?> "literal") $ asum
          [ BooleanLiteral <$> asum
            [ True <$ keyword "true"
            , False <$ keyword "false"
            ]

          , do
            integerPart <- Parsec.many1 Parsec.digit
            mFractionalPart <- Parsec.optionMaybe
              (Parsec.char '.' *> Parsec.many1 Parsec.digit)
              <* Parsec.spaces
            pure case mFractionalPart of
              Just fractionalPart -> FloatLiteral
                $ read $ integerPart ++ "." ++ fractionalPart
              Nothing -> IntegerLiteral
                $ read integerPart

          , TextLiteral <$> textLiteralParser

          , NullLiteral <$ keyword "null"

          , ListLiteral
            <$> namedBracketed "beginning of list" "end of list"
              (asum
                -- See note [Empty Container Literal Comma].
                [ [] <$ symbol ","
                , Parsec.sepEndBy
                  (expressionParser <?> "list element")
                  (symbol ",")
                ])

          , symbol "{" *> do
            keyValuePairs <- asum
              -- See note [Empty Container Literal Comma].
              [ [] <$ symbol ","
              , Parsec.sepEndBy keyValuePair (symbol ",")
              ]
              <* (symbol "}" <?> "end of map/set literal")
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

          , keyword "function"
            *> (FunctionLiteral
              <$> Parsec.optionMaybe (identifierParser <?> "function name")
              <*> parameterListParser
              <*> Parsec.optionMaybe (symbol ":" *> signatureParser)
              <*> statementParser)

          ]
          where

            textLiteralParser :: Parser Text
            textLiteralParser = Text.pack
              <$> (quoted (many (character <|> escape)) <* Parsec.spaces)

            keyValuePair :: Parser (Expression SourcePos, Maybe (Expression SourcePos))
            keyValuePair = do
              pos <- getSourcePos
              asum
                [ (<?> "map key-value pair") $ Parsec.try do
                  key <- LiteralExpression pos . TextLiteral <$> asum
                    [ identifierText <$> identifierParser
                    , textLiteralParser
                    ]
                  value <- symbol ":" *> expressionParser
                  pure (key, Just value)
                , (<?> "map key-value pair") do
                  key <- grouped expressionParser
                  mValue <- Parsec.optionMaybe $ symbol ":" *> expressionParser
                  pure (key, mValue)
                , (<?> "set element") do
                  key <- expressionParser
                  pure (key, Nothing)
                ]

    parameterListParser :: Parser [(Identifier, Maybe (Signature SourcePos), Maybe (Expression SourcePos))]
    parameterListParser
      = namedGrouped "beginning of parameter list" "end of parameter list"
        (Parsec.sepEndBy
          ((,,)
            <$> identifierParser
            <*> Parsec.optionMaybe (symbol ":" *> signatureParser)
            <*> Parsec.optionMaybe (operator "=" *> expressionParser)
            <?> "function parameter")
        (symbol ",") <?> "function parameter list")

    getSourcePos :: Parser SourcePos
    getSourcePos = Parsec.statePos <$> Parsec.getParserState

    identifierParser :: Parser Identifier
    identifierParser = Identifier . (:| []) . Text.pack
      <$> ((:)
        <$> Parsec.satisfy startsIdentifier
        <*> (many (Parsec.satisfy continuesIdentifier) <* Parsec.spaces))

    startsIdentifier :: Char -> Bool
    startsIdentifier = isAsciiLower .|| isAsciiUpper .|| (== '_')

    continuesIdentifier :: Char -> Bool
    continuesIdentifier = startsIdentifier .|| isDigit

    isOperator :: Char -> Bool
    isOperator = (`elem` ("!#$%&*+-./<=>?@\\^|~" :: String))

    signatureParser :: Parser (Signature SourcePos)
    signatureParser = (<?> "type signature") do
      prefix <- asum
        [ do
          pos <- getSourcePos <* keyword "function"
          parameters <- grouped (Parsec.sepEndBy signatureParser (symbol ","))
          result <- symbol ":" *> signatureParser
          pure $ FunctionSignature pos parameters result
        , ConstructorSignature <$> getSourcePos <*> identifierParser
        , grouped signatureParser
        ]
      suffixes <- many signatureSuffixParser
      pure $ applySuffixes suffixes prefix

    -- Currently this only supports type applications such as "list(int)" or
    -- "map(text, int)" but it should be extended as the type system evolves.
    signatureSuffixParser :: Parser (Signature SourcePos -> Signature SourcePos)
    signatureSuffixParser = do
      pos <- getSourcePos
      arguments <- grouped
        $ Parsec.sepEndBy signatureParser (symbol ",")
      pure \ prefix -> ApplicationSignature pos prefix arguments

    applySuffixes :: [a -> a] -> a -> a
    applySuffixes = foldl' (flip (.)) id
-}

-- Note [Empty Container Literal Comma]:
--
-- I'm not yet sure why it's necessary to explicitly handle the case of a
-- container literal containing only a comma, but without it, parsing a list
-- like "[,]" fails.

-- Note [Compound Comparisons]:
--
-- Relational operators are traditionally marked non-associative, making
-- comparisons like "x < y < z" illegal. They are currently marked as
-- left-associative so they can be desugared to compound comparisons, as in
-- Python, where "x1 op1 x2 op2 x3 ... xn" is desugared to:
--
--     let _x1 = x1, _x2 = x2 in
--       (_x1 op1 _x2) and
--       let _x3 = x3 in
--         _x2 op2 _x3 and
--         ...

-- The dynamic representation of a Hap value.
--
-- If you update this, you should also update the 'Eq' and 'Ord' instances.
data Value
  = BooleanValue !Bool
  | FloatValue !Double
  | IntegerValue !Integer
  | TextValue !Text
  | NullValue
  | ListValue [Value]
  | MapValue !(Map Value Value)
  | SetValue !(Set Value)
  | NativeValue !NativeId
  -- TODO: Add static environment.
{-
  | FunctionValue
    !(Maybe Identifier)
    [(Identifier, Maybe (Signature anno), Maybe (Expression anno))]
    !(Maybe (Signature anno))
    !(Statement anno)
    !(Hap IO ())
-}
  deriving stock (Eq, Ord, Show)

{-
instance Eq Value where
  BooleanValue a == BooleanValue b = a == b
  FloatValue a == FloatValue b = a == b
  IntegerValue a == IntegerValue b = a == b
  TextValue a == TextValue b = a == b
  NullValue == NullValue = True
  ListValue a == ListValue b = a == b
  MapValue a == MapValue b = a == b
  SetValue a == SetValue b = a == b
  NativeValue a == NativeValue b = a == b
  -- Function values compare ignoring their compiled representation.
{-
  FunctionValue a b c d _ == FunctionValue e f g h _
    = (a, b, c, d) == (e, f, g, h)
-}
  _ == _ = False

instance Ord Value where
  compare = curry \ case
    (BooleanValue a, BooleanValue b) -> a `compare` b
    (FloatValue a, FloatValue b) -> a `compare` b
    (IntegerValue a, IntegerValue b) -> a `compare` b
    (TextValue a, TextValue b) -> a `compare` b
    (NullValue, NullValue) -> EQ
    (ListValue a, ListValue b) -> a `compare` b
    (MapValue a, MapValue b) -> a `compare` b
    (SetValue a, SetValue b) -> a `compare` b
    (NativeValue a, NativeValue b) -> a `compare` b
    -- Function values compare ignoring their compiled representation.
{-
    (FunctionValue a b c d _, FunctionValue e f g h _)
      -> (a, b, c, d) `compare` (e, f, g, h)
-}
    -- It would be possible to define an arbitrary ordering for comparisons
    -- between values of different types, but if this case is reached, it
    -- probably indicates a typechecker bug.
    (a, b) -> error $ concat
      [ "comparison of incomparable values "
      , show a
      , " and "
      , show b
      ]
-}

instance Pretty Value where
  pretty = \ case
    BooleanValue value -> if value then "true" else "false"
    FloatValue value -> pretty value
    IntegerValue value -> pretty value
    TextValue value -> pretty value
    NullValue -> "null"
    ListValue elements -> Pretty.brackets $ Pretty.hsep
      $ Pretty.punctuate Pretty.comma
      $ pretty <$> elements
    MapValue pairs -> Pretty.braces $ Pretty.hsep
      $ Pretty.punctuate Pretty.comma
      $ prettyPair <$> Map.toList pairs
      where
        prettyPair (key, value)
          = Pretty.hsep [pretty key, "=>", pretty value]
    SetValue elements -> Pretty.braces $ Pretty.hsep
      $ Pretty.punctuate Pretty.comma
      $ pretty <$> Set.toList elements
    NativeValue n -> pretty $ nativeName n
{-
    FunctionValue name parameters result body _compiled -> concat
      [ "function"
      , case name of
        Just identifier -> concat [" ", show identifier]
        Nothing -> ""
      , "("
      , intercalate ", " $ map showParameter parameters
      , ")"
      , case result of
        Just signature -> concat [": ", show signature]
        Nothing -> ""
      , " "
      , show body
      ]
      where
        showParameter (parameterName, parameterType, parameterDefault) = concat
          [ show parameterName
          , case parameterType of
            Just signature -> concat [": ", show signature]
            Nothing -> ""
          , case parameterDefault of
            Just expression -> concat [" = ", show expression]
            Nothing -> ""
          ]
-}

newtype NativeId = NativeId Int
  deriving stock (Eq, Ord, Show)

type NativeFunction m = Env m -> [Value] -> m Value

native :: (MonadIO m) => [(Identifier, NativeFunction m)]
native =
  [ (,) "output" \ env args -> do
    for_ args $ envOutputStr env . \ case
      TextValue text -> Text.unpack text
      arg -> show arg
    pure NullValue
  , (,) "trace" \ env args -> do
    traverse_ (envOutputStr env . (++ "\n") . show . pretty) args
    pure NullValue
  , (,) "graphics_background_set" \ env args -> case args of
    [IntegerValue r, IntegerValue g, IntegerValue b] -> case envGraphicsChan env of
      Just graphicsChan -> do
        liftIO $ atomically $ writeTChan graphicsChan \ renderer
          -> SDL.rendererDrawColor renderer
          $= SDL.V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
        pure NullValue
      -- TODO: Throw Hap graphics error.
      Nothing -> pure NullValue
    -- TODO: Throw Hap argument error.
    _ -> pure NullValue
  , (,) "graphics_clear" \ env args -> case args of
    [] -> case envGraphicsChan env of
      Just graphicsChan -> do
        liftIO $ atomically $ writeTChan graphicsChan SDL.clear
        pure NullValue
      -- TODO: Throw Hap graphics error.
      Nothing -> pure NullValue
    -- TODO: Throw Hap argument error.
    _ -> pure NullValue
  , (,) "graphics_present" \ env args -> case args of
    [] -> case envGraphicsChan env of
      Just graphicsChan -> do
        liftIO $ atomically $ writeTChan graphicsChan SDL.present
        pure NullValue
      -- TODO: Throw Hap graphics error.
      Nothing -> pure NullValue
    -- TODO: Throw Hap argument error.
    _ -> pure NullValue
  ]

nativeIds :: Map Identifier NativeId
nativeIds = Map.fromList
  $ map fst (native @IO) `zip` map NativeId [0..]

nativeNames :: IntMap Identifier
nativeNames = IntMap.fromList
  $ [0..] `zip` map fst (native @IO)

nativeFunctions :: (MonadIO m) => IntMap (NativeFunction m)
nativeFunctions = IntMap.fromList
  $ [0..] `zip` map snd native

nativeId :: Identifier -> Maybe NativeId
nativeId identifier = Map.lookup identifier nativeIds

nativeName :: NativeId -> Identifier
nativeName (NativeId n) = fromMaybe (error "undefined native ID")
  $ IntMap.lookup n nativeNames

nativeFunction :: (MonadIO m) => NativeId -> NativeFunction m
nativeFunction (NativeId n) = fromMaybe (error "undefined native ID")
  $ IntMap.lookup n nativeFunctions
