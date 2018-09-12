{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Hap.Language
  ( BinaryOperator(..)
  , Expression(..)
  , Identifier(..)
  , Literal(..)
  , NativeId
  , Program(..)
  , Signature(..)
  , Statement(..)
  , UnaryOperator(..)
  , Value(..)
  , nativeFunction
  , nativeId
  , nativeName
  , parseProgram
  ) where

import Control.Applicative
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Either (partitionEithers)
import Data.Foldable (asum)
import Data.Functor.Identity (Identity)
import Data.IntMap (IntMap)
import Data.List (foldl', intercalate)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Exts (IsString(..))
import Hap.Operators
import Hap.Runtime (Env(..))
import Text.Parsec (ParseError, (<?>))
import Text.Parsec.Expr (buildExpressionParser)
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.String (Parser)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
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
  | FunctionStatement !SourcePos !Identifier [(Identifier, Maybe Signature, Maybe Expression)] !(Maybe Signature) !Statement
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
  deriving (Eq, Ord, Show)

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
  | IfExpression !SourcePos !Expression !Expression !Expression
  deriving (Eq, Ord, Show)

data Literal
  = BooleanLiteral !Bool
  | FloatLiteral !Double
  | IntegerLiteral !Integer
  | TextLiteral !Text
  | NullLiteral
  | ListLiteral [Expression]
  | MapLiteral [(Expression, Expression)]
  | SetLiteral [Expression]
  | FunctionLiteral !(Maybe Identifier) [(Identifier, Maybe Signature, Maybe Expression)] !(Maybe Signature) !Statement
  deriving (Eq, Ord, Show)

data UnaryOperator
  = UnaryEach
  | UnaryEvery
  | UnaryMinus
  | UnaryNot
  | UnaryPlus
  deriving (Eq, Ord, Show)

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
  | BinaryNotElement
  -- Conjunctive
  | BinaryAnd
  -- Disjunctive
  | BinaryOr
  | BinaryXor
  -- Implicative
  | BinaryImplies
  -- Assignment
  | BinaryAssign
  deriving (Eq, Ord, Show)

data Signature
  = ApplicationSignature !SourcePos !Signature [Signature]
  | ConstructorSignature !SourcePos !Identifier
  | FunctionSignature !SourcePos [Signature] !Signature
  deriving (Eq, Ord, Show)

newtype Identifier = Identifier { identifierText :: Text }
  deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier identifier) = Text.unpack identifier

instance IsString Identifier where
  fromString = Identifier . fromString

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parseProgram :: FilePath -> String -> Either ParseError Program
parseProgram path source = Parsec.parse programParser path source
  where
    programParser :: Parser Program
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

    statementParser :: Parser Statement
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

      -- See note [Dangling Else].
      , IfStatement
        <$> (getSourcePos <* keyword "if")
        <*> (grouped expressionParser <?> "'if' statement condition")
        <*> statementParser
        <*> Parsec.optionMaybe (keyword "else" *> statementParser)

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

    expressionParser :: Parser Expression
    expressionParser = buildExpressionParser operatorTable termParser
      where

        operatorTable :: [[Expr.Operator String () Identity Expression]]
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
              -> Expr.Operator String () Identity Expression
            prefix parser unaryOperator = Expr.Prefix $ do
              pos <- getSourcePos <* parser
              pure $ UnaryExpression pos unaryOperator

            binary
              :: Parser a
              -> Expr.Assoc
              -> BinaryOperator
              -> Expr.Operator String () Identity Expression
            binary parser associativity binaryOperator
              = flip Expr.Infix associativity $ do
              pos <- getSourcePos <* parser
              pure $ BinaryExpression pos binaryOperator

        termParser :: Parser Expression
        termParser = (<?> "expression term") $ do
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
          , do
            unknown <- Parsec.anyChar
            Parsec.unexpected $ "unknown escape '" ++ [unknown] ++ "'"
          ]

        expressionSuffixParser :: Parser (Expression -> Expression)
        expressionSuffixParser = do
          pos <- getSourcePos
          asum
            [ (<?> "subscript suffix") $ do
              subscripts <- namedBracketed "start of subscript" "end of subscript"
                $ Parsec.sepBy expressionParser (symbol ",")
              pure $ \ prefix -> SubscriptExpression pos prefix subscripts
            , (<?> "member lookup suffix") $ do
              identifier <- symbol "." *> identifierParser
              pure $ \ prefix -> DotExpression pos prefix identifier
            , (<?> "call suffix") $ do
              arguments <- grouped
                $ Parsec.sepEndBy expressionParser (symbol ",")
              pure $ \ prefix -> CallExpression pos prefix arguments
            ]

        literalParser :: Parser Literal
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
            pure $ case mFractionalPart of
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

            keyValuePair :: Parser (Expression, Maybe Expression)
            keyValuePair = do
              pos <- getSourcePos
              asum
                [ (<?> "map key-value pair") $ Parsec.try $ do
                  key <- LiteralExpression pos . TextLiteral <$> asum
                    [ identifierText <$> identifierParser
                    , textLiteralParser
                    ]
                  value <- symbol ":" *> expressionParser
                  pure (key, Just value)
                , (<?> "map key-value pair") $ do
                  key <- grouped expressionParser
                  mValue <- Parsec.optionMaybe $ symbol ":" *> expressionParser
                  pure (key, mValue)
                , (<?> "set element") $ do
                  key <- expressionParser
                  pure (key, Nothing)
                ]

    parameterListParser :: Parser [(Identifier, Maybe Signature, Maybe Expression)]
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
    identifierParser = Identifier . Text.pack
      <$> ((:)
        <$> Parsec.satisfy startsIdentifier
        <*> (many (Parsec.satisfy continuesIdentifier) <* Parsec.spaces))

    startsIdentifier :: Char -> Bool
    startsIdentifier = isAsciiLower .|| isAsciiUpper .|| (== '_')

    continuesIdentifier :: Char -> Bool
    continuesIdentifier = startsIdentifier .|| isDigit

    isOperator :: Char -> Bool
    isOperator = (`elem` ("!#$%&*+-./<=>?@\\^|~" :: String))

    signatureParser :: Parser Signature
    signatureParser = (<?> "type signature") $ do
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
    signatureSuffixParser :: Parser (Signature -> Signature)
    signatureSuffixParser = do
      pos <- getSourcePos
      arguments <- grouped
        $ Parsec.sepEndBy signatureParser (symbol ",")
      pure $ \ prefix -> ApplicationSignature pos prefix arguments

    applySuffixes :: [a -> a] -> a -> a
    applySuffixes = foldl' (flip (.)) id

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

-- Note [Dangling Else]:
--
-- The "dangling else" is resolved as in C, bound to the nearest 'if'.
-- Unfortunately, this can cause misleading code:
--
--     if (a)
--         if (b)
--             c ();
--     else
--         d ();
--
-- This is parsed as:
--
--     if (a) {
--         if (b) {
--             c ();
--         } else {
--             d ();
--         }
--     }
--
-- TODO: The compiler should add a warning when a control statement is nested
-- directly inside another control statement without being wrapped in a block
-- to help avoid this.

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
    [(Identifier, Maybe Signature, Maybe Expression)]
    !(Maybe Signature)
    !Statement
    !(Hap IO ())
-}

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
  compare = curry $ \ case
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

instance Show Value where
  show = \ case
    BooleanValue value -> if value then "true" else "false"
    FloatValue value -> show value
    IntegerValue value -> show value
    TextValue value -> Text.unpack value
    NullValue -> "null"
    ListValue elements -> concat
      [ "["
      , intercalate ", " $ map show elements
      , "]"
      ]
    MapValue pairs -> concat
      [ "{ "
      , intercalate ", " $ map showPair $ Map.toList pairs
      , " }"
      ]
      where
        showPair (key, value) = concat [show key, ": ", show value]
    SetValue elements -> concat
      [ "{ "
      , intercalate ", " $ map show $ Set.toList elements
      , " }"
      ]
    NativeValue n -> show $ nativeName n
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
  deriving (Eq, Ord, Show)

type NativeFunction m = Env m -> [Value] -> m Value

native :: (Monad m) => [(Identifier, NativeFunction m)]
native =
  [ (,) "output" $ \ env args -> do
    mapM_ (envOutputStr env . (++ "\n") . show) args
    pure NullValue
  ]

nativeIds :: Map Identifier NativeId
nativeIds = Map.fromList
  $ map fst (native @Identity) `zip` map NativeId [0..]

nativeNames :: IntMap Identifier
nativeNames = IntMap.fromList
  $ [0..] `zip` map fst (native @Identity)

nativeFunctions :: (Monad m) => IntMap (NativeFunction m)
nativeFunctions = IntMap.fromList
  $ [0..] `zip` map snd native

nativeId :: Identifier -> Maybe NativeId
nativeId identifier = Map.lookup identifier nativeIds

nativeName :: NativeId -> Identifier
nativeName (NativeId n) = fromMaybe (error "undefined native ID")
  $ IntMap.lookup n nativeNames

nativeFunction :: (Monad m) => NativeId -> NativeFunction m
nativeFunction (NativeId n) = fromMaybe (error "undefined native ID")
  $ IntMap.lookup n nativeFunctions
