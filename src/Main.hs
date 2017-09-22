{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecursiveDo #-}

module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception (Exception, throwIO)
import Control.Monad
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Either (partitionEithers)
import Data.Foldable (asum)
import Data.Functor.Identity (Identity)
import Data.IORef
import Data.IntSet (IntSet)
import Data.List (find, foldl', stripPrefix)
import Data.Text (Text)
import Data.Typeable (Typeable)
import System.Console.Haskeline (getInputLine, outputStrLn, runInputT)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPrint, hPutStrLn, stderr)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem.Weak
import Text.Parsec (ParseError)
import Text.Parsec.Expr (buildExpressionParser)
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.String (Parser)
import qualified Data.IntSet as IntSet
import qualified Data.Text as Text
import qualified System.Console.Haskeline as Haskeline
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as Expr

--------------------------------------------------------------------------------
-- Entry Point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [path] -> do
      source <- readFile path
      case parseProgram path source of
        Left parseError -> do
          hPrint stderr parseError
          exitWith $ ExitFailure 1
        Right program -> do
          print program
    _ -> do
      hPutStrLn stderr "Usage: hap [source]"
      exitWith $ ExitFailure 1

runRepl :: IO ()
runRepl = runInputT Haskeline.defaultSettings loop
  where
    loop = do
      entry <- getInputLine "> "
      case entry of
        Nothing -> do
          outputStrLn "Bye!"
        Just line -> case stripPrefix "//" line of
          Just command -> case command of
            "quit" -> do
              outputStrLn "Bye!"
            _ -> do
              outputStrLn $ "Unknown command '" ++ command ++ "'."
              loop
          Nothing -> do
            outputStrLn "Parsing..."
            case parseProgram "<interactive>" line of
              Left parseError -> do
                outputStrLn "Parse error."
                outputStrLn $ show parseError
                loop
              Right program -> do
                outputStrLn "Parsed successfully."
                outputStrLn $ show program
                loop

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- The environment contains a set of event listeners, a source of fresh IDs for
-- cells and handlers, and a queue of actions scheduled to be run at the next
-- sequence point.
data Env = Env
  { envListeners :: !(IORef [(Id, IntSet, Handler)])
  , envNext :: !(IORef Id)
  , envQueue :: !(IORef [Exp ()])
  }

-- An ID is a globally unique integer used to identify cells and listeners.
type Id = Int

-- A cell has an identifier, an expression, a cached value, a set of references
-- to cells that it reads, and a set of weak references to cells that read it.
data Cell a = Cell
  { cellId :: !Id
  , cellExpression :: !(IORef (Exp a))
  , cellCache :: !(IORef (Cache a))
  , cellReads :: !(IORef [SomeCell])
  , cellObservers :: !(IORef [WeakCell])
  }

-- The cache of a cell may be 'Empty' if the cell's value has not yet been
-- computed, 'Full' if it stores the cached result of the most recent
-- evaluation, or 'Blackhole' if it's in the process of being evaluated. If a
-- 'Blackhole' is encountered when reading a cell with 'get', this indicates a
-- reference cycle and a 'Cycle' exception is raised.
data Cache a = Empty | Full a | Blackhole

-- The exception raised when a dependency cycle is detected.
data Cycle = Cycle !SomeCell
  deriving (Typeable)

-- A weak cell is a weak reference to a cell. Cells use weak references to track
-- their observers (the cells that need to be notified when the current cell is
-- invalidated) because otherwise the dataflow graph would be fully connected
-- and no memory would ever be reclaimed.
data WeakCell = forall a. WeakCell (Weak (Cell a))

-- A cell with a hidden type. This is used to work with heterogeneous
-- collections of cells.
data SomeCell = forall a. SomeCell (Cell a)

-- A 'Handler' is an expression enqueued in response to an event, tagged with
-- the event type for filtering events. A 'Set' event indicates that a cell was
-- written.
--
-- TODO: 'Add' and 'Remove' indicate that a value was inserted into or removed
-- from a cell whose value is a container.
data Handler
  = Set !(Exp ())
  | Add !(Exp ())
  | Remove !(Exp ())

-- An expression may read and alter the contents of the environment, and perform
-- I/O. It returns a result as well as a list of references to the cells that it
-- reads while computing a result.
newtype Exp a = Exp { unExp :: Env -> IO (a, [SomeCell]) }

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
-- Environment Operations
--------------------------------------------------------------------------------

-- Create a new empty environment.
newEmptyEnv :: IO Env
newEmptyEnv = do
  listeners <- newIORef []
  next <- newIORef (0 :: Id)
  queue <- newIORef []
  pure Env
    { envListeners = listeners
    , envNext = next
    , envQueue = queue
    }

-- Run a computation in the given environment.
run :: Env -> Exp a -> IO a
run env (Exp action) = fst <$> action env

--------------------------------------------------------------------------------
-- Cell Operations
--------------------------------------------------------------------------------

-- Allocate a fresh cell ID.
newId :: Exp Id
newId = Exp $ \ env -> do
  x <- readIORef $ envNext env
  writeIORef (envNext env) (x + 1)
  pure (x, [])

-- Allocate a new cell containing the given expression.
cell :: Exp a -> Exp (Cell a)
cell exp = do
  n <- newId
  Exp $ \ env -> do
    code <- newIORef exp
    cache <- newIORef Empty
    reads <- newIORef []
    observers <- newIORef []
    let
      cell = Cell
        { cellId = n
        , cellExpression = code
        , cellCache = cache
        , cellReads = reads
        , cellObservers = observers
        }
    pure (cell, [])

-- Get the value of a cell.
get :: Cell a -> Exp a
get c = Exp $ \ env -> do
  cache <- readIORef (cellCache c)
  case cache of
    Full v -> pure (v, [SomeCell c])
    Empty -> do
      -- Replace the cache with a black hole during evaluation to detect
      -- reference cycles.
      writeIORef (cellCache c) Blackhole
      (v, ds) <- join $ unExp <$> readIORef (cellExpression c) <*> pure env
      writeIORef (cellCache c) (Full v)
      writeIORef (cellReads c) ds
      wc <- makeWeakCell c
      forM_ ds $ \ (SomeCell d) -> modifyIORef' (cellObservers d) (wc :)
      pure (v, [SomeCell c])
    Blackhole -> throwIO $ Cycle $ SomeCell c

-- Set the value of a cell to a new expression.
set :: Cell a -> Exp a -> Exp ()
set c exp = Exp $ \ env -> do
  writeIORef (cellExpression c) exp
  let sc = SomeCell c
  invalidate env sc
  pure ((), [])

-- Get the ID of a cell with a hidden type.
someCellId :: SomeCell -> Id
someCellId (SomeCell c) = cellId c

-- Get the ID of a weak cell if it hasn't expired.
weakCellId :: WeakCell -> IO (Maybe Id)
weakCellId = fmap (fmap someCellId) . strengthen

-- Make a weak reference to a cell.
makeWeakCell :: Cell a -> IO WeakCell
makeWeakCell c = WeakCell <$> mkWeakPtr c Nothing

-- Convert a weak cell into a cell reference if it hasn't expired.
strengthen :: WeakCell -> IO (Maybe SomeCell)
strengthen (WeakCell wc) = do
  mc <- deRefWeak wc
  pure $ case mc of
    Just c -> Just $ SomeCell c
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Low-level Event Operations
--------------------------------------------------------------------------------

-- Add an event listener for the given cells and return the listener's ID.
on :: IntSet -> Handler -> Exp Id
on cells handler = do
  n <- newId
  Exp $ \ env -> do
    modifyIORef' (envListeners env) ((n, cells, handler) :)
    pure (n, [])

-- Removes the event listener with the given ID.
--
-- TODO: Return the old listener so it can be restarted. (That could also be
-- implemented with a per-listener flag for pausing & resuming.)
stop :: Id -> Exp ()
stop listener = Exp $ \ env -> do
  modifyIORef' (envListeners env) $ filter
    $ \ (listener', _, _) -> listener' /= listener
  pure ((), [])

-- Add an action to be run when any of the given cells is set.
onSet :: [Cell a] -> Exp () -> Exp Id
onSet cells = on (IntSet.fromList $ map cellId cells) . Set

-- Add an action to be run when any of the given cells is changed, that is, when
-- it is set and the new value is not equal to the old value.
--
-- TODO: Implement this as a function or macro within Hap.
onChange :: (Eq a) => [Cell a] -> Exp () -> Exp Id
onChange cells action = do
  values <- mapM get cells
  state <- cell $ pure values
  onSet cells $ do
    values' <- mapM get cells
    state' <- get state
    set state $ pure values'
    when (values' /= state') action

-- Remove an observer from a cell.
removeObserver :: SomeCell -> SomeCell -> IO ()
removeObserver o (SomeCell c) = do
  observers <- readIORef (cellObservers c)
  observers' <- flip filterM observers $ \ o' -> do
    mi <- weakCellId o'
    case mi of
      Just i -> pure (someCellId o /= i)
      -- Remove expired observers as a side effect.
      Nothing -> pure False
  writeIORef (cellObservers c) observers'

-- Invalidate the dependencies and dependents of a cell.
invalidate :: Env -> SomeCell -> IO ()
invalidate env sc@(SomeCell c) = do
  os <- readIORef $ cellObservers c
  rs <- readIORef $ cellReads c
  writeIORef (cellObservers c) []
  writeIORef (cellCache c) Empty
  writeIORef (cellReads c) []
  forM_ rs $ removeObserver sc
  forM_ os $ invalidateWeak env
  notifySet env sc

-- Invalidate a weak cell if it's not expired.
invalidateWeak :: Env -> WeakCell -> IO ()
invalidateWeak env = mapM_ (invalidate env) <=< strengthen

-- Notify the environment that a cell was written.
notifySet :: Env -> SomeCell -> IO ()
notifySet env sc@(SomeCell c) = do
  let i = someCellId sc
  listeners <- readIORef $ envListeners env
  -- Listeners are evaluated in the order they were added.
  forM_ (reverse listeners) $ \ (n, cells, handler) -> do
    when (IntSet.member i cells) $ case handler of
      Set action -> enqueue env action
      _ -> pure ()

-- Enqueue an action to be executed at the next sequence point.
enqueue :: Env -> Exp () -> IO ()
enqueue env action = do
  modifyIORef' (envQueue env) (action :)

--------------------------------------------------------------------------------
-- High-level Event Operations
--------------------------------------------------------------------------------

-- Add an action to be run whenever a condition becomes true, that is, when it
-- changes from false to true. If true initially, the action is also run as soon
-- as the handler is added.
whenever :: Exp Bool -> Exp () -> Exp Id
whenever condition action = do
  current <- cell condition
  initial <- get current
  previous <- cell $ pure initial
  when initial action
  onChange [current] $ do
    previous' <- get previous
    current' <- get current
    set previous $ pure current'
    when (current' && not previous') action

-- Add an action to be run the first time a condition becomes true, after which
-- it's removed. If true initially, the action is run immediately and the
-- listener is not added. Returns the ID of the listener if one was added; it
-- can be stopped before the action has had a chance to run.
--
-- TODO: Implement this as a function or macro within Hap.
after :: Exp Bool -> Exp () -> Exp (Maybe Id)
after condition action = do
  current <- cell condition
  initial <- get current
  if initial
    then do
      action
      pure Nothing
    else do
      state <- cell $ pure False
      rec
        handler <- whenever condition $ do
          state' <- get state
          when (not state') $ do
            action
            stop handler
          set state $ pure True
      pure $ Just handler

-- Flush the queue of actions to be executed. Since these actions may enqueue
-- further actions, the whole queue is flushed at once to prevent infinite
-- loops; the feedback loop between enqueuing and flushing actions is what
-- drives evaluation.
sequencePoint :: Env -> IO ()
sequencePoint env = do
  queue <- readIORef (envQueue env)
  writeIORef (envQueue env) []
  forM_ (reverse queue) $ run env

--------------------------------------------------------------------------------
-- Typeclass Instances
--------------------------------------------------------------------------------

-- Map over the result of an expression.
instance Functor Exp where
  fmap f (Exp action) = Exp $ \ env -> do
    (result, reads) <- action env
    pure (f result, reads)

-- Embed values in an expression or join expressions by function application.
instance Applicative Exp where
  pure x = Exp (\ env -> pure (x, []))
  Exp mf <*> Exp mx = Exp $ \ env -> do
    (f, reads) <- mf env
    (x, reads') <- mx env
    pure (f x, union reads reads')

-- Sequence expressions, introducing a sequence point to flush the queue.
instance Monad Exp where
  return = pure
  Exp cmd >>= f = Exp $ \ env -> do
    (a, cs) <- cmd env
    sequencePoint env
    (b, ds) <- unExp (f a) env
    pure (b, union cs ds)

-- Arbitrary I/O actions can be executed in an expression.
--
-- FIXME: This exposes evaluation order; I/O actions should not be allowed in
-- "pure" expressions (e.g., event conditions), even though they use I/O
-- internally. It also allows weak cel references: an expression can read a cell
-- but not record the dependency by wrapping the read in 'liftIO' + 'run'.
instance MonadIO Exp where
  liftIO action = Exp $ \ env -> do
    result <- action
    pure (result, [])

-- This allows the body of a handler to refer to the handler itself, e.g., to
-- implement automatic stopping.
instance MonadFix Exp where
  mfix action = Exp $ \ env -> do
    signal <- newEmptyMVar
    argument <- unsafeInterleaveIO $ takeMVar signal
    (result, reads) <- unExp (action argument) env
    putMVar signal result
    pure (result, reads)

instance Exception Cycle

instance Show Cycle where
  show (Cycle cell) = concat
    [ "circular reference detected at cell #"
    , show $ someCellId cell
    ]

instance Show SomeCell where
  show (SomeCell c) = "#" ++ show (cellId c)

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

--------------------------------------------------------------------------------
-- Utility & Convenience Functions
--------------------------------------------------------------------------------

-- The union of two sets of cells with hidden types, removing duplicates.
union :: [SomeCell] -> [SomeCell] -> [SomeCell]
union xs ys = case xs of
  [] -> ys
  x : xs' -> case find (\ y -> someCellId x == someCellId y) ys of
    Just{} -> union xs' ys
    Nothing -> x : union xs' ys

(.>) :: (Applicative f, Ord a) => f a -> f a -> f Bool
(.>) = liftA2 (>)

(.||) :: (Applicative f) => f Bool -> f Bool -> f Bool
(.||) = liftA2 (||)
