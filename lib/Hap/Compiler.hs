{-# LANGUAGE RecursiveDo #-}

module Hap.Compiler
  ( SymbolMap
  , Context
  , compile
  , newEmptyContext
  ) where

import Control.Monad (join, void, when)
import Control.Monad.IO.Class
import Data.Char (ord)
import Data.Fixed (mod')
import Data.IORef
import Data.List ((\\), foldl', intersect, union)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Traversable (for)
import Hap.Language
import Hap.Runtime
import Text.Read (readMaybe)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

type SymbolMap m = Map Identifier (Cell m Value)

data Context m = Context
  { contextModifySymbols :: !((SymbolMap m -> SymbolMap m) -> HapT m ())
  , contextReadSymbols :: !(HapT m (SymbolMap m))
  }

newEmptyContext :: (MonadIO m) => IO (Context m)
newEmptyContext = do
  symbols <- newIORef mempty
  pure Context
    { contextModifySymbols = \ f -> liftIO $ modifyIORef' symbols f
    , contextReadSymbols = liftIO $ readIORef symbols
    }

compile
  :: (MonadIO m, Show anno)
  => Context m
  -> Program anno
  -> Either CompileError (HapT m ())
compile context (Program statements)
  = fmap sequence_ (traverse (compileStatement context) statements)

type CompileError = String

compileStatement
  :: forall m anno
  . (MonadIO m, Show anno)
  => Context m
  -> Statement anno
  -> Either CompileError (HapT m ())
compileStatement context statement = case statement of
{-
  AtomicStatement !SourcePos !Statement
-}
  AfterStatement _pos condition body -> do
    compiledCondition <- compileExpression context condition
    compiledBody <- compileStatement context body
    pure do
      -- FIXME: Should be an expression to return the ID?
      _id <- after compiledCondition compiledBody
      pure ()
{-
  AsLongAsStatement !SourcePos !Expression !Statement
-}
  BlockStatement _pos statements -> do
    compiledStatements <- traverse (compileStatement context) statements
    pure do
      sequence_ compiledStatements
      -- sequencePoint
  EmptyStatement _pos -> do
    pure $ pure ()
  ExpressionStatement _pos expression -> do
    compiledExpression <- compileExpression context expression
    pure do
      void compiledExpression
      sequencePoint

{-
  ForAllStatement !SourcePos !Identifier !Expression !Statement
  ForEachStatement !SourcePos !Identifier !Expression !Statement
  FunctionStatement !SourcePos !Identifier [(Identifier, Maybe Signature)] Statement
  IfStatement !SourcePos !Expression !Statement !(Maybe Statement)
  LastStatement !SourcePos !(Maybe Identifier)
  NextStatement !SourcePos !(Maybe Identifier)
-}
  OnChangeStatement pos vars body
    -> compileOnStatement context onChange pos vars body
  OnSetStatement pos vars body
    -> compileOnStatement context onSet pos vars body
{-
  -- OnAddStatement !Identifier !Identifier !Statement
  -- OnRemoveStatement !Identifier !Identifier !Statement
  RedoStatement !SourcePos !(Maybe Identifier)
  ReturnStatement !SourcePos !(Maybe Expression)
-}

  VarStatement _pos bindings -> do
    compiledBindings <- traverse compileBinding bindings
    let modifySymbols = contextModifySymbols context
    pure do
      initializers <- traverse runInitializer compiledBindings
      modifySymbols \ symbols0 -> foldl'
        (\ symbols (identifier, value) -> Map.insert identifier value symbols)
        symbols0
        initializers
    where

      compileBinding
        :: Binding anno
        -> Either CompileError (CompiledBinding m anno)
      compileBinding Binding
        { bindingAnno
        , bindingName
        , bindingSignature
        , bindingInitializer
        }
        = do
          compiledExpression <- for bindingInitializer
            $ compileExpression context
          pure CompiledBinding
            { compiledBindingAnno        = bindingAnno
            , compiledBindingName        = bindingName
            , compiledBindingSignature   = bindingSignature
            , compiledBindingInitializer = compiledExpression
            }

      runInitializer
        :: CompiledBinding m anno
        -> HapT m (Identifier, Cell m Value)
      runInitializer CompiledBinding
        { compiledBindingName
        , compiledBindingInitializer
        } = do
        -- FIXME: This copies the current value of the initializer; should it
        -- generate a reactive binding instead? I.e., after "var x = y", if 'y'
        -- changes, should 'x' change accordingly?
        value <- case compiledBindingInitializer of
          Just expression -> expression <* sequencePoint
          Nothing -> pure NullValue
        -- TODO: Check value against type signature.
        cell <- new (Just nameString) $ pure value
        pure (compiledBindingName, cell)
        where
          -- TODO: Avoid unpacking text.
          nameString = Text.unpack $ identifierText compiledBindingName

  WheneverStatement _pos condition body -> do
    compiledCondition <- compileExpression context condition
    compiledBody <- compileStatement context body
    pure do
      _ <- whenever compiledCondition compiledBody
      pure ()

{-
  WhileStatement !SourcePos !Expression !Statement
-}
  _ -> Left $ "TODO: compile statement: " ++ show statement

data CompiledBinding m anno = CompiledBinding
  { compiledBindingAnno        :: anno
  , compiledBindingName        :: !Identifier
  , compiledBindingSignature   :: !(Maybe (Signature anno))
  , compiledBindingInitializer :: !(Maybe (HapT m Value))
  }

compileOnStatement
  :: (MonadIO m, Show anno)
  => Context m
  -> ([Cell m Value] -> HapT m () -> HapT m a)
  -> anno
  -> NonEmpty Identifier
  -> Statement anno
  -> Either CompileError (HapT m ())
compileOnStatement context statement pos vars body = do
  let readSymbols = contextReadSymbols context
  compiledBody <- compileStatement context body
  pure do
    symbols <- readSymbols
    cells <- for vars \ var -> case Map.lookup var symbols of
      Just cell -> pure cell
      -- TODO: Raise 'Unbound' Hap error.
      Nothing -> error $ concat ["unbound name '", show var, "'"]
    void $ statement (NonEmpty.toList cells) compiledBody
    pure ()

compileExpression
  :: (MonadIO m, Show anno)
  => Context m
  -> Expression anno
  -> Either CompileError (HapT m Value)
compileExpression context expression = case expression of
  LiteralExpression _pos literal -> case literal of
    BooleanLiteral value -> pure $ pure $ BooleanValue value
    DecimalIntegerLiteral parts
      -> pure $ pure $ IntegerValue value
      where
        digits = fmap (toEnum . (+ ord '0') . fromEnum)
          $ NonEmpty.toList $ join $ snd <$> parts
        value = case readMaybe digits of
          Just x -> x
          -- TODO: Use proper error reporting or remove partiality.
          Nothing -> error
            "internal error: non-digits in decimal integer literal"
    FloatLiteral value -> pure $ pure $ FloatValue value
    ListLiteral elements -> do
      compiledElements <- traverse (compileExpression context) elements
      pure do
        elementValues <- sequence compiledElements
        pure $ ListValue elementValues
    MapLiteral pairs -> do
      compiledPairs <- traverse compilePair pairs
      pure do
        pairValues <- traverse evalPair compiledPairs
        pure $ MapValue $ Map.fromList pairValues
      where
        compilePair (key, value) = (,)
          <$> compileExpression context key
          <*> compileExpression context value
        evalPair (key, value) = (,) <$> key <*> value
    NullLiteral -> pure $ pure NullValue
    SetLiteral elements -> do
      compiledElements <- traverse (compileExpression context) elements
      pure do
        elementValues <- sequence compiledElements
        pure $ SetValue $ Set.fromList elementValues
    TextLiteral value -> pure $ pure $ TextValue value

{-
    FunctionLiteral name parameters result body -> do
      compiledBody <- compileStatement context body
      pure $ pure $ FunctionValue name parameters result body compiledBody
-}
  IdentifierExpression _pos identifier -> do
    let readSymbols = contextReadSymbols context
    pure do
      symbols <- readSymbols
      case Map.lookup identifier symbols of
        Just cell -> get cell
        Nothing -> case nativeId identifier of
          Just native -> pure $ NativeValue native
          -- TODO: Raise 'Unbound' Hap error.
          Nothing -> error $ concat ["unbound name '", show identifier, "'"]
{-
  SubscriptExpression !SourcePos !Expression [Expression]
  DotExpression !SourcePos !Expression !Identifier
-}
  CallExpression _pos function arguments -> do
    compiledFunction <- compileExpression context function
    compiledArguments <- traverse (compileExpression context) arguments
    pure do
      functionValue <- compiledFunction
      argumentValues <- sequence compiledArguments
      call functionValue argumentValues
    where
      call (NativeValue f) xs = HapT \ env -> do
        result <- nativeFunction f env xs
        pure (result, [])
      call _ _ = error "TODO: call non-native function"
{-
  LetExpression !SourcePos [(Identifier, Maybe Signature, Expression)] !Expression
-}
  GroupExpression _pos body -> do
    compiledBody <- compileExpression context body
    pure $ compiledBody
  UnaryExpression _pos operator operand -> do
    compiledOperand <- compileExpression context operand
    pure do
      operandValue <- compiledOperand
      pure case operator of
        -- See note [Each and Every].
        UnaryEach -> error "TODO: implement unary 'each'"
        UnaryEvery -> error "TODO: implement unary 'every'"
        UnaryMinus -> case operandValue of
          IntegerValue value -> IntegerValue $ -value
          FloatValue value -> FloatValue $ -value
          _ -> error "argument of unary '-' not a number"
        UnaryNot -> case operandValue of
          BooleanValue value -> BooleanValue $ not value
          _ -> error "argument of unary 'not' not a boolean"
        UnaryPlus -> case operandValue of
          IntegerValue{} -> operandValue
          FloatValue{} -> operandValue
          _ -> error "argument of unary '+' not a number"
  BinaryExpression _pos operator first second -> do
    compiledFirst <- compileExpression context first
    compiledSecond <- compileExpression context second
    pure case operator of

      -- TODO: function * function = compose with multiplication
      BinaryMultiply -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (IntegerValue a, IntegerValue b) -> pure $ IntegerValue $ a * b
          (FloatValue a, FloatValue b) -> pure $ FloatValue $ a * b
          (TextValue a, IntegerValue b)
            -> pure $ TextValue $ mconcat $ replicate (fromIntegral b) a
          (IntegerValue a, TextValue b)
            -> pure $ TextValue $ mconcat $ replicate (fromIntegral a) b
          (ListValue a, IntegerValue b)
            -> pure $ ListValue $ mconcat $ replicate (fromIntegral b) a
          (IntegerValue a, ListValue b)
            -> pure $ ListValue $ mconcat $ replicate (fromIntegral a) b
          _ -> error "invalid argument to binary '+'"

      -- TODO: function / function = compose with division
      BinaryDivide -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (IntegerValue a, IntegerValue b) -> pure $ IntegerValue $ a `div` b
          (IntegerValue a, FloatValue b) -> pure $ FloatValue $ fromIntegral a / b
          (FloatValue a, IntegerValue b) -> pure $ FloatValue $ a / fromIntegral b
          (FloatValue a, FloatValue b) -> pure $ FloatValue $ a / b
          _ -> error "argument of binary '/' not a number"

      -- TODO: function mod function = compose with modulus
      BinaryModulus -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (IntegerValue a, IntegerValue b) -> pure $ IntegerValue $ a `mod` b
          (IntegerValue a, FloatValue b) -> pure $ FloatValue $ fromIntegral a `mod'` b
          (FloatValue a, IntegerValue b) -> pure $ FloatValue $ a `mod'` fromIntegral b
          (FloatValue a, FloatValue b) -> pure $ FloatValue $ a `mod'` b
          _ -> error "argument of binary 'mod' not a number"

      -- TODO: function + function = compose with addition
      BinaryAdd -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (IntegerValue a, IntegerValue b) -> pure $ IntegerValue $ a + b
          (FloatValue a, FloatValue b) -> pure $ FloatValue $ a + b
          (TextValue a, TextValue b) -> pure $ TextValue $ a <> b
          (ListValue a, ListValue b) -> pure $ ListValue $ a <> b
          (SetValue a, SetValue b) -> pure $ SetValue $ a <> b
          (MapValue a, MapValue b) -> pure $ MapValue $ Map.unionWith (flip const) a b
          _ -> error "invalid argument to binary '+'"

      -- TODO: function - function = compose with subtraction
      BinarySubtract -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (IntegerValue a, IntegerValue b) -> pure $ IntegerValue $ a - b
          (FloatValue a, FloatValue b) -> pure $ FloatValue $ a - b
          (SetValue a, SetValue b) -> pure $ SetValue $ a `Set.difference` b
          (MapValue a, MapValue b) -> pure $ MapValue $ a `Map.difference` b
          (MapValue a, SetValue b) -> pure $ MapValue $ foldr Map.delete a $ Set.toList b
          _ -> error "invalid argument to binary '-'"

      -- TODO: function < function = compose with less than
      BinaryLess -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (BooleanValue a, BooleanValue b) -> pure $ BooleanValue $ a < b
          (IntegerValue a, IntegerValue b) -> pure $ BooleanValue $ a < b
          (FloatValue a, FloatValue b) -> pure $ BooleanValue $ a < b
          (TextValue a, TextValue b) -> pure $ BooleanValue $ a < b
          (ListValue a, ListValue b) -> pure $ BooleanValue $ a < b
          (MapValue a, MapValue b) -> pure $ BooleanValue $ a `Map.isProperSubmapOf` b
          (SetValue a, SetValue b) -> pure $ BooleanValue $ a `Set.isProperSubsetOf` b
          _ -> error "invalid argument to binary '<'"

      BinaryNotLess -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (BooleanValue a, BooleanValue b) -> pure $ BooleanValue $ a >= b
          (IntegerValue a, IntegerValue b) -> pure $ BooleanValue $ a >= b
          (FloatValue a, FloatValue b) -> pure $ BooleanValue $ a >= b
          (TextValue a, TextValue b) -> pure $ BooleanValue $ a >= b
          (ListValue a, ListValue b) -> pure $ BooleanValue $ a >= b
          (MapValue a, MapValue b) -> pure $ BooleanValue $ b `Map.isSubmapOf` a
          (SetValue a, SetValue b) -> pure $ BooleanValue $ b `Set.isSubsetOf` a
          _ -> error "invalid argument to binary '>='"

      BinaryGreater -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (BooleanValue a, BooleanValue b) -> pure $ BooleanValue $ a > b
          (IntegerValue a, IntegerValue b) -> pure $ BooleanValue $ a > b
          (FloatValue a, FloatValue b) -> pure $ BooleanValue $ a > b
          (TextValue a, TextValue b) -> pure $ BooleanValue $ a > b
          (ListValue a, ListValue b) -> pure $ BooleanValue $ a > b
          (MapValue a, MapValue b) -> pure $ BooleanValue $ b `Map.isProperSubmapOf` a
          (SetValue a, SetValue b) -> pure $ BooleanValue $ b `Set.isProperSubsetOf` a
          _ -> error "invalid argument to binary '>'"

      BinaryNotGreater -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (BooleanValue a, BooleanValue b) -> pure $ BooleanValue $ a <= b
          (IntegerValue a, IntegerValue b) -> pure $ BooleanValue $ a <= b
          (FloatValue a, FloatValue b) -> pure $ BooleanValue $ a <= b
          (TextValue a, TextValue b) -> pure $ BooleanValue $ a <= b
          (ListValue a, ListValue b) -> pure $ BooleanValue $ a <= b
          (MapValue a, MapValue b) -> pure $ BooleanValue $ a `Map.isSubmapOf` b
          (SetValue a, SetValue b) -> pure $ BooleanValue $ a `Set.isSubsetOf` b
          _ -> error "invalid argument to binary '<='"

      BinaryEqual -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (BooleanValue a, BooleanValue b) -> pure $ BooleanValue $ a == b
          (IntegerValue a, IntegerValue b) -> pure $ BooleanValue $ a == b
          (FloatValue a, FloatValue b) -> pure $ BooleanValue $ a == b
          (TextValue a, TextValue b) -> pure $ BooleanValue $ a == b
          (ListValue a, ListValue b) -> pure $ BooleanValue $ a == b
          (MapValue a, MapValue b) -> pure $ BooleanValue $ a == b
          (SetValue a, SetValue b) -> pure $ BooleanValue $ a == b
          (NullValue, NullValue) -> pure $ BooleanValue True
          (_, NullValue) -> pure $ BooleanValue False
          (NullValue, _) -> pure $ BooleanValue False
          _ -> error "invalid argument to binary '='"

      BinaryNotEqual -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (BooleanValue a, BooleanValue b) -> pure $ BooleanValue $ a /= b
          (IntegerValue a, IntegerValue b) -> pure $ BooleanValue $ a /= b
          (FloatValue a, FloatValue b) -> pure $ BooleanValue $ a /= b
          (TextValue a, TextValue b) -> pure $ BooleanValue $ a /= b
          (ListValue a, ListValue b) -> pure $ BooleanValue $ a /= b
          (MapValue a, MapValue b) -> pure $ BooleanValue $ a /= b
          (SetValue a, SetValue b) -> pure $ BooleanValue $ a /= b
          (NullValue, NullValue) -> pure $ BooleanValue False
          (_, NullValue) -> pure $ BooleanValue True
          (NullValue, _) -> pure $ BooleanValue True
          _ -> error "invalid argument to binary '<>'"

      BinaryElement -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (a, SetValue b) -> pure $ BooleanValue $ a `Set.member` b
          (a, ListValue b) -> pure $ BooleanValue $ a `elem` b
          (a, MapValue b) -> pure $ BooleanValue $ a `Map.member` b
          (TextValue a, TextValue b) -> pure $ BooleanValue $ a `Text.isInfixOf` b
          _ -> error "invalid argument to binary 'is in'"

      BinaryNotElement -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (a, SetValue b) -> pure $ BooleanValue $ a `Set.notMember` b
          (a, ListValue b) -> pure $ BooleanValue $ a `notElem` b
          (a, MapValue b) -> pure $ BooleanValue $ a `Map.notMember` b
          (TextValue a, TextValue b) -> pure $ BooleanValue $ not $ a `Text.isInfixOf` b
          _ -> error "invalid argument to binary 'is not in'"

      -- See note [Short-circuiting Operators].
      BinaryAnd -> do
        firstValue <- compiledFirst
        case firstValue of
          BooleanValue False -> pure $ BooleanValue False
          BooleanValue True -> do
            secondValue <- compiledSecond
            case secondValue of
              BooleanValue b -> pure $ BooleanValue b
              _ -> error "invalid argument to binary 'and'"
          _ -> do
            secondValue <- compiledSecond
            case (firstValue, secondValue) of
              (ListValue a, ListValue b)
                -- FIXME: Not sure this notion of list intersection is right.
                -> pure $ ListValue $ a `intersect` b
              (MapValue a, MapValue b)
                -> pure $ MapValue $ a `Map.intersection` b
              (SetValue a, SetValue b)
                -> pure $ SetValue $ a `Set.intersection` b
              _ -> error "invalid argument to binary 'and'"

      -- See note [Short-circuiting Operators].
      BinaryOr -> do
        firstValue <- compiledFirst
        case firstValue of
          BooleanValue True -> pure $ BooleanValue True
          BooleanValue False -> do
            secondValue <- compiledSecond
            case secondValue of
              BooleanValue b -> pure $ BooleanValue b
              _ -> error "invalid argument to binary 'or'"
          _ -> do
            secondValue <- compiledSecond
            case (firstValue, secondValue) of
              -- FIXME: Should this be (<>)?
              (ListValue a, ListValue b) -> pure $ ListValue $ a `union` b
              (MapValue a, MapValue b) -> pure $ MapValue $ Map.unionWith (flip const) a b
              (SetValue a, SetValue b) -> pure $ SetValue $ a `Set.union` b
              _ -> error "invalid argument to binary 'or'"

      BinaryXor -> do
        firstValue <- compiledFirst
        secondValue <- compiledSecond
        case (firstValue, secondValue) of
          (BooleanValue a, BooleanValue b) -> pure $ BooleanValue $ a /= b
          -- FIXME: Not sure this notion of list symmetric difference is right.
          (ListValue a, ListValue b) -> pure $ ListValue
            $ (a <> b) \\ (a `intersect` b)
          (MapValue a, MapValue b) -> pure $ MapValue
            $ Map.unionWith (flip const)
              (a `Map.difference` b)
              (b `Map.difference` a)
          (SetValue a, SetValue b) -> pure $ SetValue
            $ (a `Set.difference` b) `Set.union` (b `Set.difference` a)
          _ -> error "invalid argument to binary 'xor'"

      -- See note [Short-circuiting Operators].
      BinaryImplies -> do
        firstValue <- compiledFirst
        case firstValue of
          BooleanValue False -> pure $ BooleanValue True
          BooleanValue True -> do
            secondValue <- compiledSecond
            case secondValue of
              BooleanValue b -> pure $ BooleanValue b
              _ -> error "invalid argument to binary 'implies'"
          _ -> error "invalid argument to binary 'implies'"

      BinaryAssign -> do
        let readSymbols = contextReadSymbols context
        -- FIXME: We already compiled the first operand, but we ignore that
        -- compilation here and reinterpret it as an lvalue.
        case first of
          IdentifierExpression _ identifier -> do
            symbols <- readSymbols
            case Map.lookup identifier symbols of
              Just cell -> do
                -- FIXME: This copies the current value of the source of the
                -- assignment; should it generate a reactive binding instead? Or
                -- should there be a different assignment operator for that?
                secondValue <- compiledSecond
                set cell $ pure secondValue
              -- TODO: Raise 'Unbound' Hap error.
              Nothing -> error $ concat ["unbound name '", show identifier, "'"]
            pure NullValue
          -- TODO: Raise Hap error.
          _ -> error $ "invalid target of assignment: " ++ show first

  _ -> Left $ "TODO: compile expression: " ++ show expression

-- Note [Short-circuiting Operators]:
--
-- It may not be obvious that it's valid to have short-circuiting logical
-- operators in Hap, but it is correct.
--
-- In the expression 'a or b', 'a' is always evaluated. If 'a' is true, then 'b'
-- is not evaluated, so 'or' doesn't record a dependency on 'b'. However,
-- whether 'b' is true or false doesn't affect the result of the 'or' expression
-- as long as 'a' is true. And if 'a' changes from true to false, then the 'or'
-- expression will be re-evaluated, thereby evaluating 'b' and recording the new
-- dependency. A similar argument can be made for 'a and b' and 'a implies b'
-- when 'a' is initially false.

-- Add an action to be run the first time a condition becomes true, after which
-- it's removed. If true initially, the action is run immediately and the
-- listener is not added. Returns the ID of the listener if one was added; it
-- can be stopped before the action has had a chance to run.
--
-- TODO: Implement this as a function or macro within Hap.
after :: (MonadIO m) => HapT m Value -> HapT m () -> HapT m (Maybe Id)
after condition action = do
  current <- new (Just "'after' condition") condition
  initial <- get current
  if booleanValue initial
    then do
      action
      pure Nothing
    else do
      state <- new (Just "'after' state") $ pure False
      rec
        handler <- whenever condition do
          state' <- get state
          when (not state') do
            action
            stop handler
          set state $ pure True
      pure $ Just handler

-- Add an action to be run whenever a condition becomes true, that is, when it
-- changes from false to true. If true initially, the action is also run as soon
-- as the handler is added.
whenever :: (MonadIO m) => HapT m Value -> HapT m () -> HapT m Id
whenever condition action = do
  current <- new (Just "'whenever' condition") condition
  initial <- get current
  previous <- new (Just "'whenever' previous state") $ pure initial
  when (booleanValue initial) action
  onChange [current] do
    previous' <- get previous
    current' <- get current
    set previous $ pure current'
    when (booleanValue current' && not (booleanValue previous')) action

-- FIXME: Use proper error handling.
booleanValue :: Value -> Bool
booleanValue (BooleanValue True) = True
booleanValue (BooleanValue False) = False
booleanValue _ = error "boolean expected"
