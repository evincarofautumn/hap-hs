{-# LANGUAGE RecursiveDo #-}

module Hap.Compiler
  ( compile
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Fixed (mod')
import Data.List ((\\), intersect, union)
import Data.Maybe (fromJust)
import Data.Monoid
import Hap.Language
import Hap.Runtime
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

compile :: Program -> Either CompileError (Hap ())
compile (Program statements)
  = fmap sequence_ (traverse compileStatement statements)

type CompileError = String

compileStatement :: Statement -> Either CompileError (Hap ())
compileStatement statement = case statement of
{-
  AtomicStatement !SourcePos !Statement
-}
  AfterStatement _pos condition body -> do
    compiledCondition <- compileExpression condition
    compiledBody <- compileStatement body
    pure $ do
      -- FIXME: Should be an expression to return the ID?
      _id <- after compiledCondition compiledBody
      pure ()
{-
  AsLongAsStatement !SourcePos !Expression !Statement
-}
  BlockStatement _pos statements -> do
    compiledStatements <- traverse compileStatement statements
    pure $ sequence_ compiledStatements
  EmptyStatement _pos -> do
    pure $ pure ()
  ExpressionStatement _pos expression -> do
    compiledExpression <- compileExpression expression
    pure $ do
      _ <- compiledExpression
      pure ()
{-
  ForAllStatement !SourcePos !Identifier !Expression !Statement
  ForEachStatement !SourcePos !Identifier !Expression !Statement
  FunctionStatement !SourcePos !Identifier [(Identifier, Maybe Signature)] Statement
  IfStatement !SourcePos !Expression !Statement !(Maybe Statement)
  LastStatement !SourcePos !(Maybe Identifier)
  NextStatement !SourcePos !(Maybe Identifier)
  OnChangeStatement !SourcePos [Identifier] !Statement
  OnSetStatement !SourcePos [Identifier] !Statement
  -- OnAddStatement !Identifier !Identifier !Statement
  -- OnRemoveStatement !Identifier !Identifier !Statement
  RedoStatement !SourcePos !(Maybe Identifier)
  ReturnStatement !SourcePos !(Maybe Expression)
  VarStatement !SourcePos [(Identifier, Maybe Signature, Maybe Expression)]
  WheneverStatement !SourcePos !Expression !Statement
  WhileStatement !SourcePos !Expression !Statement
-}
  _ -> Left $ "TODO: compile statement: " ++ show statement

compileExpression :: Expression -> Either CompileError (Hap Value)
compileExpression expression = case expression of
  LiteralExpression _pos literal -> case literal of
    BooleanLiteral value -> pure $ pure $ BooleanValue value
    FloatLiteral value -> pure $ pure $ FloatValue value
    IntegerLiteral value -> pure $ pure $ IntegerValue value
    TextLiteral value -> pure $ pure $ TextValue value
    NullLiteral -> pure $ pure NullValue
    ListLiteral elements -> do
      compiledElements <- traverse compileExpression elements
      pure $ do
        elementValues <- sequence compiledElements
        pure $ ListValue elementValues
    MapLiteral pairs -> do
      compiledPairs <- traverse compilePair pairs
      pure $ do
        pairValues <- traverse evalPair compiledPairs
        pure $ MapValue $ Map.fromList pairValues
      where
        compilePair (key, value) = (,)
          <$> compileExpression key
          <*> compileExpression value
        evalPair (key, value) = (,) <$> key <*> value
    SetLiteral elements -> do
      compiledElements <- traverse compileExpression elements
      pure $ do
        elementValues <- sequence compiledElements
        pure $ SetValue $ Set.fromList elementValues
    FunctionLiteral name parameters result body -> do
      compiledBody <- compileStatement body
      pure $ pure $ FunctionValue name parameters result body compiledBody
  IdentifierExpression _pos identifier -> do
    -- TODO: Look up names in scope.
    pure $ pure $ NativeValue $ fromJust $ nativeId identifier
{-
  SubscriptExpression !SourcePos !Expression [Expression]
  DotExpression !SourcePos !Expression !Identifier
-}
  CallExpression _pos function arguments -> do
    compiledFunction <- compileExpression function
    compiledArguments <- traverse compileExpression arguments
    pure $ do
      functionValue <- compiledFunction
      argumentValues <- sequence compiledArguments
      call functionValue argumentValues
    where
      call (NativeValue f) xs = do
        env <- unsafeGetEnv
        liftIO $ nativeFunction f env xs
      call _ _ = error "TODO: call non-native function"
{-
  LetExpression !SourcePos [(Identifier, Maybe Signature, Expression)] !Expression
-}
  GroupExpression _pos body -> do
    compiledBody <- compileExpression body
    pure $ compiledBody
  UnaryExpression _pos operator operand -> do
    compiledOperand <- compileExpression operand
    pure $ do
      operandValue <- compiledOperand
      pure $ case operator of
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
    compiledFirst <- compileExpression first
    compiledSecond <- compileExpression second
    pure $ case operator of

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

      -- set cell
      BinaryAssign -> error "TODO: implement binary '<-'"

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
after :: Hap Value -> Hap () -> Hap (Maybe Id)
after condition action = do
  current <- new condition
  initial <- get current
  if booleanValue initial
    then do
      action
      pure Nothing
    else do
      state <- new $ pure False
      rec
        handler <- whenever condition $ do
          state' <- get state
          when (not state') $ do
            action
            stop handler
          set state $ pure True
      pure $ Just handler

-- Add an action to be run whenever a condition becomes true, that is, when it
-- changes from false to true. If true initially, the action is also run as soon
-- as the handler is added.
whenever :: Hap Value -> Hap () -> Hap Id
whenever condition action = do
  current <- new condition
  initial <- get current
  previous <- new $ pure initial
  when (booleanValue initial) action
  onChange [current] $ do
    previous' <- get previous
    current' <- get current
    set previous $ pure current'
    when (booleanValue current' && not (booleanValue previous')) action

-- FIXME: Use proper error handling.
booleanValue :: Value -> Bool
booleanValue (BooleanValue True) = True
booleanValue (BooleanValue False) = False
booleanValue _ = error "boolean expected"
