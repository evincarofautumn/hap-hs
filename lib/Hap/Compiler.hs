{-# LANGUAGE RecursiveDo #-}

module Hap.Compiler
  ( compile
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Hap.Language
import Hap.Runtime
import qualified Data.Map as Map
import qualified Data.Set as Set

compile :: Program -> Either CompileError (Hap ())
compile (Program statements)
  = fmap sequence_ (traverse compileStatement statements)

type CompileError = String

compileStatement :: Statement -> Either CompileError (Hap ())
compileStatement statement = case statement of
  AtomicStatement _pos body -> Left "TODO: compile AtomicStatement"
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
  _ -> Left "TODO: compile statement"

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
  GroupExpression !SourcePos !Expression
  UnaryExpression !SourcePos !UnaryOperator !Expression
  BinaryExpression !SourcePos !BinaryOperator !Expression !Expression
-}
  _ -> Left $ "TODO: compile expression: " ++ show expression

-- Add an action to be run the first time a condition becomes true, after which
-- it's removed. If true initially, the action is run immediately and the
-- listener is not added. Returns the ID of the listener if one was added; it
-- can be stopped before the action has had a chance to run.
--
-- TODO: Implement this as a function or macro within Hap.
after :: Hap Value -> Hap () -> Hap (Maybe Id)
after condition action = do
  current <- cell condition
  initial <- get current
  if booleanValue initial
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

-- Add an action to be run whenever a condition becomes true, that is, when it
-- changes from false to true. If true initially, the action is also run as soon
-- as the handler is added.
whenever :: Hap Value -> Hap () -> Hap Id
whenever condition action = do
  current <- cell condition
  initial <- get current
  previous <- cell $ pure initial
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
