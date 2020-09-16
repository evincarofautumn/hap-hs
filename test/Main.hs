{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Monad (join, void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty)
import Hap.Compiler (compile, newEmptyContext)
import Hap.Language
  ( BinaryOperator(..)
  , Binding(..)
  , DecimalFloat(..)
  , DecimalFraction(..)
  , DecimalInteger(..)
  , DecimalIntegerPart(..)
  , Expression(..)
  , Literal(..)
  , Program(..)
  , Signature(..)
  , Splice(..)
  , Statement(..)
  , UnaryOperator(..)
  , decimalIntegerString
  )
import Hap.ParserWrapper (ParseError, parseProgram)
import Hap.Token (DecimalDigit(..), SourceSpan)
import Hap.Runtime (Flag, newEmptyEnv, run)
import Test.HUnit (assertBool, assertFailure)
import Test.Hspec (Spec, describe, hspec, specify)
import Text.Read (readMaybe)
import qualified Data.List.NonEmpty as NonEmpty

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "parses" do

    specify "empty statement" do

      parseTest ";" \ program -> case program of
        Right (Program [EmptyStatement _]) -> True
        _ -> False

    describe "'atomic' statement" do

      specify "empty" do
        parseTest "atomic {}" \ program -> case program of
          Right (Program [AtomicStatement _ (BlockStatement _ [])]) -> True
          _ -> False

    describe "loop control statements" do

      specify "last" do
        parseTest "last;" \ program -> case program of
          Right (Program [LastStatement _ Nothing]) -> True
          _ -> False

      specify "last (space)" do
        parseTest "last ;" \ program -> case program of
          Right (Program [LastStatement _ Nothing]) -> True
          _ -> False

      specify "last label" do
        parseTest "last outer;" \ program -> case program of
          Right (Program [LastStatement _ (Just "outer")]) -> True
          _ -> False

      specify "next" do
        parseTest "next;" \ program -> case program of
          Right (Program [NextStatement _ Nothing]) -> True
          _ -> False

      specify "next (space)" do
        parseTest "next ;" \ program -> case program of
          Right (Program [NextStatement _ Nothing]) -> True
          _ -> False

      specify "next label" do
        parseTest "next outer;" \ program -> case program of
          Right (Program [NextStatement _ (Just "outer")]) -> True
          _ -> False

      specify "redo" do
        parseTest "redo;" \ program -> case program of
          Right (Program [RedoStatement _ Nothing]) -> True
          _ -> False

      specify "redo (space)" do
        parseTest "redo ;" \ program -> case program of
          Right (Program [RedoStatement _ Nothing]) -> True
          _ -> False

      specify "redo label" do
        parseTest "redo outer;" \ program -> case program of
          Right (Program [RedoStatement _ (Just "outer")]) -> True
          _ -> False

    describe "expression statements" do

      describe "identifiers" do

        specify "alpha => name(1)" do
          parseTest "velocity;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "velocity")
              ]) -> True
            _ -> False

        specify "Alpha => name(1)" do
          parseTest "Block;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "Block")
              ]) -> True
            _ -> False

        specify "alnum => name(1)" do
          parseTest "player1;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "player1")
              ]) -> True
            _ -> False

        specify "Alpha Alpha => name(2)" do
          parseTest "Main Window;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "Main Window")
              ]) -> True
            _ -> False

        specify "alpha num primary alpha => name(4)" do
          parseTest "level 1 return door;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "level 1 return door")
              ]) -> True
            _ -> False

        specify "primary alpha => keyword name(1)" do
          parseTest "return x;" \ program -> case program of
            Right (Program
              [ ReturnStatement _
                (Just (IdentifierExpression _ "x"))
              ]) -> True
            _ -> False

        specify "secondary alpha => name(2)" do
          parseTest "add enemies;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "add enemies")
              ]) -> True
            _ -> False

        specify "contextual => name(1)" do
          parseTest "false;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "false")
              ]) -> True
            _ -> False

        specify "contextual alpha => name(2)" do
          parseTest "false block;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "false block")
              ]) -> True
            _ -> False

        specify "alpha numalpha => name(3)" do
          parseTest "wait 10s;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "wait 10 s")
              ]) -> True
            _ -> False

      describe "literal expressions" do

        specify "boolean true" do
          parseTest "true;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "true")
              ]) -> True
            _ -> False

        specify "boolean false" do
          parseTest "false;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "false")
              ]) -> True
            _ -> False

        specify "integral float" do
          parseTest "1.0;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (DecimalFloatLiteral (DecimalFloat
                  (Just (DecimalInteger (NonEmpty
                    [DecimalIntegerPart _ (NonEmpty [DecimalDigit 1])])))
                  (DecimalFraction _ _ (NonEmpty
                    [DecimalIntegerPart _ (NonEmpty [DecimalDigit 0])])))))
              ]) -> True
            _ -> False

        specify "float" do
          parseTest "1.5;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (DecimalFloatLiteral (DecimalFloat
                  (Just (DecimalInteger (NonEmpty
                    [DecimalIntegerPart _ (NonEmpty [DecimalDigit 1])])))
                  (DecimalFraction _ _ (NonEmpty
                    [DecimalIntegerPart _ (NonEmpty [DecimalDigit 5])])))))
              ]) -> True
            _ -> False

        specify "long float" do
          parseTest "10.125;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (DecimalFloatLiteral (DecimalFloat
                  (Just (DecimalInteger (NonEmpty
                    [DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 1
                      , DecimalDigit 0
                      ])])))
                  (DecimalFraction _ _ (NonEmpty
                    [DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 1
                      , DecimalDigit 2
                      , DecimalDigit 5
                      ])])))))
              ]) -> True
            _ -> False

        specify "float with no integer part" do
          parseTest ".5;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (DecimalFloatLiteral (DecimalFloat
                  Nothing
                  (DecimalFraction _ _ (NonEmpty
                    [DecimalIntegerPart _ (NonEmpty [DecimalDigit 5])])))))
              ]) -> True
            _ -> False

        specify "float with multiple integer parts" do
          parseTest "1 234.00;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (DecimalFloatLiteral (DecimalFloat
                  (Just (DecimalInteger (NonEmpty
                    [ DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 1
                      ])
                    , DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 2
                      , DecimalDigit 3
                      , DecimalDigit 4
                      ])
                    ])))
                  (DecimalFraction _ _ (NonEmpty
                    [DecimalIntegerPart _ (NonEmpty
                      [DecimalDigit 0, DecimalDigit 0])])))))
              ]) -> True
            _ -> False

        specify "float with multiple fractional parts" do
          -- NB: 1023/1024
          parseTest "0.999 023 437 500;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (DecimalFloatLiteral (DecimalFloat
                  (Just (DecimalInteger (NonEmpty
                    [DecimalIntegerPart _ (NonEmpty [DecimalDigit 0])])))
                  (DecimalFraction _ _ (NonEmpty
                    [ DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 9
                      , DecimalDigit 9
                      , DecimalDigit 9
                      ])
                    , DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 0
                      , DecimalDigit 2
                      , DecimalDigit 3
                      ])
                    , DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 4
                      , DecimalDigit 3
                      , DecimalDigit 7
                      ])
                    , DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 5
                      , DecimalDigit 0
                      , DecimalDigit 0
                      ])
                    ])))))
              ]) -> True
            _ -> False

        specify "float with multiple integer and fractional parts" do
          parseTest "1 024.062 500;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (DecimalFloatLiteral (DecimalFloat
                  (Just (DecimalInteger (NonEmpty
                    [ DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 1
                      ])
                    , DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 0
                      , DecimalDigit 2
                      , DecimalDigit 4
                      ])
                    ])))
                  (DecimalFraction _ _ (NonEmpty
                    [ DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 0
                      , DecimalDigit 6
                      , DecimalDigit 2
                      ])
                    , DecimalIntegerPart _ (NonEmpty
                      [ DecimalDigit 5
                      , DecimalDigit 0
                      , DecimalDigit 0
                      ])
                    ])))))
              ]) -> True
            _ -> False

        specify "integer" do
          parseTest "1;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (IntegerLiteral 1))
              ]) -> True
            _ -> False

        specify "long integer" do
          parseTest "1234567890;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (IntegerLiteral 1234567890))
              ]) -> True
            _ -> False

        specify "two-part integer" do
          parseTest "1 000;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (DecimalIntegerLiteral
                  (DecimalInteger (NonEmpty
                    [ decimalIntegerPartDigits -> NonEmpty [DecimalDigit 1]
                    , decimalIntegerPartDigits -> NonEmpty
                      [DecimalDigit 0, DecimalDigit 0, DecimalDigit 0]
                    ]))))
              ]) -> True
            _ -> False

        specify "three-part integer" do
          parseTest "1 000 000;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (DecimalIntegerLiteral
                  (DecimalInteger (NonEmpty
                    [ decimalIntegerPartDigits -> NonEmpty [DecimalDigit 1]
                    , decimalIntegerPartDigits -> NonEmpty
                      [DecimalDigit 0, DecimalDigit 0, DecimalDigit 0]
                    , decimalIntegerPartDigits -> NonEmpty
                      [DecimalDigit 0, DecimalDigit 0, DecimalDigit 0]
                    ]))))
              ]) -> True
            _ -> False

        describe "text" do

          specify "empty" do
            parseTest "\"\";" \ program -> case program of
              Right (Program
                [ ExpressionStatement _
                  (LiteralExpression _ (TextLiteral ""))
                ]) -> True
              _ -> False

          specify "basic" do
            parseTest "\"abc123\";" \ program -> case program of
              Right (Program
                [ ExpressionStatement _
                  (LiteralExpression _ (TextLiteral "abc123"))
                ]) -> True
              _ -> False

          specify "trivial splice" do
            parseTest "\"'1'\";" \ program -> case program of
              Right (Program
                [ ExpressionStatement _
                  (SpliceExpression _ (NonEmpty
                    [ TextSplice (_, "")
                    , ExpressionSplice (LiteralExpression _ (IntegerLiteral 1))
                    , TextSplice (_, "")
                    ]))
                ]) -> True
              _ -> False

          specify "splice with prefix and suffix" do
            parseTest "\"before' between 'after\";" \ program -> case program of
              Right (Program
                [ ExpressionStatement _
                  (SpliceExpression _ (NonEmpty
                    [ TextSplice (_, "before")
                    , ExpressionSplice (IdentifierExpression _ "between")
                    , TextSplice (_, "after")
                    ]))
                ]) -> True
              _ -> False

          specify "multiple splices" do
            parseTest "\"<x:'x', y:'y', z:'z'>\";" \ program -> case program of
              Right (Program
                [ ExpressionStatement _
                  (SpliceExpression _ (NonEmpty
                    [ TextSplice (_, "<x:")
                    , ExpressionSplice (IdentifierExpression _ "x")
                    , TextSplice (_, ", y:")
                    , ExpressionSplice (IdentifierExpression _ "y")
                    , TextSplice (_, ", z:")
                    , ExpressionSplice (IdentifierExpression _ "z")
                    , TextSplice (_, ">")
                    ]))
                ]) -> True
              _ -> False

          specify "nested splices" do
            parseTest "\
              \\"before 1 ' \
              \splice 1(\"before 2 ' splice 2 ' after 2\") \
              \' after 1\";\
              \\&" \ program -> case program of
              Right (Program
                [ ExpressionStatement _
                  (SpliceExpression _ (NonEmpty
                    [ TextSplice (_, "before 1 ")
                    , ExpressionSplice (CallExpression _
                      (IdentifierExpression _ "splice 1")
                      [ SpliceExpression _ (NonEmpty
                        [ TextSplice (_, "before 2 ")
                        , ExpressionSplice (IdentifierExpression _ "splice 2")
                        , TextSplice (_, " after 2")
                        ])
                      ])
                    , TextSplice (_, " after 1")
                    ]))
                ]) -> True
              _ -> False

          specify "backslash is not special" do
            parseTest "\"\\\";" \ program -> case program of
              Right (Program
                [ ExpressionStatement _
                  (LiteralExpression _ (TextLiteral "\\"))
                ]) -> True
              _ -> False

        specify "null" do
          parseTest "null;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "null")
              ]) -> True
            _ -> False

        specify "grouped literal" do
          parseTest "(1);" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _
                  (LiteralExpression _ (IntegerLiteral 1)))
              ]) -> True
            _ -> False

        specify "empty list" do
          parseTest "[];" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (ListLiteral []))
              ]) -> True
            _ -> False

        specify "singleton list" do
          parseTest "[1];" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _
                  (ListLiteral
                    [ LiteralExpression _ (IntegerLiteral 1)
                    ]))
              ]) -> True
            _ -> False

        specify "list" do
          parseTest "[1,2,3];" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _
                  (ListLiteral
                    [ LiteralExpression _ (IntegerLiteral 1)
                    , LiteralExpression _ (IntegerLiteral 2)
                    , LiteralExpression _ (IntegerLiteral 3)
                    ]))
              ]) -> True
            _ -> False

        specify "list (trailing comma)" do
          parseTest "[1,2,3,];" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _
                  (ListLiteral
                    [ LiteralExpression _ (IntegerLiteral 1)
                    , LiteralExpression _ (IntegerLiteral 2)
                    , LiteralExpression _ (IntegerLiteral 3)
                    ]))
              ]) -> True
            _ -> False

        specify "nested list" do
          parseTest
            "[\n\
            \  [1, 0],\n\
            \  [0, 1],\n\
            \];\n\
            \\&"
            \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _
                  (ListLiteral
                    [ LiteralExpression _
                      (ListLiteral
                        [ LiteralExpression _ (IntegerLiteral 1)
                        , LiteralExpression _ (IntegerLiteral 0)
                        ])
                    , LiteralExpression _
                      (ListLiteral
                        [ LiteralExpression _ (IntegerLiteral 0)
                        , LiteralExpression _ (IntegerLiteral 1)
                        ])
                    ]))
              ]) -> True
            _ -> False

          -- Parentheses are required to differentiate map and set literals from
          -- block statements.

        specify "null map/set" do
          parseTest "({});" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _ (LiteralExpression _ (SetLiteral [])))
              ]) -> True
            _ -> False

        specify "singleton set" do
          parseTest "({1});" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _
                  (LiteralExpression _
                    (SetLiteral
                      [ LiteralExpression _ (IntegerLiteral 1)
                      ])))
              ]) -> True
            _ -> False

        specify "singleton set (trailing comma)" do
          parseTest "({1,});" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _
                  (LiteralExpression _
                    (SetLiteral
                      [ LiteralExpression _ (IntegerLiteral 1)
                      ])))
              ]) -> True
            _ -> False

        specify "singleton map" do
          parseTest "({ unquoted key => value });" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _
                  (LiteralExpression _
                    (MapLiteral
                      [ (,)
                        (LiteralExpression _ (TextLiteral "unquoted key"))
                        (IdentifierExpression _ "value")
                      ])))
              ]) -> True
            _ -> False

        specify "singleton map (quotes)" do
          parseTest "({ \"quoted key\" => value });" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _
                  (LiteralExpression _
                    (MapLiteral
                      [ (,)
                        (LiteralExpression _ (TextLiteral "quoted key"))
                        (IdentifierExpression _ "value")
                      ])))
              ]) -> True
            _ -> False

        specify "map" do
          parseTest "({ key1 => value1, key2 => value2 });"
            \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _
                  (LiteralExpression _
                    (MapLiteral
                      [ (,)
                        (LiteralExpression _ (TextLiteral "key1"))
                        (IdentifierExpression _ "value1")
                      , (,)
                        (LiteralExpression _ (TextLiteral "key2"))
                        (IdentifierExpression _ "value2")
                      ])))
              ]) -> True
            _ -> False

        specify "map (quotes)" do
          parseTest "({ \"key1\" => value1, key2 => value2 });"
            \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _
                  (LiteralExpression _
                    (MapLiteral
                      [ (,)
                        (LiteralExpression _ (TextLiteral "key1"))
                        (IdentifierExpression _ "value1")
                      , (,)
                        (LiteralExpression _ (TextLiteral "key2"))
                        (IdentifierExpression _ "value2")
                      ])))
              ]) -> True
            _ -> False

        specify "map (expression)" do
          parseTest "({ (expr key) => value });"
            \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _
                  (LiteralExpression _
                    (MapLiteral
                      [ (,)
                        -- Note that this is not wrapped in a group expression,
                        -- because bare keys are parsed as text literals, not
                        -- identifiers.
                        (IdentifierExpression _ "expr key")
                        (IdentifierExpression _ "value")
                      ])))
              ]) -> True
            _ -> False

      describe "expression suffixes" do

        specify "call with no arguments" do
          parseTest "f();" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "f")
                  [])
              ]) -> True
            _ -> False

        specify "call with one argument" do
          parseTest "f(x);" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "f")
                  [IdentifierExpression _ "x"])
              ]) -> True
            _ -> False

        specify "call with one argument and trailing comma" do
          parseTest "f(x,);" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "f")
                  [IdentifierExpression _ "x"])
              ]) -> True
            _ -> False

        specify "call with two arguments" do
          parseTest "f(x, y);" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "f")
                  [IdentifierExpression _ "x", IdentifierExpression _ "y"])
              ]) -> True
            _ -> False

        specify "call with two arguments and trailing comma" do
          parseTest "f(x, y,);" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "f")
                  [IdentifierExpression _ "x", IdentifierExpression _ "y"])
              ]) -> True
            _ -> False

        specify "subscript" do
          parseTest "[1, 2][0];" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (SubscriptExpression _
                  (LiteralExpression _
                    (ListLiteral
                      [ LiteralExpression _ (IntegerLiteral 1)
                      , LiteralExpression _ (IntegerLiteral 2)
                      ]))
                  [LiteralExpression _ (IntegerLiteral 0)])
              ]) -> True
            _ -> False

        specify "multidimensional subscript" do
          parseTest "[1, 2][0, 1];" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (SubscriptExpression _
                  (LiteralExpression _
                    (ListLiteral
                      [ LiteralExpression _ (IntegerLiteral 1)
                      , LiteralExpression _ (IntegerLiteral 2)
                      ]))
                  [ LiteralExpression _ (IntegerLiteral 0)
                  , LiteralExpression _ (IntegerLiteral 1)
                  ])
              ]) -> True
            _ -> False

        specify "multiple subscripts" do
          parseTest "[[1, 2]][0][1];" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (SubscriptExpression _
                  (SubscriptExpression _
                    (LiteralExpression _
                      (ListLiteral
                        [ (LiteralExpression _
                          (ListLiteral
                            [ LiteralExpression _ (IntegerLiteral 1)
                            , LiteralExpression _ (IntegerLiteral 2)
                            ]))
                        ]))
                    [LiteralExpression _ (IntegerLiteral 0)])
                  [LiteralExpression _ (IntegerLiteral 1)])
              ]) -> True
            _ -> False

        specify "dot" do
          parseTest "foo.bar;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (DotExpression _
                  (IdentifierExpression _ "foo")
                  "bar")
              ]) -> True
            _ -> False

        specify "dot call (0 arguments)" do
          parseTest "foo.bar();" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (CallExpression _
                  (DotExpression _
                    (IdentifierExpression _ "foo")
                    "bar")
                  [])
              ]) -> True
            _ -> False

        specify "dot call (1 argument)" do
          parseTest "foo.bar(baz);" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (CallExpression _
                  (DotExpression _
                    (IdentifierExpression _ "foo")
                    "bar")
                  [ IdentifierExpression _ "baz"
                  ])
              ]) -> True
            _ -> False

        specify "dot call (multiple arguments)" do
          parseTest "foo.bar(baz, quux);" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (CallExpression _
                  (DotExpression _
                    (IdentifierExpression _ "foo")
                    "bar")
                  [ IdentifierExpression _ "baz"
                  , IdentifierExpression _ "quux"
                  ])
              ]) -> True
            _ -> False

        specify "call after subscript" do
          parseTest "foo.bar[0](\"baz\");" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (CallExpression _
                  (SubscriptExpression _
                    (DotExpression _
                      (IdentifierExpression _ "foo")
                      "bar")
                    [ LiteralExpression _ (IntegerLiteral 0)
                    ])
                  [LiteralExpression _ (TextLiteral "baz")])
              ]) -> True
            _ -> False

      describe "operators" do

        specify "negate integer" do
          parseTest "-1;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (UnaryExpression _ UnaryMinus
                  (LiteralExpression _ (IntegerLiteral 1)))
              ]) -> True
            _ -> False

        specify "identity integer" do
          parseTest "+1;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (UnaryExpression _ UnaryPlus
                  (LiteralExpression _ (IntegerLiteral 1)))
              ]) -> True
            _ -> False

        specify "negate variable" do
          parseTest "-x;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (UnaryExpression _ UnaryMinus
                  (IdentifierExpression _ "x"))
              ]) -> True
            _ -> False

        specify "not boolean" do
          parseTest "~true;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (UnaryExpression _ UnaryNot
                  (IdentifierExpression _ "true"))
              ]) -> True
            _ -> False

        specify "addition" do
          parseTest "2 + 3;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryAdd
                  (LiteralExpression _ (IntegerLiteral 2))
                  (LiteralExpression _ (IntegerLiteral 3)))
              ]) -> True
            _ -> False

        specify "addition (no space)" do
          parseTest "2+3;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryAdd
                  (LiteralExpression _ (IntegerLiteral 2))
                  (LiteralExpression _ (IntegerLiteral 3)))
              ]) -> True
            _ -> False

        -- Associativity should be correct for all operators.

        specify "addition associativity" do
          parseTest "2 + 3 + 5;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryAdd
                  (BinaryExpression _ BinaryAdd
                    (LiteralExpression _ (IntegerLiteral 2))
                    (LiteralExpression _ (IntegerLiteral 3)))
                  (LiteralExpression _ (IntegerLiteral 5)))
              ]) -> True
            _ -> False

        specify "multiplication associativity" do
          parseTest "2 * 3 * 5;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryMultiply
                  (BinaryExpression _ BinaryMultiply
                    (LiteralExpression _ (IntegerLiteral 2))
                    (LiteralExpression _ (IntegerLiteral 3)))
                  (LiteralExpression _ (IntegerLiteral 5)))
              ]) -> True
            _ -> False

        -- Relative operator precedence should be preserved and overridden with
        -- parentheses, which also introduce group expressions.

        specify "precedence multiplication > addition" do
          parseTest "2 * 3 + 5;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryAdd
                  (BinaryExpression _ BinaryMultiply
                    (LiteralExpression _ (IntegerLiteral 2))
                    (LiteralExpression _ (IntegerLiteral 3)))
                  (LiteralExpression _ (IntegerLiteral 5)))
              ]) -> True
            _ -> False

        specify "precedence redundant grouping" do
          parseTest "(2 * 3) + 5;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryAdd
                  (GroupExpression _
                    (BinaryExpression _ BinaryMultiply
                      (LiteralExpression _ (IntegerLiteral 2))
                      (LiteralExpression _ (IntegerLiteral 3))))
                  (LiteralExpression _ (IntegerLiteral 5)))
              ]) -> True
            _ -> False

        specify "precedence required grouping" do
          parseTest "2 * (3 + 5);" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryMultiply
                  (LiteralExpression _ (IntegerLiteral 2))
                  (GroupExpression _
                    (BinaryExpression _ BinaryAdd
                      (LiteralExpression _ (IntegerLiteral 3))
                      (LiteralExpression _ (IntegerLiteral 5)))))
              ]) -> True
            _ -> False

        specify "modulus and multiplication left-associative" do
          parseTest "x mod 2 * 3;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryMultiply
                  (BinaryExpression _ BinaryModulus
                    (IdentifierExpression _ "x")
                    (LiteralExpression _ (IntegerLiteral 2)))
                  (LiteralExpression _ (IntegerLiteral 3)))
              ]) -> True
            _ -> False

        specify "relational lower precedence than arithmetic" do
          parseTest "x + y < z * w;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryLess
                  (BinaryExpression _ BinaryAdd
                    (IdentifierExpression _ "x")
                    (IdentifierExpression _ "y"))
                  (BinaryExpression _ BinaryMultiply
                    (IdentifierExpression _ "z")
                    (IdentifierExpression _ "w")))
              ]) -> True
            _ -> False

        -- See note [Compound Comparisons].
        specify "chained relations" do
          parseTest "x < y < z;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryLess
                  (BinaryExpression _ BinaryLess
                    (IdentifierExpression _ "x")
                    (IdentifierExpression _ "y"))
                  (IdentifierExpression _ "z"))
              ]) -> True
            _ -> False

        specify "logical operators" do
          parseTest "a and not b implies not c or d;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryImplies
                  (BinaryExpression _ BinaryAnd
                    (IdentifierExpression _ "a")
                    (UnaryExpression _ UnaryNot
                      (IdentifierExpression _ "b")))
                  (BinaryExpression _ BinaryOr
                    (UnaryExpression _ UnaryNot
                      (IdentifierExpression _ "c"))
                    (IdentifierExpression _ "d")))
              ]) -> True
            _ -> False

      describe "'if' expressions" do

        specify "basic" do
          parseTest "(if (x) a else b)();" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (CallExpression _
                  (GroupExpression _
                    (IfExpression _
                      (IdentifierExpression _ "x")
                      (IdentifierExpression _ "a")
                      (IdentifierExpression _ "b")))
                  [])
              ]) -> True
            _ -> False

      describe "'let' expressions" do

        specify "single" do
          parseTest "let x = 1 in trace(x);" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LetExpression _
                  [("x", Nothing, LiteralExpression _ (IntegerLiteral 1))]
                  (CallExpression _
                    (IdentifierExpression _ "trace")
                    [IdentifierExpression _ "x"]))
              ]) -> True
            _ -> False

        specify "multiple" do
          parseTest "let x = 1, y = 2 in trace(x + y);"
            \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LetExpression _
                  [ ("x", Nothing, LiteralExpression _ (IntegerLiteral 1))
                  , ("y", Nothing, LiteralExpression _ (IntegerLiteral 2))
                  ]
                  (CallExpression _
                    (IdentifierExpression _ "trace")
                    [BinaryExpression _ BinaryAdd
                      (IdentifierExpression _ "x")
                      (IdentifierExpression _ "y")]))
              ]) -> True
            _ -> False

        -- The 'in' keyword shouldn't cause ambiguity with the 'is in' operator.
        specify "with 'is in'" do
          parseTest "let a = b is in c in a is in d;"
            \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LetExpression _
                  [ (,,)
                    "a"
                    Nothing
                    (BinaryExpression _ BinaryElement
                      (IdentifierExpression _ "b")
                      (IdentifierExpression _ "c"))
                  ]
                  (BinaryExpression _ BinaryElement
                    (IdentifierExpression _ "a")
                    (IdentifierExpression _ "d")))
              ]) -> True
            _ -> False

        specify "type annotation" do
          parseTest "let x:int = 1 in x;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LetExpression _
                  [ (,,)
                    "x"
                    (Just (ConstructorSignature _ "int"))
                    (LiteralExpression _ (IntegerLiteral 1))
                  ]
                  (IdentifierExpression _ "x"))
              ]) -> True
            _ -> False

    describe "'after' statement" do

      specify "basic" do
        parseTest "after (player.score = 0) {\n\ttrace(\"You lost!\");\n}"
          \ program -> case program of
          Right (Program
            [ AfterStatement _
              (BinaryExpression _ BinaryEqual
                (DotExpression _
                  (IdentifierExpression _ "player")
                  "score")
                (LiteralExpression _ (IntegerLiteral 0)))
              (ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "trace")
                  [LiteralExpression _ (TextLiteral "You lost!")]))
            ]) -> True
          _ -> False

    describe "'as long as' statement" do

      specify "basic" do
        parseTest "as long as (player.x < 0) {\n\tout of bounds();\n}"
          \ program -> case program of
          Right (Program
            [ AsLongAsStatement _
              (BinaryExpression _ BinaryLess
                (DotExpression _
                  (IdentifierExpression _ "player")
                  "x")
                (LiteralExpression _ (IntegerLiteral 0)))
              (BlockStatement _
                [ (ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "out of bounds")
                    []))
                ])
            ]) -> True
          _ -> False

    describe "'for' statements" do

      specify "for each" do
        parseTest "for each (enemy : enemies) { hurt(enemy); }"
          \ program -> case program of
          Right (Program
            [ ForEachStatement _ "enemy" (IdentifierExpression _ "enemies")
              (BlockStatement _
                [ ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "hurt")
                    [IdentifierExpression _ "enemy"])
                ])
            ]) -> True
          _ -> False

      specify "for all" do
        parseTest
          "for all (bullet : bullets) {\n\
          \\twhenever (collides(bullet, player)) {\n\
          \\t\tdestroy (bullet);\n\
          \\t\thurt (player);\n\
          \\t}\n\
          \}\n\
          \\&"
          \ program -> case program of
          Right (Program
            [ ForAllStatement _ "bullet" (IdentifierExpression _ "bullets")
              (BlockStatement _
                [ WheneverStatement _
                  (CallExpression _
                    (IdentifierExpression _ "collides")
                    [ IdentifierExpression _ "bullet"
                    , IdentifierExpression _ "player"
                    ])
                  (BlockStatement _
                    [ ExpressionStatement _
                      (CallExpression _
                        (IdentifierExpression _ "destroy")
                        [IdentifierExpression _ "bullet"])
                    , ExpressionStatement _
                      (CallExpression _
                        (IdentifierExpression _ "hurt")
                        [IdentifierExpression _ "player"])
                    ])
                ])
            ]) -> True
          _ -> False

    describe "'function' statements" do

      specify "empty" do
        parseTest "function test() {}" \ program -> case program of
          Right (Program
            [ FunctionStatement _ "test" [] Nothing
              (BlockStatement _ [])
            ]) -> True
          _ -> False

      specify "return statement" do
        parseTest "function test(x) { return x; }" \ program -> case program of
          Right (Program
            [ FunctionStatement _ "test"
              [Binding _ "x" Nothing Nothing]
              Nothing
              (BlockStatement _
                [ ReturnStatement _
                  (Just (IdentifierExpression _ "x"))
                ])
            ]) -> True
          _ -> False

      specify "parameter with type annotation" do
        parseTest "function test(x: int) { return x; }" \ program -> case program of
          Right (Program
            [ FunctionStatement _ "test"
              [Binding _ "x" (Just (ConstructorSignature _ "int")) Nothing]
              Nothing
              (BlockStatement _
                [ (ReturnStatement _
                  (Just (IdentifierExpression _ "x")))
                ])
            ]) -> True
          _ -> False

      specify "parameter and return type" do
        parseTest "function test(x: int): int { return x; }"
          \ program -> case program of
          Right (Program
            [ FunctionStatement _ "test"
              [Binding _ "x" (Just (ConstructorSignature _ "int")) Nothing]
              (Just (ConstructorSignature _ "int"))
              (BlockStatement _
                [ (ReturnStatement _
                  (Just (IdentifierExpression _ "x")))
                ])
            ]) -> True
          _ -> False

      specify "parameter with default value" do
        parseTest "function test(x: int = 0): int { return x; }"
          \ program -> case program of
          Right (Program
            [ FunctionStatement _ "test"
              [ Binding _
                "x"
                (Just (ConstructorSignature _ "int"))
                (Just (LiteralExpression _ (IntegerLiteral 0)))
              ]
              (Just (ConstructorSignature _ "int"))
              (BlockStatement _
                [ (ReturnStatement _
                  (Just (IdentifierExpression _ "x")))
                ])
            ]) -> True
          _ -> False

      specify "return function" do
        parseTest
          "function add(x: int): function(int): int {\n\
          \\treturn function(y: int): (int) {\n\
          \\t\treturn x + y;\n\
          \\t};\n\
          \}\n\
          \\&"
          \ program -> case program of
          Right (Program
            [ FunctionStatement _ "add"
              [Binding _ "x" (Just (ConstructorSignature _ "int")) Nothing]
              (Just
                (FunctionSignature _
                  [ConstructorSignature _ "int"]
                  (ConstructorSignature _ "int")))
              (BlockStatement _
                [ (ReturnStatement _
                  (Just
                    (LiteralExpression _
                      (FunctionLiteral
                        Nothing
                        [ (,,)
                          "y"
                          (Just (ConstructorSignature _ "int"))
                          Nothing
                        ]
                        (Just (ConstructorSignature _ "int"))
                        (BlockStatement _
                          [ ReturnStatement _
                            (Just
                              (BinaryExpression _ BinaryAdd
                                (IdentifierExpression _ "x")
                                (IdentifierExpression _ "y")))
                          ])))))
                ])
            ]) -> True
          _ -> False

    describe "'if' statements" do

      specify "one-sided" do
        parseTest "\
          \if (true) {\n\
          \\tgood();\n\
          \}\n\
          \\&" \ program -> case program of
          Right (Program
            [ IfStatement _
              (IdentifierExpression _ "true")
              (BlockStatement _
                [ ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "good")
                    [])
                ])
              Nothing
            ]) -> True
          _ -> False

      specify "two-sided" do
        parseTest "\
          \if (true) {\n\
          \\tgood();\n\
          \} else {\n\
          \\tbad();\n\
          \}\n\
          \\&" \ program -> case program of
          Right (Program
            [ IfStatement _
              (IdentifierExpression _ "true")
              (BlockStatement _
                [ ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "good")
                    [])
                ])
              (Just (BlockStatement _
                [ ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "bad")
                    [])
                ]))
            ]) -> True
          _ -> False

      specify "one-sided 'else if'" do
        parseTest "if (true) { good(); } else if (false) { bad(); }"
          \ program -> case program of
          Right (Program
            [ IfStatement _
              (IdentifierExpression _ "true")
              (BlockStatement _
                [ ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "good")
                    [])
                ])
              (Just
                (IfStatement _
                  (IdentifierExpression _ "false")
                  (BlockStatement _
                    [ ExpressionStatement _
                      (CallExpression _
                        (IdentifierExpression _ "bad")
                        [])
                    ])
                  Nothing))
            ]) -> True
          _ -> False

      specify "if-else if-else" do
        parseTest "\
          \if (true) {\n\
          \\tgood();\n\
          \} else if (false) {\n\
          \\tbad();\n\
          \} else {\n\
          \\treally bad();\n\
          \}\n\
          \\&"
          \ program -> case program of
          Right (Program
            [ IfStatement _
              (IdentifierExpression _ "true")
              (BlockStatement _
                [ ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "good")
                    [])
                ])
              (Just
                (IfStatement _
                  (IdentifierExpression _ "false")
                  (BlockStatement _
                    [ ExpressionStatement _
                      (CallExpression _
                        (IdentifierExpression _ "bad")
                        [])
                    ])
                  (Just
                    (BlockStatement _
                      [ (ExpressionStatement _
                        (CallExpression _
                          (IdentifierExpression _ "really bad")
                          []))
                      ]))))
            ]) -> True
          _ -> False

    specify "'on' statements" do

      parseTest "on set (x) { trace(\"x was set\"); }"
        \ program -> case program of
        Right (Program
          [ OnSetStatement _ (NonEmpty ["x"])
            (BlockStatement _
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "trace")
                  [LiteralExpression _ (TextLiteral "x was set")])
              ])
          ]) -> True
        _ -> False

      parseTest "on set (x,) { trace(\"x was set\"); }"
        \ program -> case program of
        Right (Program
          [ OnSetStatement _ (NonEmpty ["x"])
            (BlockStatement _
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "trace")
                  [LiteralExpression _ (TextLiteral "x was set")])
              ])
          ]) -> True
        _ -> False

      parseTest "on set (x, y) { trace(\"x or y was set\"); }"
        \ program -> case program of
        Right (Program
          [ OnSetStatement _ (NonEmpty ["x", "y"])
            (BlockStatement _
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "trace")
                  [LiteralExpression _ (TextLiteral "x or y was set")])
              ])
          ]) -> True
        _ -> False

      parseTest "on change (x) { trace(\"x was changed\"); }"
        \ program -> case program of
        Right (Program
          [ OnChangeStatement _ (NonEmpty ["x"])
            (BlockStatement _
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "trace")
                  [LiteralExpression _ (TextLiteral "x was changed")])
              ])
          ]) -> True
        _ -> False

      parseTest "on change (x,) { trace(\"x was changed\"); }"
        \ program -> case program of
        Right (Program
          [ OnChangeStatement _ (NonEmpty ["x"])
            (BlockStatement _
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "trace")
                  [LiteralExpression _ (TextLiteral "x was changed")])
              ])
          ]) -> True
        _ -> False

      parseTest "on change (x, y) { trace(\"x or y was changed\"); }"
        \ program -> case program of
        Right (Program
          [ OnChangeStatement _ (NonEmpty ["x", "y"])
            (BlockStatement _
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "trace")
                  [LiteralExpression _ (TextLiteral "x or y was changed")])
              ])
          ]) -> True
        _ -> False

      -- TODO: on add, on remove

    describe "'var' statements" do

      specify "single" do
        parseTest "var x;" \ program -> case program of
          Right (Program
            [ VarStatement _ (NonEmpty [Binding _ "x" Nothing Nothing])
            ]) -> True
          _ -> False

      specify "single, initializer" do
        parseTest "var x = 1;" \ program -> case program of
          Right (Program
            [ VarStatement _
              (NonEmpty
                [ Binding _
                  "x"
                  Nothing
                  (Just (LiteralExpression _ (IntegerLiteral 1)))
                ])
            ]) -> True
          _ -> False

      specify "single, annotation" do
        parseTest "var x: int;" \ program -> case program of
          Right (Program
            [ VarStatement _
              (NonEmpty
                [ Binding _
                  "x"
                  (Just (ConstructorSignature _ "int"))
                  Nothing
                ])
            ]) -> True
          _ -> False

      specify "single, annotation, initializer" do
        parseTest "var x: int = 1;" \ program -> case program of
          Right (Program
            [ VarStatement _
              (NonEmpty
                [ Binding _
                  "x"
                  (Just (ConstructorSignature _ "int"))
                  (Just (LiteralExpression _ (IntegerLiteral 1)))
                ])
            ]) -> True
          _ -> False

      specify "plural" do
        parseTest "var x, y;" \ program -> case program of
          Right (Program
            [ VarStatement _
              (NonEmpty
                [ Binding _ "x" Nothing Nothing
                , Binding _ "y" Nothing Nothing
                ])
            ]) -> True
          _ -> False

      specify "plural, initializers" do
        parseTest "var x = 1, y = 2;" \ program -> case program of
          Right (Program
            [ VarStatement _
              (NonEmpty
                [ Binding _
                  "x"
                  Nothing
                  (Just (LiteralExpression _ (IntegerLiteral 1)))
                , Binding _
                  "y"
                  Nothing
                  (Just (LiteralExpression _ (IntegerLiteral 2)))
                ])
            ]) -> True
          _ -> False

      specify "single, function type" do
        parseTest
          "var f: function(int): int =\n\
          \\tfunction identity(x) {\n\
          \\t\treturn x;\n\
          \\t};\n\
          \\&"
          \ program -> case program of
          Right (Program
            [ VarStatement _
              (NonEmpty
                [ Binding _
                  "f"
                  (Just
                    (FunctionSignature _
                      [ConstructorSignature _ "int"]
                      (ConstructorSignature _ "int")))
                  (Just
                    (LiteralExpression _
                      (FunctionLiteral
                        (Just "identity")
                        [("x", Nothing, Nothing)]
                        Nothing
                        (BlockStatement _
                          [ ReturnStatement _ (Just (IdentifierExpression _ "x"))
                          ]))))
                ])
            ]) -> True
          _ -> False

    describe "'whenever' statement" do

      specify "basic" do
        parseTest "whenever (score > high score) {\n\tshow high score();\n}"
          \ program -> case program of
          Right (Program
            [ WheneverStatement _
              (BinaryExpression _ BinaryGreater
                (IdentifierExpression _ "score")
                (IdentifierExpression _ "high score"))
              (BlockStatement _
                [(ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "show high score")
                    []))
                ])
            ]) -> True
          _ -> False

    describe "'while' statement" do

      specify "basic" do
        parseTest "while (true) {\n\tplay();\n}"
          \ program -> case program of
          Right (Program
            [ WhileStatement _
              (IdentifierExpression _ "true")
              (BlockStatement _
                [ ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "play")
                    [])
                ])
            ]) -> True
          _ -> False

  describe "evaluates" do

    describe "basic output" do

      specify "text" do
        evalTest "trace (\"hello\");" (== "\"hello\"\n")

      specify "integer" do
        evalTest "trace (2 + 2);" (== "4\n")

    describe "literals" do

      specify "boolean true" do
        evalTest "trace (true);" (== "true\n")

      specify "boolean false" do
        evalTest "trace (false);" (== "false\n")

      specify "float" do
        evalTest "trace (3.14);" (== "3.14\n")

      specify "integer" do
        evalTest "trace (42);" (== "42\n")

      specify "text" do
        evalTest "trace (\"test\");" (== "\"test\"\n")

      specify "null" do
        evalTest "trace (null);" (== "null\n")

      specify "list" do
        evalTest "trace ([1, 2, 3]);" (== "[1, 2, 3]\n")

      specify "empty" do
        evalTest "trace ({});" (== "{}\n")

      specify "map" do
        evalTest "trace ({ thing1 => \"text\", thing2 => 20 });"
          (== "{ \"thing1\" => \"text\", \"thing2\" => 20 }\n")

      specify "set" do
        evalTest "trace ({ \"red\", \"green\", \"blue\" });"
          (== "{ \"blue\", \"green\", \"red\" }\n")

    describe "'on set' statement" do

      specify "single" do
        evalTest "\
          \var x = 0;\n\
          \on set (x) { output line (\"x was set to \", x); }\n\
          \x := 0;\n\
          \x := 1;\n\
          \x := 0;\n\
          \\&"
          (== "\
            \x was set to 0\n\
            \x was set to 1\n\
            \x was set to 0\n\
            \\&")

    describe "'on change' statement" do

      specify "single" do
        evalTest "\
          \var x = 0;\n\
          \on change (x) { output line (\"x was changed to \", x); }\n\
          \x := 0;\n\
          \x := 1;\n\
          \x := 0;\n\
          \\&"
          (== "\
            \x was changed to 1\n\
            \x was changed to 0\n\
            \\&")

    describe "'whenever' statement" do

      specify "basic" do
        evalTest "\
         \var x = 1;\n\
         \whenever (x = 2) { output line (\"beep\"); }\n\
         \x := 3;\n\
         \x := 2;\n\
         \x := 3;\n\
         \x := 2;\n\
         \x := 2;\n\
         \\&"
         (== "\
           \beep\n\
           \beep\n\
           \\&")

parseTest :: String -> (Either ParseError (Program SourceSpan) -> Bool) -> IO ()
parseTest source successful = do
  let result = parseProgram "test" source
  assertBool (concat [show source, " => ", show result]) $ successful result

evalTest :: String -> (String -> Bool) -> IO ()
evalTest = evalTestFlags []

evalTestFlags :: [Flag] -> String -> (String -> Bool) -> IO ()
evalTestFlags flags source successful = do
  context <- newEmptyContext
  output <- newIORef []
  let logOutput = modifyIORef' output . (:)
  env <- newEmptyEnv logOutput flags
  case parseProgram "test" source of
    Left parseError -> assertFailure (show parseError)
    Right program -> case compile context program of
      Left compileError -> assertFailure compileError
      Right compiled -> do
        void $ run env compiled
        result <- concat . reverse <$> readIORef output
        assertBool (concat [show source, " => ", show result]) $ successful result

pattern NonEmpty :: [a] -> NonEmpty a
pattern NonEmpty xs <- (NonEmpty.toList -> xs)

pattern IntegerLiteral :: Int -> Literal anno
pattern IntegerLiteral n
  <- (\ case {
      DecimalIntegerLiteral digits
        -> Just $ readMaybe $ decimalIntegerString digits;
      _ -> Nothing;
    } -> Just (Just n))
