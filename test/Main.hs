{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Monad (void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty)
import Hap.Compiler (compile, newEmptyContext)
import Hap.Language
  ( BinaryOperator(..)
  , Binding(..)
  , Expression(..)
  , Literal(..)
  , Program(..)
  , Signature(..)
  , Statement(..)
  , UnaryOperator(..)
  )
import Hap.ParserWrapper (ParseError, parseProgram)
import Hap.Token (SourceSpan)
import Hap.Runtime (Flag, newEmptyEnv, run)
import Test.HUnit (assertBool, assertFailure)
import Test.Hspec (Spec, describe, hspec, specify)
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
        parseTest "atomic;" \ program -> case program of
          Right (Program [AtomicStatement _ (EmptyStatement _)]) -> True
          _ -> False

      specify "empty (space)" do
        parseTest "atomic ;" \ program -> case program of
          Right (Program [AtomicStatement _ (EmptyStatement _)]) -> True
          _ -> False

    describe "block statement" do

      specify "null" do
        parseTest "{}" \ program -> case program of
          Right (Program [BlockStatement _ []]) -> True
          _ -> False

      specify "singleton empty" do
        parseTest "{;}" \ program -> case program of
          Right (Program [BlockStatement _ [EmptyStatement _]]) -> True
          _ -> False

      specify "singleton empty (space before)" do
        parseTest "{ ;}" \ program -> case program of
          Right (Program [BlockStatement _ [EmptyStatement _]]) -> True
          _ -> False

      specify "singleton empty (space after)" do
        parseTest "{; }" \ program -> case program of
          Right (Program [BlockStatement _ [EmptyStatement _]]) -> True
          _ -> False

      specify "singleton empty (space around)" do
        parseTest "{ ; }" \ program -> case program of
          Right (Program [BlockStatement _ [EmptyStatement _]]) -> True
          _ -> False

      specify "two empties" do
        parseTest "{;;}" \ program -> case program of
          Right (Program
            [ BlockStatement _
              [ EmptyStatement _
              , EmptyStatement _
              ]
            ]) -> True
          _ -> False

      specify "two empties (space around)" do
        parseTest "{ ;; }" \ program -> case program of
          Right (Program
            [ BlockStatement _
              [ EmptyStatement _
              , EmptyStatement _
              ]
            ]) -> True
          _ -> False

      specify "two empties (space around & between)" do
        parseTest "{ ; ; }" \ program -> case program of
          Right (Program
            [ BlockStatement _
              [ EmptyStatement _
              , EmptyStatement _
              ]
            ]) -> True
          _ -> False

      specify "singleton empty, null, empty" do
        parseTest "{;};{}" \ program -> case program of
          Right (Program
            [ BlockStatement _
              [ EmptyStatement _
              ]
            , EmptyStatement _
            , BlockStatement _ []
            ]) -> True
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

      describe "literal expressions" do

        specify "boolean true" do
          parseTest "true;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (BooleanLiteral True))
              ]) -> True
            _ -> False

        specify "boolean false" do
          parseTest "false;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (BooleanLiteral False))
              ]) -> True
            _ -> False

        specify "integral float" do
          parseTest "1.0;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (FloatLiteral 1.0))
              ]) -> True
            _ -> False

        specify "float" do
          parseTest "1.5;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (FloatLiteral 1.5))
              ]) -> True
            _ -> False

        specify "long float" do
          parseTest "10.125;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (FloatLiteral 10.125))
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

        specify "empty string" do
          parseTest "\"\";" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (TextLiteral ""))
              ]) -> True
            _ -> False

        specify "string" do
          parseTest "\"abc123\";" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (TextLiteral "abc123"))
              ]) -> True
            _ -> False

        specify "string escapes" do
          parseTest "\"\\\"\\\\\";" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ (TextLiteral "\"\\"))
              ]) -> True
            _ -> False

        specify "null" do
          parseTest "null;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (LiteralExpression _ NullLiteral)
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

        specify "empty list (trailing comma)" do
          parseTest "[,];" \ program -> case program of
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

        specify "null map/set (trailing comma)" do
          parseTest "({,});" \ program -> case program of
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
          parseTest "({unquoted_key:value});" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _
                  (LiteralExpression _
                    (MapLiteral
                      [ (,)
                        (LiteralExpression _ (TextLiteral "unquoted_key"))
                        (IdentifierExpression _ "value")
                      ])))
              ]) -> True
            _ -> False

        specify "singleton map (space)" do
          parseTest "({ unquoted_key: value });" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _
                  (LiteralExpression _
                    (MapLiteral
                      [ (,)
                        (LiteralExpression _ (TextLiteral "unquoted_key"))
                        (IdentifierExpression _ "value")
                      ])))
              ]) -> True
            _ -> False

        specify "singleton map (quotes)" do
          parseTest "({ \"quoted_key\": value });" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (GroupExpression _
                  (LiteralExpression _
                    (MapLiteral
                      [ (,)
                        (LiteralExpression _ (TextLiteral "quoted_key"))
                        (IdentifierExpression _ "value")
                      ])))
              ]) -> True
            _ -> False

        specify "map" do
          parseTest "({ key1: value1, key2: value2 });"
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
          parseTest "({ \"key1\": value1, key2: value2 });"
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
          parseTest "({ (expr_key): value });"
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
                        (IdentifierExpression _ "expr_key")
                        (IdentifierExpression _ "value")
                      ])))
              ]) -> True
            _ -> False

      describe "expression suffixes" do

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

        specify "each variable" do
          parseTest "each xs;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (UnaryExpression _ UnaryEach
                  (IdentifierExpression _ "xs"))
              ]) -> True
            _ -> False

        specify "every variable" do
          parseTest "every xs;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (UnaryExpression _ UnaryEvery
                  (IdentifierExpression _ "xs"))
              ]) -> True
            _ -> False

        specify "not boolean" do
          parseTest "not true;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (UnaryExpression _ UnaryNot
                  (LiteralExpression _ (BooleanLiteral True)))
              ]) -> True
            _ -> False

        -- Identifier with keyword prefix shouldn't treat keyword as operator.

        specify "name whose first part begins with 'each'" do
          parseTest "eachxs;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "eachxs")
              ]) -> True
            _ -> False

        specify "name whose first part begins with 'every'" do
          parseTest "everyxs;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "everyxs")
              ]) -> True
            _ -> False

        specify "name whose first part begins with 'not'" do
          parseTest "nottrue;" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (IdentifierExpression _ "nottrue")
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

        -- Multi-token operators should not cause ambiguity.

        specify "multi-word operator 'is in'" do
          parseTest "2 is in [1, 2];" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryElement
                  (LiteralExpression _ (IntegerLiteral 2))
                  (LiteralExpression _
                    (ListLiteral
                      [ LiteralExpression _ (IntegerLiteral 1)
                      , LiteralExpression _ (IntegerLiteral 2)
                      ])))
              ]) -> True
            _ -> False

        specify "multi-word operator 'is not in'" do
          parseTest "3 is not in [1, 2];" \ program -> case program of
            Right (Program
              [ ExpressionStatement _
                (BinaryExpression _ BinaryNotElement
                  (LiteralExpression _ (IntegerLiteral 3))
                  (LiteralExpression _
                    (ListLiteral
                      [ LiteralExpression _ (IntegerLiteral 1)
                      , LiteralExpression _ (IntegerLiteral 2)
                      ])))
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

      specify "'if' expressions" do

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

      specify "'let' expressions" do

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

    specify "'after' statement" do

      parseTest "after (player.score = 0)\n\ttrace(\"You lost!\");"
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

    specify "'as long as' statement" do

      parseTest "as long as (player.x < 0) {\n\tout_of_bounds();\n}"
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
                  (IdentifierExpression _ "out_of_bounds")
                  []))
              ])
          ]) -> True
        _ -> False

    specify "'for' statements" do

      parseTest "for each enemy in (enemies) { hurt(enemy); }"
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

      parseTest
        "for all bullet in (bullets) {\n\
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

    specify "'function' statements" do

      parseTest "function test();" \ program -> case program of
        Right (Program
          [ FunctionStatement _ "test" [] Nothing (EmptyStatement _)
          ]) -> True
        _ -> False

      parseTest "function test(x) return x;" \ program -> case program of
        Right (Program
          [ FunctionStatement _ "test"
            [Binding _ "x" Nothing Nothing]
            Nothing
            (ReturnStatement _
              (Just (IdentifierExpression _ "x")))
          ]) -> True
        _ -> False

      parseTest "function test(x) { return x; }" \ program -> case program of
        Right (Program
          [ FunctionStatement _ "test"
            [Binding _ "x" Nothing Nothing]
            Nothing
            (BlockStatement _
              [ (ReturnStatement _
                (Just (IdentifierExpression _ "x")))
              ])
          ]) -> True
        _ -> False

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

    specify "'if' statements" do

      parseTest "if (true) good();" \ program -> case program of
        Right (Program
          [ IfStatement _
            (LiteralExpression _ (BooleanLiteral True))
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "good")
                []))
            Nothing
          ]) -> True
        _ -> False

      parseTest "if (true) good(); else bad();" \ program -> case program of
        Right (Program
          [ IfStatement _
            (LiteralExpression _ (BooleanLiteral True))
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "good")
                []))
            (Just
              (ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "bad")
                  [])))
          ]) -> True
        _ -> False

      parseTest "if (true) { good(); }" \ program -> case program of
        Right (Program
          [ IfStatement _
            (LiteralExpression _ (BooleanLiteral True))
            (BlockStatement _
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "good")
                  [])
              ])
            Nothing
          ]) -> True
        _ -> False

      parseTest "if (true) { good(); } else { bad(); }"
        \ program -> case program of
        Right (Program
          [ IfStatement _
            (LiteralExpression _ (BooleanLiteral True))
            (BlockStatement _
              [ ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "good")
                  [])
              ])
            (Just
              (BlockStatement _
                [ (ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "bad")
                    []))
                ]))
          ]) -> True
        _ -> False

      parseTest "if (true) good(); else if (false) bad();"
        \ program -> case program of
        Right (Program
          [ IfStatement _
            (LiteralExpression _ (BooleanLiteral True))
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "good")
                []))
            (Just
              (IfStatement _
                (LiteralExpression _ (BooleanLiteral False))
                (ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "bad")
                    []))
                Nothing))
          ]) -> True
        _ -> False

      parseTest "if (true) good(); else if (false) bad(); else really_bad();"
        \ program -> case program of
        Right (Program
          [ IfStatement _
            (LiteralExpression _ (BooleanLiteral True))
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "good")
                []))
            (Just
              (IfStatement _
                (LiteralExpression _ (BooleanLiteral False))
                (ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "bad")
                    []))
                (Just
                  (ExpressionStatement _
                    (CallExpression _
                      (IdentifierExpression _ "really_bad")
                      [])))))
          ]) -> True
        _ -> False

      -- See note [Dangling Else].
      parseTest "if (a) if (b) c(); else d();"
        \ program -> case program of
        Right (Program
          [ IfStatement _
            (IdentifierExpression _ "a")
            (IfStatement _
              (IdentifierExpression _ "b")
              (ExpressionStatement _
                (CallExpression _
                  (IdentifierExpression _ "c")
                  []))
              (Just
                (ExpressionStatement _
                  (CallExpression _
                    (IdentifierExpression _ "d")
                    []))))
            Nothing
          ]) -> True
        _ -> False

    specify "'on' statements" do

      parseTest "on set (x) trace(\"x was set\");"
        \ program -> case program of
        Right (Program
          [ OnSetStatement _ (NonEmpty ["x"])
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "trace")
                [LiteralExpression _ (TextLiteral "x was set")]))
          ]) -> True
        _ -> False

      parseTest "on set (x,) trace(\"x was set\");"
        \ program -> case program of
        Right (Program
          [ OnSetStatement _ (NonEmpty ["x"])
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "trace")
                [LiteralExpression _ (TextLiteral "x was set")]))
          ]) -> True
        _ -> False

      parseTest "on set (x, y) trace(\"x or y was set\");"
        \ program -> case program of
        Right (Program
          [ OnSetStatement _ (NonEmpty ["x", "y"])
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "trace")
                [LiteralExpression _ (TextLiteral "x or y was set")]))
          ]) -> True
        _ -> False

      parseTest "on change (x) trace(\"x was changed\");"
        \ program -> case program of
        Right (Program
          [ OnChangeStatement _ (NonEmpty ["x"])
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "trace")
                [LiteralExpression _ (TextLiteral "x was changed")]))
          ]) -> True
        _ -> False

      parseTest "on change (x,) trace(\"x was changed\");"
        \ program -> case program of
        Right (Program
          [ OnChangeStatement _ (NonEmpty ["x"])
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "trace")
                [LiteralExpression _ (TextLiteral "x was changed")]))
          ]) -> True
        _ -> False

      parseTest "on change (x, y) trace(\"x or y was changed\");"
        \ program -> case program of
        Right (Program
          [ OnChangeStatement _ (NonEmpty ["x", "y"])
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "trace")
                [LiteralExpression _ (TextLiteral "x or y was changed")]))
          ]) -> True
        _ -> False

      -- TODO: on add, on remove

    specify "'var' statements" do

      parseTest "var x;" \ program -> case program of
        Right (Program
          [ VarStatement _ (NonEmpty [Binding _ "x" Nothing Nothing])
          ]) -> True
        _ -> False

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

      parseTest "var x, y;" \ program -> case program of
        Right (Program
          [ VarStatement _
            (NonEmpty
              [ Binding _ "x" Nothing Nothing
              , Binding _ "y" Nothing Nothing
              ])
          ]) -> True
        _ -> False

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

    specify "'whenever' statement" do

      parseTest "whenever (score > high_score)\n\tshow_high_score();"
        \ program -> case program of
        Right (Program
          [ WheneverStatement _
            (BinaryExpression _ BinaryGreater
              (IdentifierExpression _ "score")
              (IdentifierExpression _ "high_score"))
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "show_high_score")
                []))
          ]) -> True
        _ -> False

    specify "'while' statement" do

      parseTest "while (true)\n\tplay();"
        \ program -> case program of
        Right (Program
          [ WhileStatement _
            (LiteralExpression _ (BooleanLiteral True))
            (ExpressionStatement _
              (CallExpression _
                (IdentifierExpression _ "play")
                []))
          ]) -> True
        _ -> False

  describe "evaluates" do

    specify "basic output" do

      evalTest "trace (\"hello\");" (== "\"hello\"\n")

      evalTest "trace (2 + 2);" (== "4\n")

    specify "literals" do

      evalTest "trace (true);" (== "true\n")

      evalTest "trace (false);" (== "false\n")

      evalTest "trace (3.14);" (== "3.14\n")

      evalTest "trace (42);" (== "42\n")

      evalTest "trace (\"test\");" (== "\"test\"\n")

      evalTest "trace (null);" (== "null\n")

      evalTest "trace ([1, 2, 3]);" (== "[1, 2, 3]\n")

      evalTest "trace ({});" (== "{  }\n")

      evalTest "trace ({ thing1: \"text\", thing2: 20 });" (== "{ \"thing1\": \"text\", \"thing2\": 20 }\n")

      evalTest "trace ({ \"red\", \"green\", \"blue\" });" (== "{ \"blue\", \"green\", \"red\" }\n")

    specify "'on set' statement" do

      evalTest "\
        \var x = 0;\n\
        \on set (x) output (\"x was set to \", x, \"\\n\");\n\
        \x <- 0;\n\
        \x <- 1;\n\
        \x <- 0;\n\
        \\&"
        (== "\
          \x was set to 0\n\
          \x was set to 1\n\
          \x was set to 0\n\
          \\&")

    specify "'on change' statement" do

      evalTest "\
        \var x = 0;\n\
        \on change (x) output (\"x was changed to \", x, \"\\n\");\n\
        \x <- 0;\n\
        \x <- 1;\n\
        \x <- 0;\n\
        \\&"
        (== "\
          \x was changed to 1\n\
          \x was changed to 0\n\
          \\&")

    specify "'whenever' statement" do

      evalTest "\
       \var x = 1;\n\
       \whenever (x = 2) { output (\"beep\\n\"); }\n\
       \x <- 3;\n\
       \x <- 2;\n\
       \x <- 3;\n\
       \x <- 2;\n\
       \x <- 2;\n\
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
