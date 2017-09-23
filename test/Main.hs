module Main (main) where

import Test.Hspec (Spec, describe, hspec, specify)
import Test.HUnit (assertFailure)

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()
