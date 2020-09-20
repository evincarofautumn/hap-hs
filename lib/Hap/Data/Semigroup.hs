module Hap.Data.Semigroup
  ( foldMap1
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)

foldMap1 :: (Semigroup s) => (a -> s) -> NonEmpty a -> s
foldMap1 f xs = sconcat (fmap f xs)
