module Hap.Data.Monoid
  ( Maximum(..)
  , Minimum(..)
  ) where

import Data.Ord (Down(..))
import Data.Semigroup (Max(..))

newtype Maximum a = Maximum { getMaximum :: Maybe a }
  deriving (Monoid, Semigroup) via (Maybe (Max a))

newtype Minimum a = Minimum { getMinimum :: Maybe a }
  deriving (Monoid, Semigroup) via (Down (Maybe (Max (Down a))))
  deriving stock (Show)
