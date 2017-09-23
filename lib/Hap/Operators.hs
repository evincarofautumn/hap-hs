module Hap.Operators
  ( (.>)
  , (.||)
  ) where

import Control.Applicative

(.>) :: (Applicative f, Ord a) => f a -> f a -> f Bool
(.>) = liftA2 (>)

(.||) :: (Applicative f) => f Bool -> f Bool -> f Bool
(.||) = liftA2 (||)
