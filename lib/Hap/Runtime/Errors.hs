{-# LANGUAGE ExistentialQuantification #-}

module Hap.Runtime.Errors
  ( Cycle(..)
  ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Hap.Runtime.Types

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- The exception raised when a dependency cycle is detected.
data Cycle = forall m. Cycle !(SomeCell m)
  deriving (Typeable)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Exception Cycle

instance Show Cycle where
  show (Cycle cell) = concat
    [ "circular reference detected at cell #"
    , show $ someCellId cell
    ]
