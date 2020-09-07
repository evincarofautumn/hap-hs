{-# LANGUAGE GADTs #-}

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
data Cycle where
  Cycle :: !(SomeCell m) -> Cycle
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
