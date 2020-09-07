{-# LANGUAGE GADTs #-}

module Hap.Runtime.Types
  ( Cache(..)
  , Cell(..)
  , Env(..)
  , Flag(..)
  , FlagSet(..)
  , Handler(..)
  , HapT(..)
  , Id
  , SomeCell(..)
  , WeakCell(..)

  , someCellId
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Bits
import Data.IORef
import Data.IntSet (IntSet)
import Data.List (find)
import Data.Semigroup
import Data.Word (Word64)
import Prelude hiding (id)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem.Weak
import qualified SDL

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- The cache of a cell may be 'Empty' if the cell's value has not yet been
-- computed, 'Full' if it stores the cached result of the most recent
-- evaluation, or 'Blackhole' if it's in the process of being evaluated. If a
-- 'Blackhole' is encountered when reading a cell with 'get', this indicates a
-- reference cycle and a 'Cycle' exception is raised.
data Cache a = Empty | Full a | Blackhole

-- A cell has an identifier, an expression, a cached value, a set of references
-- to cells that it reads (sources), a set of weak references to cells that
-- read it (sinks), and an optional name.
data Cell m a = Cell
  { cellId :: !Id
  , cellExpression :: !(IORef (HapT m a))
  , cellCache :: !(IORef (Cache a))
  , cellSources :: !(IORef [SomeCell m])
  , cellSinks :: !(IORef [WeakCell m])
  , cellName :: !(IORef (Maybe String))
  }

-- The environment contains a set of event listeners, a source of fresh IDs for
-- cells and handlers, a queue of actions scheduled to be run at the next
-- sequence point, and a set of flags.
data Env m = Env
  { envListeners :: !(IORef [(Id, IntSet, Handler m)])
  , envNext :: !(IORef Id)
  , envQueue :: !(IORef [HapT m ()])
  , envOutputStr :: String -> m ()
  , envFlags :: !FlagSet
  , envGraphicsChan :: Maybe (TChan (SDL.Renderer -> IO ()))
  }

data Flag
  = GraphicsEnabledFlag
  | LoggingEnabledFlag
  deriving (Enum)

data FlagSet = FlagSet !Word64

-- A 'Handler' is an expression enqueued in response to an event, tagged with
-- the event type for filtering events. A 'Set' event indicates that a cell was
-- written.
--
-- TODO: 'Add' and 'Remove' indicate that a value was inserted into or removed
-- from a cell whose value is a container.
data Handler m
  = Set !(HapT m ())
  | Add !(HapT m ())
  | Remove !(HapT m ())

-- An expression may read and alter the contents of the environment, and perform
-- I/O. It returns a result as well as a list of references to the cells that it
-- reads while computing a result.
newtype HapT m a = HapT { unHapT :: Env m -> m (a, [SomeCell m]) }

-- An ID is a globally unique integer used to identify cells and listeners.
type Id = Int

-- A cell with a hidden type. This is used to work with heterogeneous
-- collections of cells.
data SomeCell m where
  SomeCell :: !(Cell m a) -> SomeCell m

-- A weak cell is a weak reference to a cell. Cells use weak references to track
-- their observers (the cells that need to be notified when the current cell is
-- invalidated) because otherwise the dataflow graph would be fully connected
-- and no memory would ever be reclaimed.
data WeakCell m where
  WeakCell :: !(Weak (Cell m a)) -> WeakCell m

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- FlagSet

instance Semigroup FlagSet where
  FlagSet a <> FlagSet b = FlagSet (a .|. b)

instance Monoid FlagSet where
  mempty = FlagSet 0

-- HapT

-- Map over the result of an expression.
instance (Monad m) => Functor (HapT m) where
  fmap f (HapT action) = HapT \ env -> do
    (result, sources) <- action env
    pure (f result, sources)

-- Embed values in an expression or join expressions by function application.
instance (Monad m) => Applicative (HapT m) where
  pure x = HapT (\ _env -> pure (x, []))
  HapT mf <*> HapT mx = HapT \ env -> do
    (f, sources) <- mf env
    (x, sources') <- mx env
    pure (f x, union sources sources')

-- Sequence expressions, introducing a sequence point to flush the queue.
instance (MonadIO m) => Monad (HapT m) where
  return = pure
  HapT cmd >>= f = HapT \ env -> do
    (a, cs) <- cmd env
    -- sequencePointM env
    (b, ds) <- unHapT (f a) env
    pure (b, union cs ds)

instance MonadTrans HapT where
  lift action = HapT \ _env -> do
    result <- action
    pure (result, [])

-- Arbitrary I/O actions can be executed in an expression.
--
-- FIXME: This exposes evaluation order; I/O actions should not be allowed in
-- "pure" expressions (e.g., event conditions), even though they use I/O
-- internally. It also allows weak cel references: an expression can read a cell
-- but not record the dependency by wrapping the read in 'liftIO' + 'run'.
instance (MonadIO m) => MonadIO (HapT m) where
  liftIO action = HapT \ _env -> do
    result <- liftIO action
    pure (result, [])

-- This allows the body of a handler to refer to the handler itself, e.g., to
-- implement automatic stopping.
instance (MonadIO m) => MonadFix (HapT m) where
  mfix action = HapT \ env -> do
    signal <- liftIO newEmptyMVar
    argument <- liftIO $ unsafeInterleaveIO $ takeMVar signal
    (result, sources) <- unHapT (action argument) env
    liftIO $ putMVar signal result
    pure (result, sources)

-- SomeCell

instance Show (SomeCell m) where
  show (SomeCell cell) = "#" ++ show (cellId cell)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- Get the ID of a cell with a hidden type.
someCellId :: SomeCell m -> Id
someCellId (SomeCell cell) = cellId cell

-- The union of two sets of cells with hidden types, removing duplicates.
union :: [SomeCell m] -> [SomeCell m] -> [SomeCell m]
union xs ys = case xs of
  [] -> ys
  x : xs' -> case find (\ y -> someCellId x == someCellId y) ys of
    Just{} -> union xs' ys
    Nothing -> x : union xs' ys
