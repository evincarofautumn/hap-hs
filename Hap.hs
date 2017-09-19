{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecursiveDo #-}

module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception (Exception, throwIO)
import Control.Monad
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List (find)
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem.Weak
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

main :: IO ()
main = do
  env0 <- newEmptyEnv
  let log = liftIO . putStrLn . (">>> " ++)
  result <- run env0 $ do

    log "var a = 2;"
    a <- cell (pure 2)

    log "var b = 3;"
    b <- cell (pure 3)

    log "var c = a * b;"
    c <- cell ((*) <$> get a <*> get b)

    log "on set (a) { print ('a was set'); }"
    onSet [a] $ liftIO $ putStrLn "a was set"

    log "on change (b) { print ('b was changed'); }"
    onChange [b] $ liftIO $ putStrLn "b was changed"

    log "after (c > 10) { print ('c became greater than 10 for the first time'); }"
    after (get c .> pure 10) $ liftIO $ putStrLn "c became greater than 10 for the first time"

    log "whenever (c > 10) { print ('c became greater than 10'); }"
    whenever (get c .> pure 10) $ liftIO $ putStrLn "c became greater than 10"

    log "a <- 5;"
    set a $ pure 5

    log "a <- 5;"
    set a $ pure 5

    log "b <- 0;"
    set b $ pure 0

    log "b <- 0;"
    set b $ pure 0

    log "b <- 7;"
    set b $ pure 7

    log "b <- 0;"
    set b $ pure 0

    log "b <- 7;"
    set b $ pure 7

    log "return c;"
    get c

  print result

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- The environment contains a set of event listeners, a source of fresh IDs for
-- cells and handlers, and a queue of actions scheduled to be run at the next
-- sequence point.
data Env = Env
  { envListeners :: !(IORef [(Id, IntSet, Handler)])
  , envNext :: !(IORef Id)
  , envQueue :: !(IORef [Exp ()])
  }

-- An ID is a globally unique integer used to identify cells and listeners.
type Id = Int

-- A cell has an identifier, an expression, a cached value, a set of references
-- to cells that it reads, and a set of weak references to cells that read it.
data Cell a = Cell
  { cellId :: !Id
  , cellExpression :: !(IORef (Exp a))
  , cellCache :: !(IORef (Cache a))
  , cellReads :: !(IORef [SomeCell])
  , cellObservers :: !(IORef [WeakCell])
  }

-- The cache of a cell may be 'Empty' if the cell's value has not yet been
-- computed, 'Full' if it stores the cached result of the most recent
-- evaluation, or 'Blackhole' if it's in the process of being evaluated. If a
-- 'Blackhole' is encountered when reading a cell with 'get', this indicates a
-- reference cycle and a 'Cycle' exception is raised.
data Cache a = Empty | Full a | Blackhole

-- The exception raised when a dependency cycle is detected.
data Cycle = Cycle !SomeCell
  deriving (Typeable)

-- A weak cell is a weak reference to a cell. Cells use weak references to track
-- their observers (the cells that need to be notified when the current cell is
-- invalidated) because otherwise the dataflow graph would be fully connected
-- and no memory would ever be reclaimed.
data WeakCell = forall a. WeakCell (Weak (Cell a))

-- A cell with a hidden type. This is used to work with heterogeneous
-- collections of cells.
data SomeCell = forall a. SomeCell (Cell a)

-- A 'Handler' is an expression enqueued in response to an event, tagged with
-- the event type for filtering events. A 'Set' event indicates that a cell was
-- written.
--
-- TODO: 'Add' and 'Remove' indicate that a value was inserted into or removed
-- from a cell whose value is a container.
data Handler
  = Set !(Exp ())
  | Add !(Exp ())
  | Remove !(Exp ())

-- An expression may read and alter the contents of the environment, and perform
-- I/O. It returns a result as well as a list of references to the cells that it
-- reads while computing a result.
newtype Exp a = Exp { unExp :: Env -> IO (a, [SomeCell]) }

--------------------------------------------------------------------------------
-- Environment Operations
--------------------------------------------------------------------------------

-- Create a new empty environment.
newEmptyEnv :: IO Env
newEmptyEnv = do
  listeners <- newIORef []
  next <- newIORef (0 :: Id)
  queue <- newIORef []
  pure Env
    { envListeners = listeners
    , envNext = next
    , envQueue = queue
    }

-- Run a computation in the given environment.
run :: Env -> Exp a -> IO a
run env (Exp action) = fst <$> action env

--------------------------------------------------------------------------------
-- Cell Operations
--------------------------------------------------------------------------------

-- Allocate a fresh cell ID.
newId :: Exp Id
newId = Exp $ \ env -> do
  x <- readIORef $ envNext env
  writeIORef (envNext env) (x + 1)
  pure (x, [])

-- Allocate a new cell containing the given expression.
cell :: Exp a -> Exp (Cell a)
cell exp = do
  n <- newId
  Exp $ \ env -> do
    code <- newIORef exp
    cache <- newIORef Empty
    reads <- newIORef []
    observers <- newIORef []
    let
      cell = Cell
        { cellId = n
        , cellExpression = code
        , cellCache = cache
        , cellReads = reads
        , cellObservers = observers
        }
    pure (cell, [])

-- Get the value of a cell.
get :: Cell a -> Exp a
get c = Exp $ \ env -> do
  cache <- readIORef (cellCache c)
  case cache of
    Full v -> pure (v, [SomeCell c])
    Empty -> do
      -- Replace the cache with a black hole during evaluation to detect
      -- reference cycles.
      writeIORef (cellCache c) Blackhole
      (v, ds) <- join $ unExp <$> readIORef (cellExpression c) <*> pure env
      writeIORef (cellCache c) (Full v)
      writeIORef (cellReads c) ds
      wc <- makeWeakCell c
      forM_ ds $ \ (SomeCell d) -> modifyIORef' (cellObservers d) (wc :)
      pure (v, [SomeCell c])
    Blackhole -> throwIO $ Cycle $ SomeCell c

-- Set the value of a cell to a new expression.
set :: Cell a -> Exp a -> Exp ()
set c exp = Exp $ \ env -> do
  writeIORef (cellExpression c) exp
  let sc = SomeCell c
  invalidate env sc
  pure ((), [])

-- Get the ID of a cell with a hidden type.
someCellId :: SomeCell -> Id
someCellId (SomeCell c) = cellId c

-- Get the ID of a weak cell if it hasn't expired.
weakCellId :: WeakCell -> IO (Maybe Id)
weakCellId = fmap (fmap someCellId) . strengthen

-- Make a weak reference to a cell.
makeWeakCell :: Cell a -> IO WeakCell
makeWeakCell c = WeakCell <$> mkWeakPtr c Nothing

-- Convert a weak cell into a cell reference if it hasn't expired.
strengthen :: WeakCell -> IO (Maybe SomeCell)
strengthen (WeakCell wc) = do
  mc <- deRefWeak wc
  pure $ case mc of
    Just c -> Just $ SomeCell c
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Low-level Event Operations
--------------------------------------------------------------------------------

-- Add an event listener for the given cells and return the listener's ID.
on :: IntSet -> Handler -> Exp Id
on cells handler = do
  n <- newId
  Exp $ \ env -> do
    modifyIORef' (envListeners env) ((n, cells, handler) :)
    pure (n, [])

-- Removes the event listener with the given ID.
--
-- TODO: Return the old listener so it can be restarted. (That could also be
-- implemented with a per-listener flag for pausing & resuming.)
stop :: Id -> Exp ()
stop listener = Exp $ \ env -> do
  modifyIORef' (envListeners env) $ filter
    $ \ (listener', _, _) -> listener' /= listener
  pure ((), [])

-- Add an action to be run when any of the given cells is set.
onSet :: [Cell a] -> Exp () -> Exp Id
onSet cells = on (IntSet.fromList $ map cellId cells) . Set

-- Add an action to be run when any of the given cells is changed, that is, when
-- it is set and the new value is not equal to the old value.
onChange :: (Eq a) => [Cell a] -> Exp () -> Exp Id
onChange cells action = do
  values <- mapM get cells
  state <- cell $ pure values
  onSet cells $ do
    values' <- mapM get cells
    state' <- get state
    set state $ pure values'
    when (values' /= state') action

-- Remove an observer from a cell.
removeObserver :: SomeCell -> SomeCell -> IO ()
removeObserver o (SomeCell c) = do
  observers <- readIORef (cellObservers c)
  observers' <- flip filterM observers $ \ o' -> do
    mi <- weakCellId o'
    case mi of
      Just i -> pure (someCellId o /= i)
      -- Remove expired observers as a side effect.
      Nothing -> pure False
  writeIORef (cellObservers c) observers'

-- Invalidate the dependencies and dependents of a cell.
invalidate :: Env -> SomeCell -> IO ()
invalidate env sc@(SomeCell c) = do
  os <- readIORef $ cellObservers c
  rs <- readIORef $ cellReads c
  writeIORef (cellObservers c) []
  writeIORef (cellCache c) Empty
  writeIORef (cellReads c) []
  forM_ rs $ removeObserver sc
  forM_ os $ invalidateWeak env
  notifySet env sc

-- Invalidate a weak cell if it's not expired.
invalidateWeak :: Env -> WeakCell -> IO ()
invalidateWeak env = mapM_ (invalidate env) <=< strengthen

-- Notify the environment that a cell was written.
notifySet :: Env -> SomeCell -> IO ()
notifySet env sc@(SomeCell c) = do
  let i = someCellId sc
  listeners <- readIORef $ envListeners env
  -- Listeners are evaluated in the order they were added.
  forM_ (reverse listeners) $ \ (n, cells, handler) -> do
    when (IntSet.member i cells) $ case handler of
      Set action -> enqueue env action
      _ -> pure ()

-- Enqueue an action to be executed at the next sequence point.
enqueue :: Env -> Exp () -> IO ()
enqueue env action = do
  modifyIORef' (envQueue env) (action :)

--------------------------------------------------------------------------------
-- High-level Event Operations
--------------------------------------------------------------------------------

-- Add an action to be run whenever a condition becomes true, that is, when it
-- changes from false to true. If true initially, the action is also run as soon
-- as the handler is added.
whenever :: Exp Bool -> Exp () -> Exp Id
whenever condition action = do
  current <- cell condition
  initial <- get current
  previous <- cell $ pure initial
  when initial action
  onChange [current] $ do
    previous' <- get previous
    current' <- get current
    set previous $ pure current'
    when (current' && not previous') action

-- Add an action to be run the first time a condition becomes true, after which
-- it's removed. If true initially, the action is run immediately and the
-- listener is not added. Returns the ID of the listener if one was added; it
-- can be stopped before the action has had a chance to run.
after :: Exp Bool -> Exp () -> Exp (Maybe Id)
after condition action = do
  current <- cell condition
  initial <- get current
  if initial
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

-- Flush the queue of actions to be executed. Since these actions may enqueue
-- further actions, the whole queue is flushed at once to prevent infinite
-- loops; the feedback loop between enqueuing and flushing actions is what
-- drives evaluation.
sequencePoint :: Env -> IO ()
sequencePoint env = do
  queue <- readIORef (envQueue env)
  writeIORef (envQueue env) []
  forM_ (reverse queue) $ run env

--------------------------------------------------------------------------------
-- Typeclass Instances
--------------------------------------------------------------------------------

-- Map over the result of an expression.
instance Functor Exp where
  fmap f (Exp action) = Exp $ \ env -> do
    (result, reads) <- action env
    pure (f result, reads)

-- Embed values in an expression or join expressions by function application.
instance Applicative Exp where
  pure x = Exp (\ env -> pure (x, []))
  Exp mf <*> Exp mx = Exp $ \ env -> do
    (f, reads) <- mf env
    (x, reads') <- mx env
    pure (f x, union reads reads')

-- Sequence expressions, introducing a sequence point to flush the queue.
instance Monad Exp where
  return = pure
  Exp cmd >>= f = Exp $ \ env -> do
    (a, cs) <- cmd env
    sequencePoint env
    (b, ds) <- unExp (f a) env
    pure (b, union cs ds)

-- Arbitrary I/O actions can be executed in an expression.
--
-- FIXME: This exposes evaluation order; I/O actions should not be allowed in
-- "pure" expressions (e.g., event conditions), even though they use I/O
-- internally. It also allows weak cel references: an expression can read a cell
-- but not record the dependency by wrapping the read in 'liftIO' + 'run'.
instance MonadIO Exp where
  liftIO action = Exp $ \ env -> do
    result <- action
    pure (result, [])

-- This allows the body of a handler to refer to the handler itself, e.g., to
-- implement automatic stopping.
instance MonadFix Exp where
  mfix action = Exp $ \ env -> do
    signal <- newEmptyMVar
    argument <- unsafeInterleaveIO $ takeMVar signal
    (result, reads) <- unExp (action argument) env
    putMVar signal result
    pure (result, reads)

instance Exception Cycle

instance Show Cycle where
  show (Cycle cell) = concat
    [ "circular reference detected at cell #"
    , show $ someCellId cell
    ]

instance Show SomeCell where
  show (SomeCell c) = "#" ++ show (cellId c)

--------------------------------------------------------------------------------
-- Utility & Convenience Functions
--------------------------------------------------------------------------------

-- The union of two sets of cells with hidden types, removing duplicates.
union :: [SomeCell] -> [SomeCell] -> [SomeCell]
union xs ys = case xs of
  [] -> ys
  x : xs' -> case find (\ y -> someCellId x == someCellId y) ys of
    Just{} -> union xs' ys
    Nothing -> x : union xs' ys

(.>) :: (Applicative f, Ord a) => f a -> f a -> f Bool
(.>) = liftA2 (>)
