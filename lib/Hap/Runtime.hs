{-# LANGUAGE BangPatterns #-}

module Hap.Runtime
  ( Errors.Cycle(..)

  , Types.Cell
  , Types.Env(..)
  , Types.Flag(..)
  , Types.FlagSet
  , Types.Handler(..)
  , Types.HapT(HapT)
  , Types.Id
  , Types.SomeCell
  , Types.WeakCell

  , clearFlag
  , get
  , getFlag
  , new
  , newEmptyEnv
  , newId
  , on
  , onChange
  , onSet
  , run
  , sequencePoint
  , set
  , setFlag
  , stop
  , unsafeGetEnv
  ) where

import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits
import Data.IORef
import Data.IntSet (IntSet)
import Hap.Runtime.Errors
import Hap.Runtime.Types
import Prelude hiding (id)
import System.Mem.Weak
import qualified Data.IntSet as IntSet
import qualified Hap.Runtime.Errors as Errors
import qualified Hap.Runtime.Types as Types

logMessage :: (Applicative m) => Env m -> String -> m ()
logMessage env
  = when (getFlag LoggingEnabledFlag (envFlags env))
  . envOutputStr env . (++ "\n")

--------------------------------------------------------------------------------
-- Environment Operations
--------------------------------------------------------------------------------

-- Create a new empty environment.
newEmptyEnv :: (String -> m ()) -> [Flag] -> IO (Env m)
newEmptyEnv outputStr flags = do
  let flagSet = foldr setFlag mempty flags
  listeners <- newIORef []
  next <- newIORef (0 :: Id)
  queue <- newIORef []
  graphicsChan <- if getFlag GraphicsEnabledFlag flagSet
    then Just <$> atomically newTChan
    else pure Nothing
  pure Env
    { envListeners = listeners
    , envNext = next
    , envQueue = queue
    , envOutputStr = outputStr
    , envFlags = flagSet
    , envGraphicsChan = graphicsChan
    }

-- Run a computation in the given environment.
run :: (Functor m) => Env m -> HapT m a -> m a
run env (HapT action) = fst <$> action env

-- Unsafely access the environment from within a Hap computation.
unsafeGetEnv :: (Applicative m) => HapT m (Env m)
unsafeGetEnv = HapT \ env -> pure (env, [])

--------------------------------------------------------------------------------
-- Cell Operations
--------------------------------------------------------------------------------

-- Allocate a fresh cell ID.
newId :: (MonadIO m) => HapT m Id
newId = HapT \ env -> liftIO do
  next <- readIORef $ envNext env
  writeIORef (envNext env) (next + 1)
  pure (next, [])

-- Allocate a new cell containing the given expression.
new :: (MonadIO m) => Maybe String -> HapT m a -> HapT m (Cell m a)
new label action = do
  id <- newId
  HapT \ env -> do
    expression <- liftIO $ newIORef action
    cache <- liftIO $ newIORef Empty
    sources <- liftIO $ newIORef []
    sinks <- liftIO $ newIORef []
    name <- liftIO $ newIORef label
    let
      cell = Cell
        { cellId = id
        , cellExpression = expression
        , cellCache = cache
        , cellSources = sources
        , cellSinks = sinks
        , cellName = name
        }
    debug <- debugName cell
    logMessage env $ concat ["new(", debug, ")"]
    pure (cell, [])

debugName :: (MonadIO m) => Cell m a -> m String
debugName cell = (('#' : show (cellId cell)) ++)
  . maybe "" (("[" ++) . (++ "]"))
  <$> liftIO (readIORef $ cellName cell)

-- Get the value of a cell.
get :: (Show a, MonadIO m) => Cell m a -> HapT m a
get cell = HapT \ !env -> do
  cache <- liftIO $ readIORef $ cellCache cell
  name <- debugName cell
  logMessage env $ concat ["get(", name, ") {"]
  result <- case cache of
    Full v -> do
      logMessage env $ concat
        [name, ".cache = Full ", show v]
      pure (v, [SomeCell cell])
    Empty -> do
      logMessage env $ concat
        [name, ".cache = Empty"]
      -- Replace the cache with a black hole during evaluation to detect
      -- reference cycles.
      liftIO $ writeIORef (cellCache cell) Blackhole
      logMessage env $ concat
        [name, ".cache = Blackhole"]
      expression <- liftIO $ readIORef $ cellExpression cell
      (v, ds) <- unHapT expression env
      liftIO $ writeIORef (cellCache cell) (Full v)
      logMessage env $ concat
        [name, ".cache = Full ", show v]
      liftIO $ writeIORef (cellSources cell) ds
      wc <- makeWeakCell cell
      liftIO $ forM_ ds \ (SomeCell d) -> modifyIORef' (cellSinks d) (wc :)
      pure (v, [SomeCell cell])
    Blackhole -> liftIO $ throwIO $ Cycle $ SomeCell cell
  logMessage env "}"
  pure result

-- Set the value of a cell to a new expression.
set :: (Show a, MonadIO m) => Cell m a -> HapT m a -> HapT m ()
set cell action = HapT \ env -> do
  name <- debugName cell
  logMessage env $ concat
    ["set(", name, ") {"]
  liftIO $ writeIORef (cellExpression cell) action
  invalidate env $ SomeCell cell
  logMessage env "}"
  pure ((), [])

-- Get the ID of a weak cell if it hasn't expired.
weakCellId :: (MonadIO m) => WeakCell m -> m (Maybe Id)
weakCellId = fmap (fmap someCellId) . strengthen

-- Make a weak reference to a cell.
makeWeakCell :: (MonadIO m) => Cell m a -> m (WeakCell m)
makeWeakCell cell = WeakCell <$> liftIO (mkWeakPtr cell Nothing)

-- Convert a weak cell into a cell reference if it hasn't expired.
strengthen :: (MonadIO m) => WeakCell m -> m (Maybe (SomeCell m))
strengthen (WeakCell wc) = liftIO do
  mc <- deRefWeak wc
  pure case mc of
    Just cell -> Just $ SomeCell cell
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Low-level Event Operations
--------------------------------------------------------------------------------

-- Add an event listener for the given cells and return the listener's ID.
on :: (MonadIO m) => IntSet -> Handler m -> HapT m Id
on cells handler = do
  n <- newId
  HapT \ env -> liftIO do
    modifyIORef' (envListeners env) ((n, cells, handler) :)
    pure (n, [])

-- Removes the event listener with the given ID.
--
-- TODO: Return the old listener so it can be restarted. (That could also be
-- implemented with a per-listener flag for pausing & resuming.)
stop :: (MonadIO m) => Id -> HapT m ()
stop listener = HapT \ env -> liftIO do
  modifyIORef' (envListeners env) $ filter
    \ (listener', _, _) -> listener' /= listener
  pure ((), [])

-- Add an action to be run when any of the given cells is set.
onSet :: (MonadIO m) => [Cell m a] -> HapT m () -> HapT m Id
onSet cells = on (IntSet.fromList $ map cellId cells) . Set

-- Add an action to be run when any of the given cells is changed, that is, when
-- it is set and the new value is not equal to the old value.
--
-- TODO: Implement this as a function or macro within Hap.
onChange :: (Eq a, Show a, MonadIO m) => [Cell m a] -> HapT m () -> HapT m Id
onChange cells action = do
  values <- mapM get cells
  state <- new (Just "'onChange' set") $ pure values
  onSet cells do
    values' <- mapM get cells
    state' <- get state
    set state $ pure values'
    when (values' /= state') action

-- Remove an observer from a cell.
removeObserver :: (MonadIO m) => SomeCell m -> SomeCell m -> m ()
removeObserver o (SomeCell cell) = do
  observers <- liftIO $ readIORef (cellSinks cell)
  observers' <- flip filterM observers \ o' -> do
    mi <- weakCellId o'
    case mi of
      Just i -> pure (someCellId o /= i)
      -- Remove expired observers as a side effect.
      Nothing -> pure False
  liftIO $ writeIORef (cellSinks cell) observers'

-- Invalidate the dependencies and dependents of a cell.
invalidate :: (MonadIO m) => Env m -> SomeCell m -> m ()
invalidate env sc@(SomeCell cell) = do
  os <- liftIO $ readIORef $ cellSinks cell
  rs <- liftIO $ readIORef $ cellSources cell
  liftIO do
    writeIORef (cellSinks cell) []
    writeIORef (cellCache cell) Empty
    writeIORef (cellSources cell) []
  forM_ rs $ removeObserver sc
  forM_ os $ invalidateWeak env
  notifySet env sc

-- Invalidate a weak cell if it's not expired.
invalidateWeak :: (MonadIO m) => Env m -> WeakCell m -> m ()
invalidateWeak env = mapM_ (invalidate env) <=< strengthen

-- Notify the environment that a cell was written.
notifySet :: (MonadIO m) => Env m -> SomeCell m -> m ()
notifySet env someCell = do
  let notifiedId = someCellId someCell
  listeners <- liftIO $ readIORef $ envListeners env
  -- Listeners are evaluated in the order they were added.
  forM_ (reverse listeners) \ (_listenerId, cells, handler) -> do
    when (notifiedId `IntSet.member` cells) case handler of
      Set action -> enqueue env action
      _ -> pure ()

-- Enqueue an action to be executed at the next sequence point.
enqueue :: (MonadIO m) => Env m -> HapT m () -> m ()
enqueue env action = liftIO do
  modifyIORef' (envQueue env) (action :)

-- Flush the queue of actions to be executed. Since these actions may enqueue
-- further actions, the whole queue is flushed at once to prevent infinite
-- loops; the feedback loop between enqueuing and flushing actions is what
-- drives evaluation.
sequencePointM :: (MonadIO m) => Env m -> m ()
sequencePointM env = do
  logMessage env "sequence_point {"
  queue <- liftIO $ readIORef $ envQueue env
  liftIO $ writeIORef (envQueue env) []
  forM_ (reverse queue) $ run env
  logMessage env "}"

sequencePoint :: (MonadIO m) => HapT m ()
sequencePoint = HapT (\ env -> ((), []) <$ sequencePointM env)

--------------------------------------------------------------------------------
-- Flag Operations
--------------------------------------------------------------------------------

clearFlag :: Flag -> FlagSet -> FlagSet
clearFlag flag (FlagSet bits) = FlagSet $ clearBit bits $ fromEnum flag

getFlag :: Flag -> FlagSet -> Bool
getFlag flag (FlagSet bits) = testBit bits $ fromEnum flag

setFlag :: Flag -> FlagSet -> FlagSet
setFlag flag (FlagSet bits) = FlagSet $ setBit bits $ fromEnum flag
