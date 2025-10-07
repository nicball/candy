module Refcount
  ( Refcount
  , newRefcount
  , incRef
  , decRef
  , deref
  , withRefcount

  , Cache
  , newCache
  , deleteCache
  , lookupCache
  ) where

import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import Control.Monad (when)
import Control.Exception (assert, finally)
import Data.Cache.LRU.IO qualified as LRU

data Refcount a = Refcount
  { value :: a
  , count :: IORef Int
  , finalizer :: IO ()
  }

newRefcount :: a -> IO () -> IO (Refcount a)
newRefcount value finalizer = do
  count <- newIORef 1
  pure Refcount{..}

incRef :: Refcount a -> IO ()
incRef rc = modifyIORef rc.count (+ 1)

decRef :: Refcount a -> IO ()
decRef rc = do
  count <- readIORef rc.count
  when (count == 1) rc.finalizer
  writeIORef rc.count (count - 1)

deref :: Refcount a -> IO a
deref rc = do
  count <- readIORef rc.count
  assert (count > 0) (pure rc.value)

withRefcount :: Refcount a -> (a -> IO b) -> IO b
withRefcount rc action = do
  incRef rc
  action rc.value `finally` decRef rc

newtype Cache k v = Cache (LRU.AtomicLRU k (Refcount v))

newCache :: Ord k => Int -> IO (Cache k v)
newCache size = Cache <$> LRU.newAtomicLRU (Just . fromIntegral $ size)

deleteCache :: Ord k => Cache k v -> IO ()
deleteCache (Cache lru) =
  mapM_ decRef . fmap snd =<< LRU.toList lru

lookupCache :: Ord k => k -> IO v -> (v -> IO ()) -> Cache k v -> IO (Refcount v)
lookupCache key new delete (Cache lru) = do
  LRU.lookup key lru >>= flip maybe pure do
    value <- new
    rc <- newRefcount value (delete value)
    LRU.maxSize lru >>= maybe (pure ()) \limit -> do
      s <- LRU.size lru
      when (s == fromIntegral limit) do
        LRU.pop lru >>= maybe (pure ()) (decRef . snd)
    LRU.insert key rc lru
    pure rc
