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

import Control.Exception (assert, finally)
import Control.Monad (when)
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import Data.LruCache.IO.Finalizer qualified as LRU

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

newtype Cache k v = Cache (LRU.LruHandle k (Refcount v))

newCache :: Int -> IO (Cache k v)
newCache size = Cache <$> LRU.newLruHandle size

deleteCache :: Cache k v -> IO ()
deleteCache (Cache (LRU.LruHandle r)) = mapM_ (decRef . fst) =<< readIORef r

lookupCache :: (Hashable k, Ord k) => k -> IO v -> (v -> IO ()) -> Cache k v -> IO (Refcount v)
lookupCache key new delete (Cache lru) = LRU.cached lru key newRc decRef
  where
    newRc = do
      value <- new
      newRefcount value (delete value)
