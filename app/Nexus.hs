module Nexus
  ( Nexus
  , ListenerID
  , newNexus
  , addListener
  , removeListener
  , notify
  ) where

import Data.IORef
import Data.IntMap
import Data.IntMap qualified as IntMap

data Nexus a = Nexus
  { listeners :: IORef (IntMap (a -> IO ()))
  , counter :: IORef Int
  }

newtype ListenerID = ListenerID Int

newNexus :: IO (Nexus a)
newNexus = Nexus <$> newIORef IntMap.empty <*> newIORef 0

addListener :: (a -> IO ()) -> Nexus a -> IO ListenerID
addListener action nexus = do
  token <- readIORef nexus.counter
  modifyIORef nexus.counter (+ 1)
  modifyIORef nexus.listeners (IntMap.insert token action)
  pure (ListenerID token)

removeListener :: ListenerID -> Nexus a -> IO ()
removeListener (ListenerID lid) nexus = modifyIORef nexus.listeners (IntMap.delete lid)

notify :: a -> Nexus a -> IO ()
notify a nexus = mapM_ ($ a) =<< readIORef nexus.listeners
