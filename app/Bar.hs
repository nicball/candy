module Bar
  ( DefaultBar
  , withDefaultBar
  ) where

import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Text.Foreign qualified as Text
import Data.Text qualified as Text

import Config (Config(..), config)
import GL (Resolution(..), Texture, clearViewport)
import Weaver (drawTextCached, getWeaverCached, Weaver)
import Window (Bar(..), Draw(..), Status(..), GetBox(..), minimumSize)
import Refcount (deref, Refcount, incRef, decRef)
import Box (AnyBox, drawClipping, drawableBox, textureBox, withPadding)

data DefaultBar = DefaultBar
  { barBox :: IORef (Maybe BarBox)
  , weaver :: Refcount Weaver
  }

withDefaultBar :: (DefaultBar -> IO a) -> IO a
withDefaultBar action = do
  barBox <- newIORef Nothing
  face <- (.face) <$> readIORef config
  weaver <- getWeaverCached face
  incRef weaver
  action DefaultBar{..} `finally` decRef weaver

data BarBox = BarBox
  { box :: AnyBox
  , textTexture :: Refcount (Texture, Resolution)
  }

newBarBox :: DefaultBar -> Status -> IO BarBox
newBarBox bar status = do
  cfg <- readIORef config
  let text = (status.name <>) . Text.pack $ " " <> show status.mode <> " " <> show status.selection
  weaver <- deref bar.weaver
  textTexture <- drawTextCached weaver [(0, Text.lengthWord8 text, cfg.barForeground)] text
  incRef textTexture
  (textTex, textRes) <- deref textTexture
  let box = withPadding 5 5 5 5 $ textureBox textRes textTex
  pure BarBox{..}

deleteBarBox :: BarBox -> IO ()
deleteBarBox barBox = decRef barBox.textTexture

instance Draw DefaultBar where
  draw res bar = do
    cfg <- readIORef config
    clearViewport cfg.barBackground
    barBox <- fromJust <$> readIORef bar.barBox
    drawClipping res barBox.box

instance GetBox DefaultBar where
  getBox bar = do
    barBox <- fromJust <$> readIORef bar.barBox
    pure $ drawableBox (minimumSize barBox.box) True False bar

instance Bar DefaultBar where
  setStatus bar status = do
    barBox <- newBarBox bar status
    readIORef bar.barBox >>= maybe (pure ()) deleteBarBox
    writeIORef bar.barBox (Just barBox)
