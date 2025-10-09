module Bar
  ( DefaultBar
  , withDefaultBar
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Text.Foreign qualified as Text
import Data.Text qualified as Text

import Config (ConfigT(..), config)
import GL (Resolution(..), Texture, clearViewport)
import Weaver (drawTextCached)
import Window (Bar(..), Draw(..), Status(..), GetBox(..), minimumSize)
import Refcount (deref, Refcount, incRef, decRef)
import Box (AnyBox, drawClipping, drawableBox, textureBox, withPadding)

data DefaultBar = DefaultBar
  { barBox :: IORef (Maybe BarBox)
  }

withDefaultBar :: (DefaultBar -> IO a) -> IO a
withDefaultBar action = do
  barBox <- newIORef Nothing
  action DefaultBar{..}

data BarBox = BarBox
  { box :: AnyBox
  , textTexture :: Refcount (Texture, Resolution)
  }

newBarBox :: Status -> IO BarBox
newBarBox status = do
  cfg <- readIORef config
  let text = (status.name <>) . Text.pack $ " " <> show status.mode <> " " <> show status.selections
  textTexture <- drawTextCached cfg.face [(0, Text.lengthWord8 text, cfg.barForeground)] text
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
    barBox <- newBarBox status
    readIORef bar.barBox >>= maybe (pure ()) deleteBarBox
    writeIORef bar.barBox (Just barBox)
