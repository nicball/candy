module Bar
  ( DefaultBar
  , withDefaultBar
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text.Foreign qualified as Text
import Data.Text qualified as Text
import Graphics.GL (pattern GL_COLOR_BUFFER_BIT, glClear, glClearColor)

import Config (Color(..), Config(..), config)
import GL (drawQuadTexture, quadFromTopLeftWH, withPoster, Poster, Resolution(..))
import Weaver (drawTextCached, withWeaver, Weaver)
import Window (Bar(..), Draw(..), Status(..))

data DefaultBar = DefaultBar
  { status :: IORef (Maybe Status)
  , weaver :: Weaver
  , poster :: Poster
  }

withDefaultBar :: (DefaultBar -> IO a) -> IO a
withDefaultBar action = do
  status <- newIORef Nothing
  face <- (.face) <$> readIORef config
  withWeaver face \weaver -> do
    withPoster \poster -> do
      action DefaultBar{..}

instance Draw DefaultBar where
  draw res bar = do
    cfg <- readIORef config
    let Color{..} = cfg.barBackground in
      glClearColor red green blue alpha
    glClear GL_COLOR_BUFFER_BIT
    readIORef bar.status >>= maybe (pure ()) \status -> do
      let text = Text.pack $ show status.mode <> " " <> show status.selection
      (ctxTex, ctxRes) <- drawTextCached bar.weaver [(0, Text.lengthWord8 text, cfg.barForeground)] text
      drawQuadTexture bar.poster res ctxTex $ quadFromTopLeftWH 5 5 ctxRes.w ctxRes.h
      pure ()

instance Bar DefaultBar where
  setStatus w ctx = writeIORef w.status (Just ctx)
