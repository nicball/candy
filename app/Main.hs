{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Graphics.UI.GLFW qualified as GLFW
import Graphics.GL
import Control.Monad (unless, when)
import Data.Function (fix)

import GL (withGLFW, withWindow, getSlot, setSlot, viewportSlot, Viewport(..), Resolution(..))
import Config (Config(..), FaceID(..), Color(..))
import Window (flush, withDefaultWindowManager, withDemoWindow, WindowManager(registerWindow), scroll, sendKey)

config :: Config
config = Config
  { configFace = FaceID
    { faceIDSizePx = 24
    -- , faceIDPath = "/nix/store/4c819hv4pvz4l37yxf391mwwvwdhvia9-source-han-serif-2.003/share/fonts/opentype/source-han-serif/SourceHanSerif.ttc"
    -- , faceIDIndex = 17

    -- , faceIDPath = "/nix/store/569nxifmwb4r26phghxyn4xszdg7xjxm-source-han-sans-2.004/share/fonts/opentype/source-han-sans/SourceHanSans.ttc"
    -- , faceIDIndex = 27

    -- , faceIDPath = "/nix/store/vl44mgyhq46plr28vfj06zj9lk89jyaw-liberation-fonts-2.1.5/share/fonts/truetype/LiberationSans-Regular.ttf"
    , faceIDPath = "/nix/store/hibcvpqv3w7s7fpl3wgd8c33hs0znywq-Iosevka-33.2.3/share/fonts/truetype/Iosevka-ExtendedMedium.ttf"
    -- , faceIDPath = "./font.ttf"
    -- , faceIDPath = "/nix/store/46g6p6698lc50ypik6mgg0wf3q23gzqz-dejavu-fonts-2.37/share/fonts/truetype/DejaVuSansMono.ttf"
    , faceIDIndex = 0
    }

  , configForeground = Color 0.93 0.94 0.96 1
  , configBackground = Color 0.18 0.2 0.25 0.8
  , configPrimarySelectionForeground = Color 0.18 0.2 0.25 1
  , configPrimarySelectionBackground = Color 0.37 0.5 0.67 1
  , configPrimaryCursorForeground = Color 0.18 0.2 0.25 1
  , configPrimaryCursorBackground = Color 0.53 0.75 0.82 1
  }

main :: IO ()
main = do
  withGLFW . withWindow 800 600 "Candy" $ \win -> do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    withDefaultWindowManager \wm -> do
      withDemoWindow config \dw -> do
        _ <- registerWindow dw wm

        GLFW.setFramebufferSizeCallback win (Just \_ w h -> onResize win w h)
        uncurry (onResize win) =<< GLFW.getFramebufferSize win
        GLFW.setWindowRefreshCallback win (Just (redraw wm))
        GLFW.setScrollCallback win (Just \_ x y -> scroll x y wm >> redraw wm win)
        GLFW.setKeyCallback win . Just $ \_ key _ state mods -> do
          when (state /= GLFW.KeyState'Released) do
            sendKey key mods wm
            redraw wm win

        fix \loop -> do
          GLFW.waitEvents
          -- redraw win
          c <- GLFW.windowShouldClose win
          unless c loop
  where

    onResize _ w h = do
      setSlot viewportSlot (Viewport 0 0 w h)

    redraw wm win = do
      GLFW.getWindowSize win >>= uncurry (onResize win)
      let Color{..} = configBackground config in
        glClearColor colorRed colorGreen colorBlue colorAlpha
      glClear GL_COLOR_BUFFER_BIT

      Viewport 0 0 w h <- getSlot viewportSlot
      flush (Resolution w h) wm
      -- drawWindow (Resolution w h) (DemoWindow undefined (undefined :: DefaultWindowManager) config)
      GLFW.swapBuffers win
