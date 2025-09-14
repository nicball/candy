module Main (main) where

import Graphics.UI.GLFW qualified as GLFW
import Graphics.GL
import Control.Monad (unless, when)
import Data.Function (fix)

import GL (withGLFW, withWindow, getSlot, setSlot, viewportSlot, Viewport(..), Resolution(..))
import Config (Config(..), FaceID(..), Color(..))
import Config qualified
import Window (flush, withDefaultWindowManager, withDemoWindow, WindowManager(registerWindow), scroll, sendKey, sendChar)

config :: Config
config = Config
  { face = FaceID
    { sizePx = 24
    , path = "/nix/store/4c819hv4pvz4l37yxf391mwwvwdhvia9-source-han-serif-2.003/share/fonts/opentype/source-han-serif/SourceHanSerif.ttc"
    , index = 17

    -- , path = "/nix/store/569nxifmwb4r26phghxyn4xszdg7xjxm-source-han-sans-2.004/share/fonts/opentype/source-han-sans/SourceHanSans.ttc"
    -- , index = 27

    -- , path = "/nix/store/vl44mgyhq46plr28vfj06zj9lk89jyaw-liberation-fonts-2.1.5/share/fonts/truetype/LiberationSans-Regular.ttf"
    -- , path = "/nix/store/hibcvpqv3w7s7fpl3wgd8c33hs0znywq-Iosevka-33.2.3/share/fonts/truetype/Iosevka-ExtendedMedium.ttf"
    -- , path = "/nix/store/46g6p6698lc50ypik6mgg0wf3q23gzqz-dejavu-fonts-2.37/share/fonts/truetype/DejaVuSansMono.ttf"
    -- , index = 0
    }

  , foreground = Config.nord6
  , background = Config.nord0 { alpha = 0.8 }
  , primarySelectionForeground = Config.nord0
  , primarySelectionBackground = Config.nord10
  , primaryCursorForeground = Config.nord0
  , primaryCursorBackground = Config.nord8
  , cursorVerticalRangeOnScreen = (0.1, 0.9)
  , cursorHorizontalRangeOnScreen = (0.1, 0.9)
  , lineNumbersForeground = Config.nord10
  , lineNumbersBackground = Config.nord1 { alpha = 0.8 }
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
        GLFW.setCharCallback win . Just $ \_ char -> sendChar char wm >> redraw wm win

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
      let Color{..} = config.background in
        glClearColor red green blue alpha
      glClear GL_COLOR_BUFFER_BIT

      Viewport 0 0 w h <- getSlot viewportSlot
      flush (Resolution w h) wm
      -- drawWindow (Resolution w h) (DemoWindow undefined (undefined :: DefaultWindowManager) config)
      GLFW.swapBuffers win
