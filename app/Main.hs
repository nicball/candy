module Main (main) where

import Graphics.UI.GLFW qualified as GLFW
import Graphics.GL
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Data.Function (fix)
import Data.IORef (writeIORef, readIORef, newIORef)

import GL 
import Config 
import Window 

config :: Config
config = Config
  { face = FaceID
    { sizePx = 24
    -- , path = "/nix/store/4c819hv4pvz4l37yxf391mwwvwdhvia9-source-han-serif-2.003/share/fonts/opentype/source-han-serif/SourceHanSerif.ttc"
    -- , index = 17

    , path = "/nix/store/569nxifmwb4r26phghxyn4xszdg7xjxm-source-han-sans-2.004/share/fonts/opentype/source-han-sans/SourceHanSans.ttc"
    , index = 27

    -- , path = "/nix/store/vl44mgyhq46plr28vfj06zj9lk89jyaw-liberation-fonts-2.1.5/share/fonts/truetype/LiberationSans-Regular.ttf"
    -- , path = "/nix/store/hibcvpqv3w7s7fpl3wgd8c33hs0znywq-Iosevka-33.2.3/share/fonts/truetype/Iosevka-ExtendedMedium.ttf"
    -- , path = "/nix/store/46g6p6698lc50ypik6mgg0wf3q23gzqz-dejavu-fonts-2.37/share/fonts/truetype/DejaVuSansMono.ttf"
    -- , index = 0
    }

  , foreground = nord6
  , background = nord0 -- { alpha = 0.8 }
  , primarySelectionForeground = nord0
  , primarySelectionBackground = nord10
  , primaryCursorForeground = nord0
  , primaryCursorBackground = nord8
  , cursorVerticalRangeOnScreen = (0.1, 0.9)
  , cursorHorizontalRangeOnScreen = (0.1, 0.9)
  , lineNumbersForeground = nord3
  , lineNumbersBackground = Color 0 0 0 0
  , lineNumbersCurrentForeground = nord0
  , lineNumbersCurrentBackground = nord3
  , barBackground = nord3
  , barForeground = nord6
  }

main :: IO ()
main = do
  withGLFW . withWindow 800 600 "Candy" $ \win -> do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    withDefaultWindowManager config \wm -> do
      withDefaultEditorWindow config \dw -> do
        withDefaultBar config \bar -> do
          _ <- registerEditorWindow dw wm
          setBar bar wm

          shouldRedraw <- newIORef False

          GLFW.setWindowRefreshCallback win . Just $ \_ -> writeIORef shouldRedraw True
          GLFW.setFramebufferSizeCallback win . Just $ \_ w h -> onResize win w h
          uncurry (onResize win) =<< GLFW.getFramebufferSize win
          GLFW.setScrollCallback win . Just $ \_ x y -> scroll x y wm >> writeIORef shouldRedraw True
          GLFW.setKeyCallback win . Just $ \_ key _ state mods -> do
            when (state /= GLFW.KeyState'Released) do
              sendKey key mods wm
              writeIORef shouldRedraw True
          GLFW.setCharCallback win . Just $ \_ char -> sendChar char wm >> writeIORef shouldRedraw True

          timerFreq <- GLFW.getTimerFrequency
          fix $ \loop -> do
            begin <- GLFW.getTimerValue
            GLFW.waitEvents
            readIORef shouldRedraw >>= flip when do
              redraw wm win
              writeIORef shouldRedraw False
            end <- GLFW.getTimerValue
            let sleepAmount = 25000 - (end - begin) * 1000000 `div` timerFreq
            when (sleepAmount > 0) (threadDelay . fromIntegral $ sleepAmount)
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
      draw (Resolution w h) wm
      -- drawWindow (Resolution w h) (DemoWindow undefined (undefined :: DefaultWindowManager) config)
      GLFW.swapBuffers win
