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
import WindowManager
import EditorWindow
import Bar

main :: IO ()
main = do
  withGLFW . withWindow 800 600 "Candy" $ \win -> do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    ew <- fromPath "./app/Window.hs" :: IO DefaultEditorWindow
    withDefaultWindowManager ew \wm -> do
      withDefaultBar \bar -> do
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
      -- readIORef config >>= \Config{background = Color{..}} ->
      let Color{..} = nord1 in
        glClearColor red green blue alpha
      glClear GL_COLOR_BUFFER_BIT

      Viewport 0 0 w h <- getSlot viewportSlot
      draw (Resolution w h) wm
      GLFW.swapBuffers win
