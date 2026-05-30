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
import Document

import TextLayout

main :: IO ()
main = do
  test
  withGLFW . withWindow 800 600 "Candy" $ \win -> do
    setSlot blendSlot True
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    ew <- new =<< fromFile "./app/EditorWindow.hs" :: IO DefaultEditorWindow
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
      setSlot viewportSlot (ScreenRect 0 0 w h)

    redraw wm win = do
      GLFW.getWindowSize win >>= uncurry (onResize win)
      -- readIORef config >>= \Config{background = Color{..}} ->
      withSlot clearColorSlot nord1 do
        glClear GL_COLOR_BUFFER_BIT

      ScreenRect 0 0 w h <- getSlot viewportSlot
      draw (Resolution w h) wm
      GLFW.swapBuffers win

{-
data DummyWindow = DummyWindow
instance EditorWindow DummyWindow where
  new _ = pure DummyWindow
  getDocument _ = fromFile "./test.txt"
  fork _ = pure DummyWindow
  close _ = pure ()
  getStatus _ = pure Status{selections=Selections (Selection (Coord 0 0) (Coord 0 0) :| []), mode=NormalMode, name="Hello, World!"}
instance Scroll DummyWindow where
  scroll _ _ _ = pure ()
instance SendKey DummyWindow where
  sendKey _ _ _ = pure ()
instance SendChar DummyWindow where
  sendChar _ _ = pure ()
instance GetBox DummyWindow where
  getBox _ = pure (AnyBox DummyBox)

data DummyBox = DummyBox
instance Box DummyBox where
  minimumSize _ = Resolution 0 0
  expandableX _ = True
  expandableY _ = True
  safeDraw _ _ = pure ()
-}
