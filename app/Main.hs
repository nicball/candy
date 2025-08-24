{-# LANGUAGE OverloadedStrings
           , BlockArguments
           , DeriveAnyClass
           , PatternSynonyms
           , TemplateHaskell
           , RecursiveDo
           #-}

module Main (main) where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
  ( pattern GL_COLOR_BUFFER_BIT
  , pattern GL_BLEND
  , pattern GL_SRC_ALPHA
  , pattern GL_ONE_MINUS_SRC_ALPHA
  , glEnable
  , glBlendFunc
  , glClear
  , glClearColor
  )
import Control.Monad (unless, forM_)
import Data.Function (fix)

import GL (withGLFW, withWindow, getSlot, setSlot, viewportSlot, Viewport(..), Resolution(..))
import Config (Config(..))
import Window
  ( drawWindow
  , flush
  , newDefaultWindowManager
  , TimeWindow(TimeWindow)
  , WindowManager(registerWindow)
  )

config :: Config
config = Config
  { configFontSizePx = 24

  -- , configFontPath = "/nix/store/4c819hv4pvz4l37yxf391mwwvwdhvia9-source-han-serif-2.003/share/fonts/opentype/source-han-serif/SourceHanSerif.ttc"
  -- , configFontIndex = 17
  --
  -- , configFontPath = "/nix/store/569nxifmwb4r26phghxyn4xszdg7xjxm-source-han-sans-2.004/share/fonts/opentype/source-han-sans/SourceHanSans.ttc"
  -- , configFontIndex = 27

  -- , configFontPath = "/nix/store/vl44mgyhq46plr28vfj06zj9lk89jyaw-liberation-fonts-2.1.5/share/fonts/truetype/LiberationSans-Regular.ttf"
  -- , configFontPath = "/nix/store/hibcvpqv3w7s7fpl3wgd8c33hs0znywq-Iosevka-33.2.3/share/fonts/truetype/Iosevka-ExtendedMedium.ttf"
  -- , configFontPath = "./font.ttf"
  , configFontPath = "/nix/store/46g6p6698lc50ypik6mgg0wf3q23gzqz-dejavu-fonts-2.37/share/fonts/truetype/DejaVuSansMono.ttf"
  , configFontIndex = 0

  , configForeground = (0.93, 0.94, 0.96)
  , configBackground = (0.18, 0.2, 0.25)
  }

main :: IO ()
main = do
  withGLFW . withWindow 800 600 "Candy" $ \win -> do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    uncurry (onResize win) =<< GLFW.getFramebufferSize win
    GLFW.setFramebufferSizeCallback win (Just onResize)

    GLFW.setWindowRefreshCallback win (Just redraw)

    fix \loop -> do
      GLFW.waitEvents
      c <- GLFW.windowShouldClose win
      unless c loop
  where

    onResize win w h = do
      setSlot viewportSlot (Viewport 0 0 w h)
      redraw win

    redraw win = do
      let (clearColorR, clearColorG, clearColorB) = configBackground config
      glClearColor clearColorR clearColorG clearColorB 1
      glClear GL_COLOR_BUFFER_BIT

      wm <- newDefaultWindowManager
      rec twid <- registerWindow (TimeWindow twid wm config) wm

      Viewport 0 0 w h <- getSlot viewportSlot
      flush (Resolution w h) wm
      -- drawWindow (Resolution w h) (TimeWindow twid wm config)
      GLFW.swapBuffers win
