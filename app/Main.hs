{-# LANGUAGE OverloadedStrings
           , BlockArguments
           , DeriveAnyClass
           , PatternSynonyms
           , TemplateHaskell
           #-}

module Main (main)  where

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
  , glViewport
  )
import Control.Monad (unless)
import Data.Function (fix)

import GL (withGLFW, withWindow)
import Weaver (drawText, setResolution, withWeaver)
import Config (Config(..))

config :: Config
config = Config
  { configFontSizePx = 32
  , configFontPath = "/nix/store/lby2cfffnpqw88mn1caqwc5x8dlwbk6d-source-han-serif-2.003/share/fonts/opentype/source-han-serif/SourceHanSerif.ttc"
      -- "/nix/store/x0rjl3c84fmq5gs88w2xggaryp3y9czn-Iosevka-32.5.0/share/fonts/truetype/Iosevka-ExtendedItalic.ttf"
  , configForeground = (0.93, 0.94, 0.96)
  , configBackground = (0.18, 0.2, 0.25)
  }

main :: IO ()
main = do
  withGLFW . withWindow 800 600 "Candy" $ \win ->
    withWeaver config \weaver -> do
      glEnable GL_BLEND
      glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

      uncurry (onResize weaver win) =<< GLFW.getFramebufferSize win
      GLFW.setFramebufferSizeCallback win (Just (onResize weaver))

      fix \loop -> do
        let (clearColorR, clearColorG, clearColorB) = configBackground config
        glClearColor clearColorR clearColorG clearColorB 1
        glClear GL_COLOR_BUFFER_BIT
        drawText weaver "超级牛力#[编辑器]！ fi"
        GLFW.swapBuffers win
        GLFW.waitEvents
        c <- GLFW.windowShouldClose win
        unless c loop
  where
    onResize weaver _ w h = do
      glViewport 0 0 (fromIntegral w) (fromIntegral h)
      setResolution w h weaver
