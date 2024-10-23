{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Main where

import qualified SDL
import SDL (($=))
import qualified Data.Text.Encoding as Text
import qualified SDLPango as Pango

main :: IO ()
main = do
  SDL.initializeAll
  Pango.initialize
  window <- SDL.createWindow "Candy" SDL.defaultWindow
  mainLoop window
  SDL.destroyWindow window
  pure ()
  where

    mainLoop window = do
      winsurface <- SDL.getWindowSurface window
      SDL.surfaceFillRect winsurface Nothing (SDL.V4 255 255 255 255)
      drawWithPango window
      SDL.updateWindowSurface window
      event <- SDL.waitEvent
      case SDL.eventPayload event of
        SDL.KeyboardEvent ke | SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeQ -> pure ()
        _ -> mainLoop window

    drawWithPango window = do
      ctxt <- Pango.createContext
      Pango.setDefaultColor ctxt Pango.matrixTransparentBackBlackLetter
      Pango.setMinimumSize ctxt 640 0
      Pango.setMarkup ctxt (Text.encodeUtf8 "<span font=\"serif 64px\"><i>Italic</i> <b>Bold</b>\n<span foreground=\"blue\" size=\"x-large\">Large Blue Text</span></span>")
      w <- Pango.getLayoutWidth ctxt
      h <- Pango.getLayoutHeight ctxt
      let margin = 100
      -- surface <- SDL.createRGBSurface (SDL.V2 (margin * 2 + w) (margin * 2 + h)) SDL.ABGR8888
      surface <- Pango.createSurfaceDraw ctxt
      Pango.freeContext ctxt
      SDL.surfaceBlendMode surface $= SDL.BlendAlphaBlend
      winsurface <- SDL.getWindowSurface window
      SDL.surfaceFillRect winsurface (Just (SDL.Rectangle (SDL.P (SDL.V2 margin margin)) (SDL.V2 w h))) (SDL.V4 200 200 200 255)
      _ <- SDL.surfaceBlit surface Nothing winsurface (Just (SDL.P (SDL.V2 margin margin)))
      SDL.freeSurface surface
      pure ()
