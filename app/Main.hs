{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Main where

import qualified SDL
import SDL (($=))
import qualified FreeType as FT
import Data.Char (ord)
import qualified Foreign.Storable as Ptr
import qualified Foreign.Marshal.Array as Ptr
import qualified Data.ByteString as BSL

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Candy" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  mainLoop renderer
  SDL.destroyWindow window
  pure ()
  where
    mainLoop renderer = do
      SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 0
      SDL.rendererDrawBlendMode renderer $= SDL.BlendNone
      SDL.clear renderer
      niu <- createTextureFromCodePoint renderer '牛'
      SDL.copy renderer niu Nothing Nothing
      SDL.present renderer
      event <- SDL.waitEvent
      case SDL.eventPayload event of
        SDL.KeyboardEvent ke | SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeQ -> pure ()
        _ -> mainLoop renderer

createTextureFromCodePoint :: SDL.Renderer -> Char -> IO SDL.Texture
createTextureFromCodePoint renderer char =
  FT.ft_With_FreeType \lib -> do
    FT.ft_With_Face lib "/nix/store/mvzg44lxmj70ljnq7bc7zrr0yc4xyjc3-source-han-serif-2.001/share/fonts/opentype/source-han-serif/SourceHanSerif.ttc" 0 \face -> do
      FT.ft_Set_Pixel_Sizes face 0 64
      glyphId <- FT.ft_Get_Char_Index face (fromIntegral . ord $ char)
      FT.ft_Load_Glyph face glyphId 0
      glyphSlot <- FT.frGlyph <$> Ptr.peek face
      FT.ft_Render_Glyph glyphSlot FT.FT_RENDER_MODE_NORMAL
      bitmap <- FT.gsrBitmap <$> Ptr.peek glyphSlot
      greyMap <- Ptr.peekArray (fromIntegral (FT.bRows bitmap) * fromIntegral (FT.bPitch bitmap)) (FT.bBuffer bitmap)
      let argbMap = concatMap (\g -> [g, 255 - g, 255 - g, 255 - g]) greyMap
      texture <- SDL.createTexture renderer SDL.BGRA8888 SDL.TextureAccessStatic (SDL.V2 (fromIntegral (FT.bWidth bitmap)) (fromIntegral (FT.bRows bitmap)))
      SDL.textureBlendMode texture $= SDL.BlendAlphaBlend
      SDL.updateTexture texture Nothing (BSL.pack argbMap) (fromIntegral (FT.bWidth bitmap) * 4)
      pure texture
