{-# LANGUAGE OverloadedStrings, BlockArguments, DeriveAnyClass #-}
module Main where

import qualified SDL
import SDL (($=))
import qualified FreeType as FT
import qualified Foreign.Storable as C
import qualified Foreign.Marshal.Array as C
import qualified Foreign.C.Types as C
import qualified Data.ByteString as BSL
import qualified GI.HarfBuzz.Functions as HB
import qualified GI.HarfBuzz.Structs as HB
import qualified GI.HarfBuzz.Enums as HB
import Control.Exception (Exception, throwIO, assert)
import Control.Monad (when, forM, foldM_)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

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
      -- let fontPath = "/nix/store/mvzg44lxmj70ljnq7bc7zrr0yc4xyjc3-source-han-serif-2.001/share/fonts/opentype/source-han-serif/SourceHanSerif.ttc"
      let fontPath = "/nix/store/82vyvql4j3pbyvrb059y2wf98facdrnh-Iosevka-31.7.1/share/fonts/truetype/Iosevka-ExtendedItalic.ttf"
      SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 0
      SDL.rendererDrawBlendMode renderer $= SDL.BlendNone
      SDL.clear renderer
      renderText 64 fontPath renderer (SDL.V2 10 400) "Editor with >>= Ox Power!"
      renderText 64 fontPath renderer (SDL.V2 10 200) "超级牛力#[编辑器]！ fi"
      SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 0
      SDL.drawLine renderer (SDL.P (SDL.V2 0 400)) (SDL.P (SDL.V2 800 400))
      SDL.drawLine renderer (SDL.P (SDL.V2 0 200)) (SDL.P (SDL.V2 800 200))
      SDL.present renderer
      event <- SDL.waitEvent
      case SDL.eventPayload event of
        SDL.KeyboardEvent ke | SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeQ -> pure ()
        _ -> mainLoop renderer

data Glyph = Glyph
  { glyphWidth :: Int
  , glyphHeight :: Int
  , glyphTexture :: SDL.Texture
  , glyphIndex :: Int
  , glyphLeftBearing :: Int
  , glyphTopBearing :: Int
  }

createTexturesFromGlyphIds :: Integral a => SDL.Renderer -> Int -> FilePath -> [a]-> IO [Glyph]
createTexturesFromGlyphIds renderer sizePx path glyphIds =
  FT.ft_With_FreeType \lib -> do
    FT.ft_With_Face lib path 0 \face -> do
      FT.ft_Set_Pixel_Sizes face 0 (fromIntegral sizePx)
      forM glyphIds \glyphId -> do
        FT.ft_Load_Glyph face (fromIntegral glyphId) 0
        glyphSlot <- FT.frGlyph <$> C.peek face
        FT.ft_Render_Glyph glyphSlot FT.FT_RENDER_MODE_NORMAL
        bitmap <- FT.gsrBitmap <$> C.peek glyphSlot
        leftBearing <- FT.gsrBitmap_left <$> C.peek glyphSlot
        topBearing <- FT.gsrBitmap_top <$> C.peek glyphSlot
        let
          width = FT.bWidth bitmap
          height = let res = FT.bRows bitmap in assert (res == fromIntegral (FT.bPitch bitmap)) res
          correctedWidth = if width == 0 then fromIntegral sizePx else width
          correctedHeight = if height == 0 then 1 else height
        greyMap <- C.peekArray (fromIntegral height * fromIntegral width) (FT.bBuffer bitmap)
        let argbMap = if width == 0 then replicate (sizePx * 4) 0
                                    else concatMap (\g -> [g, 255 - g, 255 - g, 255 - g]) greyMap
        -- putStrLn $ "creating texture " ++ show width ++ " " ++ show height
        texture <- SDL.createTexture renderer SDL.BGRA8888 SDL.TextureAccessStatic (SDL.V2 (fromIntegral correctedWidth) (fromIntegral correctedHeight))
        SDL.textureBlendMode texture $= SDL.BlendAlphaBlend
        SDL.updateTexture texture Nothing (BSL.pack argbMap) (fromIntegral correctedWidth * 4)
        pure (Glyph (fromIntegral width) (fromIntegral height) texture (fromIntegral glyphId) (fromIntegral leftBearing) (fromIntegral topBearing))

data HBAllocationError = HBAllocationError deriving (Show, Exception)

createHBBufferFromText :: Text.Text -> IO HB.BufferT
createHBBufferFromText text = do
  buffer <- HB.bufferCreate
  code <- HB.bufferAllocationSuccessful buffer
  when (code == 0) (throwIO HBAllocationError)
  HB.bufferSetDirection buffer HB.DirectionTLtr
  HB.bufferSetClusterLevel buffer HB.BufferClusterLevelTMonotoneCharacters
  HB.bufferSetLanguage buffer =<< HB.languageFromString "zh"
  HB.bufferSetScript buffer =<< HB.scriptFromString "Latn"
  let utf8Text = Text.encodeUtf8 text
  HB.bufferAddUtf8 buffer utf8Text 0 (fromIntegral (BSL.length utf8Text) {-(Text.length text)-})
  pure buffer

data HBFontCreationError = HBFontCreationError FilePath deriving (Show, Exception)

createHBFontFromPath :: Int -> FilePath -> IO HB.FontT
createHBFontFromPath sizePx path = do
  blob <- HB.blobCreateFromFile (Text.pack path)
  emptyBlob <- HB.blobGetEmpty
  when (blob == emptyBlob) (throwIO (HBFontCreationError path))
  face <- HB.faceCreate blob 0
  emptyFace <- HB.faceGetEmpty
  when (face == emptyFace) (throwIO (HBFontCreationError path))
  font <- HB.fontCreate face
  emptyFont <- HB.fontGetEmpty
  when (font == emptyFont) (throwIO (HBFontCreationError path))
  HB.fontSetPpem font (fromIntegral sizePx) (fromIntegral sizePx)
  pure font

defaultFeatures :: IO [HB.FeatureT]
defaultFeatures = do
  mapM feature ["kern", "liga", "clig"]
  where
    feature str = do
      f <- featureTag str
      HB.setFeatureTStart f 0
      HB.setFeatureTStart f maxBound
      HB.setFeatureTValue f 1
      pure f
    featureTag str = do
      (suc, f) <- HB.featureFromString str
      assert (suc /= 0) (pure f)

shapeText :: Int -> FilePath -> Text.Text -> IO [(HB.GlyphInfoT, HB.GlyphPositionT)]
shapeText sizePx path text = do
  buffer <- createHBBufferFromText text
  font <- createHBFontFromPath sizePx path
  HB.shape font buffer . Just =<< defaultFeatures
  liftA2 zip (HB.bufferGetGlyphInfos buffer) (HB.bufferGetGlyphPositions buffer)

renderText :: Int -> FilePath -> SDL.Renderer -> SDL.V2 C.CInt -> Text.Text -> IO ()
renderText sizePx path renderer pos text = do
  (glyphInfos, glyphPoss) <- unzip <$> shapeText sizePx path text
  glyphIds <- mapM HB.getGlyphInfoTCodepoint glyphInfos
  -- print text
  -- print glyphIds
  textures <- createTexturesFromGlyphIds renderer sizePx path glyphIds
  foldM_ renderGlyph pos (zip glyphPoss textures)
  where
    renderGlyph pen (offset, texture) = do
      xOffset <- scale . fromIntegral <$> HB.getGlyphPositionTXOffset offset
      yOffset <- scale . fromIntegral <$> HB.getGlyphPositionTYOffset offset
      xAdvance <- scale . fromIntegral <$> HB.getGlyphPositionTXAdvance offset
      let leftBearing = fromIntegral (glyphLeftBearing texture)
      let topBearing = fromIntegral (glyphTopBearing texture)
      -- putStrLn $ "Rendering glyph " ++ show (glyphIndex texture) ++ " at " ++ show (pen + SDL.V2 xOffset yOffset)
      SDL.copy renderer (glyphTexture texture) Nothing
               (Just (SDL.Rectangle (SDL.P (pen + SDL.V2 xOffset (-yOffset) + SDL.V2 leftBearing (-topBearing)))
                                    (SDL.V2 (fromIntegral (glyphWidth texture)) (fromIntegral (glyphHeight texture)))))
      pure (pen + SDL.V2 xAdvance 0)
    scale x = (x * fromIntegral sizePx) `div` 1000
