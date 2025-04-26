{-# LANGUAGE OverloadedStrings
           , BlockArguments
           , DeriveAnyClass
           , PatternSynonyms
           , TemplateHaskell
           #-}

module Main (main)  where

import Data.FileEmbed (embedFileRelative)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
  ( pattern GL_COLOR_BUFFER_BIT
  , pattern GL_TEXTURE_2D
  , pattern GL_FLOAT
  , pattern GL_FALSE
  , pattern GL_POINTS
  , GLfloat
  , glClear
  , glClearColor
  , glGetUniformLocation
  , glUniform3f
  , glUniform1i
  , glViewport
  , glUniform1f
  , glBindTexture
  , glVertexAttribPointer
  , glEnableVertexAttribArray
  , glDrawArrays
  )
import FreeType
  ( pattern FT_RENDER_MODE_NORMAL
  , FT_Library
  , FT_Face
  , ft_With_FreeType
  , ft_With_Face
  , ft_Set_Pixel_Sizes
  , FT_FaceRec(..)
  , FT_BBox(..)
  , FT_FaceRec(..)
  , FT_UInt
  , ft_Load_Glyph
  , ft_Render_Glyph, gsrBitmap, gsrBitmap_left, gsrBitmap_top, FT_Bitmap (..), FT_Int
  )
import qualified Foreign.Storable as C
import qualified Foreign.C.String as C
import qualified Foreign.Ptr as C
import qualified Data.ByteString as BS
import qualified GI.HarfBuzz.Functions as HB
import qualified GI.HarfBuzz.Structs as HB
import qualified GI.HarfBuzz.Enums as HB
import Control.Exception (Exception, throwIO, assert)
import Control.Monad (when, forM, unless, foldM, (<=<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Function (fix)
import Data.String (IsString)
import Data.Word (Word32)

import GL (withGLFW, withWindow, withProgram, withVAO, bindVAO, withArrayBuffer, bindArrayBuffer, writeArrayBuffer)
import Atlas (withAtlas, Atlas (..), addGlyph)

configFontSizePx :: Num a => a
configFontSizePx = 16

configFontPath :: IsString a => a
configFontPath = "/nix/store/lby2cfffnpqw88mn1caqwc5x8dlwbk6d-source-han-serif-2.003/share/fonts/opentype/source-han-serif/SourceHanSerif.ttc"
-- "/nix/store/82vyvql4j3pbyvrb059y2wf98facdrnh-Iosevka-31.7.1/share/fonts/truetype/Iosevka-ExtendedItalic.ttf"

withFace :: FilePath -> FT_UInt -> (FT_Library -> FT_Face -> IO a) -> IO a
withFace fontPath fontSizePx action = do
  ft_With_FreeType \lib -> do
    ft_With_Face lib fontPath 0 \face -> do
      ft_Set_Pixel_Sizes face 0 fontSizePx
      action lib face

globalBboxSize :: FT_UInt -> FT_Face -> IO (Int, Int)
globalBboxSize fontSizePx face = do
  bbox <- frBbox <$> C.peek face
  let w = bbXMax bbox - bbXMin bbox
      h = bbYMax bbox - bbYMin bbox
  upem <- frUnits_per_EM <$> C.peek face
  pure (scale upem w, scale upem h)
  where
    scale upem x = ceiling (fromIntegral x / fromIntegral upem * fromIntegral fontSizePx :: Double)

main :: IO ()
main = do
  withGLFW . withWindow 800 600 "Candy" $ \win ->
    withProgram vertexShaderSource geometryShaderSource fragmentShaderSource \prog -> do

      penColor <- C.withCAString "pen_color" (glGetUniformLocation prog)
      glUniform3f penColor 0.93 0.94 0.96

      tex <- C.withCAString "atlas" (glGetUniformLocation prog)
      glUniform1i tex 0

      uncurry (onResize prog win) =<< GLFW.getFramebufferSize win
      GLFW.setFramebufferSizeCallback win (Just (onResize prog))

      withFace configFontPath configFontSizePx \_ face -> do
        (cellW, cellH) <- globalBboxSize configFontSizePx face
        withAtlas 1000 cellW cellH \atlas -> do

          texWidth <- C.withCAString "tex_width" (glGetUniformLocation prog)
          glUniform1f texWidth (fromIntegral (atlasWidth atlas))

          texHeight <- C.withCAString "tex_height" (glGetUniformLocation prog)
          glUniform1f texHeight (fromIntegral (atlasHeight atlas))

          glBindTexture GL_TEXTURE_2D (atlasTexture atlas)

          fix \loop -> do
            glClearColor 0.18 0.2 0.25 1
            glClear GL_COLOR_BUFFER_BIT
            array <- genVertexArray configFontSizePx configFontPath face atlas "超级牛力#[编辑器]！ fi"
            withVAO \vao -> bindVAO vao do
              withArrayBuffer \vbo -> do
                bindArrayBuffer vbo do
                  let array = [ 0, 0, 0, 0, 30, 600 ]
                  writeArrayBuffer array
                  print array
                  let
                    floatSize :: Num a => a
                    floatSize = fromIntegral (C.sizeOf (0 :: GLfloat))
                    stride = 6 * floatSize
                  glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE stride C.nullPtr
                  glEnableVertexAttribArray 0
                  glVertexAttribPointer 1 1 GL_FLOAT GL_FALSE stride (C.plusPtr C.nullPtr (2 * floatSize))
                  glEnableVertexAttribArray 1
                  glVertexAttribPointer 2 1 GL_FLOAT GL_FALSE stride (C.plusPtr C.nullPtr (3 * floatSize))
                  glEnableVertexAttribArray 2
                  glVertexAttribPointer 3 2 GL_FLOAT GL_FALSE stride (C.plusPtr C.nullPtr (4 * floatSize))
                  glEnableVertexAttribArray 3
                  glDrawArrays GL_POINTS 0 (fromIntegral (length array `div` 6))
            GLFW.swapBuffers win
            GLFW.waitEvents
            c <- GLFW.windowShouldClose win
            unless c loop
  where
    onResize prog _ w h = do
      glViewport 0 0 (fromIntegral w) (fromIntegral h)
      horiPPU <- C.withCAString "hori_ppu" (glGetUniformLocation prog)
      glUniform1f horiPPU (fromIntegral w / 2)
      vertPPU <- C.withCAString "vert_ppu" (glGetUniformLocation prog)
      glUniform1f vertPPU (fromIntegral h / 2)
    genVertexArray fontSizePx fontPath face atlas text = do
      (glyphInfos, glyphPoss) <- unzip <$> shapeText fontSizePx fontPath text
      glyphIds <- mapM HB.getGlyphInfoTCodepoint glyphInfos
      glyphs <- renderGlyphToAtlas face glyphIds atlas
      concat . reverse . snd <$> foldM renderGlyph (0, []) (zip glyphPoss glyphs)
      where
        renderGlyph (penX, res) (offset, mGlyph) = do
          xOffset <- scale . fromIntegral <$> HB.getGlyphPositionTXOffset offset
          yOffset <- scale . fromIntegral <$> HB.getGlyphPositionTYOffset offset
          xAdvance <- scale . fromIntegral <$> HB.getGlyphPositionTXAdvance offset
          case mGlyph of
            Just glyph ->
              let leftBearing = fromIntegral (gLeftBearing glyph)
                  topBearing = fromIntegral (gTopBearing glyph)
                  x = penX + leftBearing + xOffset
                  y = topBearing - fromIntegral (gHeight glyph) + yOffset
              in pure (penX + xAdvance, [x, y, fromIntegral (gAtlasX glyph), fromIntegral (gAtlasY glyph), fromIntegral (gWidth glyph), fromIntegral (gHeight glyph)] : res)
            Nothing ->
              pure (penX + xAdvance, res)

        scale x = (x * fromIntegral fontSizePx) / 1000

vertexShaderSource :: BS.ByteString
vertexShaderSource = $(embedFileRelative "./app/shader.vert")

geometryShaderSource :: BS.ByteString
geometryShaderSource = $(embedFileRelative "./app/shader.geom")

fragmentShaderSource :: BS.ByteString
fragmentShaderSource = $(embedFileRelative "./app/shader.frag")

{-
      SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 0
      SDL.rendererDrawBlendMode renderer $= SDL.BlendNone
      SDL.clear renderer
      renderText 64 fontPath renderer (SDL.V2 10 400) "Editor with >>= Ox Power!"
      renderText 64 fontPath renderer (SDL.V2 10 200) "超级牛力#[编辑器]！ fi"
      renderText 64 fontPath renderer (SDL.V2 10 100) "Dr.Ross, D.B.Weierstrauss, 1.5.4"
      SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 0
      SDL.drawLine renderer (SDL.P (SDL.V2 0 400)) (SDL.P (SDL.V2 800 400))
      SDL.drawLine renderer (SDL.P (SDL.V2 0 200)) (SDL.P (SDL.V2 800 200))
      SDL.present renderer
      event <- SDL.waitEvent
      case SDL.eventPayload event of
        SDL.KeyboardEvent ke | SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeQ -> pure ()
        _ -> mainLoop renderer
-}

data Glyph = Glyph
  { gAtlasX :: Int
  , gAtlasY :: Int
  , gWidth :: Word32
  , gHeight :: Word32
  , gLeftBearing :: FT_Int
  , gTopBearing :: FT_Int
  }

renderGlyphToAtlas :: Integral a => FT_Face -> [a] -> Atlas -> IO [Maybe Glyph]
renderGlyphToAtlas face glyphIds atlas =
  forM glyphIds \glyphId -> do
    ft_Load_Glyph face (fromIntegral glyphId) 0
    glyphSlot <- frGlyph <$> C.peek face
    ft_Render_Glyph glyphSlot FT_RENDER_MODE_NORMAL
    bitmap <- gsrBitmap <$> C.peek glyphSlot
    leftBearing <- gsrBitmap_left <$> C.peek glyphSlot
    topBearing <- gsrBitmap_top <$> C.peek glyphSlot
    let
      width = bWidth bitmap
      height = let res = bRows bitmap in assert (res == fromIntegral (bPitch bitmap)) res
    if width == 0 || height == 0
      then pure Nothing
      else do
        (x, y, _) <- addGlyph (fromIntegral glyphId) (fromIntegral width) (fromIntegral height) (C.castPtr (bBuffer bitmap)) atlas
        pure (Just (Glyph x y width height leftBearing topBearing))

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
  HB.bufferAddUtf8 buffer utf8Text 0 (fromIntegral (BS.length utf8Text) {-(Text.length text)-})
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
