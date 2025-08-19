{-# LANGUAGE OverloadedStrings
           , BlockArguments
           , DeriveAnyClass
           , DerivingStrategies
           , PatternSynonyms
           , TemplateHaskell
           , RecordWildCards
           #-}

module Weaver
  ( Weaver
  , withWeaver
  , setResolution
  , getLineHeight
  , drawText
  ) where

import Data.FileEmbed (embedFileRelative)
import Graphics.GL
  ( pattern GL_TEXTURE_2D
  , pattern GL_FLOAT
  , pattern GL_FALSE
  , pattern GL_POINTS
  , GLfloat
  , GLuint
  , glGenerateMipmap
  , glGetUniformLocation
  , glUniform1f
  , glUniform3f
  , glUniform1i
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
  , FT_Int
  , ft_Load_Glyph
  , ft_Render_Glyph
  , FT_Bitmap(..)
  , FT_GlyphSlotRec(..)
  , FT_Size_Metrics(smHeight)
  , FT_SizeRec(srMetrics)
  )
import qualified Foreign.Storable as C
import qualified Foreign.C.String as C
import qualified Foreign.Ptr as C
import qualified Foreign.Marshal.Array as C
import qualified Data.ByteString as BS
import Control.Monad (forM, foldM)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import Data.Word (Word32)

import GL (withProgram, withVAO, bindVAO, withArrayBuffer, bindArrayBuffer, writeArrayBuffer, withTexture2D)
import Atlas (withAtlas, Atlas (..), addGlyph)
import Config (Config(..))
import qualified Raqm

data Weaver = Weaver
  { weaverProgram :: GLuint
  , weaverAtlas :: Atlas
  , weaverFtLib :: FT_Library
  , weaverFtFace :: FT_Face
  , weaverRaqm :: Raqm.Raqm
  }

withWeaver :: Config -> (Weaver -> IO a) -> IO a
withWeaver config action = do
  withProgram vertexShaderSource geometryShaderSource fragmentShaderSource \weaverProgram -> do
    tex <- C.withCAString "atlas" (glGetUniformLocation weaverProgram)
    glUniform1i tex 0
    withFace (configFontPath config) (configFontIndex config) (configFontSizePx config) \weaverFtLib weaverFtFace -> do
      (cellW, cellH) <- globalBboxSize (configFontSizePx config) weaverFtFace
      -- printFace weaverFtFace
      withAtlas 100 cellW cellH \weaverAtlas -> do
        texWidth <- C.withCAString "tex_width" (glGetUniformLocation weaverProgram)
        glUniform1f texWidth (fromIntegral (atlasWidth weaverAtlas))
        texHeight <- C.withCAString "tex_height" (glGetUniformLocation weaverProgram)
        glUniform1f texHeight (fromIntegral (atlasHeight weaverAtlas))
        penColor <- C.withCAString "pen_color" (glGetUniformLocation weaverProgram)
        let (penColorR, penColorG, penColorB) = configForeground config
        glUniform3f penColor penColorR penColorG penColorB
        Raqm.withRaqm \weaverRaqm ->
          action Weaver
            { weaverProgram
            , weaverAtlas
            , weaverFtLib
            , weaverFtFace
            , weaverRaqm
            }
  -- where
  --   printFace face = do
  --     rcd <- C.peek face
  --     print $ "num faces " <> show (frNum_faces rcd)
  --     print . ("family name " <>) . show =<< (C.peekCString . C.castPtr . frFamily_name $ rcd)
  --     print . ("family name " <>) . show =<< (C.peekCString . C.castPtr . frStyle_name $ rcd)
  --     let FT_BBox{..} = frBbox rcd
  --     print $ "xMin " <> show bbXMin
  --     print $ "xMax " <> show bbXMax
  --     print $ "yMin " <> show bbYMin
  --     print $ "yMax " <> show bbYMax
  --     print $ "units per EM " <> show (frUnits_per_EM rcd)
  --     print $ "ascender " <> show (frAscender rcd)
  --     print $ "descender " <> show (frDescender rcd)
  --     print $ "height " <> show (frHeight rcd)
  --     print $ "max advance width " <> show (frMax_advance_width rcd)
  --     print $ "underline position " <> show (frUnderline_position rcd)

setResolution :: Int -> Int -> Weaver -> IO ()
setResolution w h Weaver{weaverProgram = prog} = do
  horiPPU <- C.withCAString "hori_ppu" (glGetUniformLocation prog)
  glUniform1f horiPPU (fromIntegral w / 2)
  vertPPU <- C.withCAString "vert_ppu" (glGetUniformLocation prog)
  glUniform1f vertPPU (fromIntegral h / 2)

getLineHeight :: Weaver -> IO Int
getLineHeight Weaver{weaverFtFace = face} = do
  fromIntegral . (`div` 64) . smHeight . srMetrics <$> (C.peek . frSize =<< C.peek face)

drawText :: Weaver -> Int -> Int -> Text.Text -> IO ()
drawText weaver originX originY text = do
  withTexture2D (atlasTexture (weaverAtlas weaver)) do
    array <- genVertexArray
    glGenerateMipmap GL_TEXTURE_2D
    withVAO \vao -> bindVAO vao do
      withArrayBuffer \vbo -> do
        bindArrayBuffer vbo do
          writeArrayBuffer array
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
  where
    genVertexArray = do
      glyphs <- shapeText weaver text
      renderedGlyphs <- renderGlyphToAtlas weaver (Raqm.gIndex <$> glyphs)
      concat . reverse . snd <$> foldM renderGlyph (0, []) (zip glyphs renderedGlyphs)
      where
        renderGlyph (penX, res) (glyph, mRenderedGlyph) = do
          let xOffset = fromIntegral . (`div` 64) . Raqm.gXOffset $ glyph
          let yOffset = fromIntegral . (`div` 64) . Raqm.gYOffset $ glyph
          let xAdvance = fromIntegral . (`div` 64) . Raqm.gXAdvance $ glyph
          case mRenderedGlyph of
            Just rg->
              let leftBearing = fromIntegral (gLeftBearing rg)
                  topBearing = fromIntegral (gTopBearing rg)
                  x = penX + leftBearing + xOffset + fromIntegral originX
                  y = topBearing - fromIntegral (gHeight rg) + yOffset + fromIntegral originY
              in pure (penX + xAdvance, [x, y, fromIntegral (gWidth rg), fromIntegral (gHeight rg), fromIntegral (gAtlasX rg), fromIntegral (gAtlasY rg)] : res)
            Nothing ->
              pure (penX + xAdvance, res)

withFace :: FilePath -> Int -> Int -> (FT_Library -> FT_Face -> IO a) -> IO a
withFace fontPath fontIndex fontSizePx action = do
  ft_With_FreeType \lib -> do
    ft_With_Face lib fontPath (fromIntegral fontIndex) \face -> do
      ft_Set_Pixel_Sizes face 0 (fromIntegral fontSizePx)
      action lib face

globalBboxSize :: Int -> FT_Face -> IO (Int, Int)
globalBboxSize fontSizePx face = do
  bbox <- frBbox <$> C.peek face
  let w = bbXMax bbox - bbXMin bbox
      h = bbYMax bbox - bbYMin bbox
  upem <- frUnits_per_EM <$> C.peek face
  pure (scale upem w, scale upem h)
  where
    scale upem x = ceiling (fromIntegral x / fromIntegral upem * fromIntegral fontSizePx :: Double)

vertexShaderSource :: BS.ByteString
vertexShaderSource = $(embedFileRelative "./app/weaver.vert")

geometryShaderSource :: BS.ByteString
geometryShaderSource = $(embedFileRelative "./app/weaver.geom")

fragmentShaderSource :: BS.ByteString
fragmentShaderSource = $(embedFileRelative "./app/weaver.frag")

data RenderedGlyph = RenderedGlyph
  { gAtlasX :: Int
  , gAtlasY :: Int
  , gWidth :: Word32
  , gHeight :: Word32
  , gLeftBearing :: FT_Int
  , gTopBearing :: FT_Int
  }

renderGlyphToAtlas :: Integral a => Weaver -> [a] -> IO [Maybe RenderedGlyph]
renderGlyphToAtlas weaver glyphIds =
  forM glyphIds \glyphId -> do
    ft_Load_Glyph (weaverFtFace weaver) (fromIntegral glyphId) 0
    glyphSlot <- frGlyph <$> C.peek (weaverFtFace weaver)
    ft_Render_Glyph glyphSlot FT_RENDER_MODE_NORMAL
    bitmap <- gsrBitmap <$> C.peek glyphSlot
    leftBearing <- gsrBitmap_left <$> C.peek glyphSlot
    topBearing <- gsrBitmap_top <$> C.peek glyphSlot
    let
      width = bPitch bitmap
      height = bRows bitmap
      eWidth = bWidth bitmap
    if width == 0 || height == 0
      then pure Nothing
      else do
        (x, y, _) <- do
          withReversed (fromIntegral width * fromIntegral height) (fromIntegral width) (fromIntegral eWidth) (bBuffer bitmap) \buf ->
            addGlyph (fromIntegral glyphId) (fromIntegral eWidth) (fromIntegral height) (C.castPtr buf) (weaverAtlas weaver)
        pure (Just (RenderedGlyph x y eWidth height leftBearing topBearing))
    where
      withReversed len ncol necol arr action = flip C.withArray action . concat . groupNRev ncol necol [] =<< C.peekArray len arr
      groupNRev _ _ acc [] = acc
      groupNRev ncol necol acc xs = groupNRev ncol necol (take necol xs : acc) (drop ncol xs)

shapeText :: Weaver -> Text.Text -> IO [Raqm.Glyph]
shapeText weaver text = do
  let rq = weaverRaqm weaver
  Raqm.clearContents rq
  Raqm.setText rq text
  Raqm.setLanguage rq "en" 0 (Text.lengthWord8 text)
  Raqm.setFreetypeFace rq (weaverFtFace weaver)
  Raqm.layout rq
  Raqm.getGlyphs rq
