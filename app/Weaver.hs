{-# LANGUAGE OverloadedStrings
           , BlockArguments
           , DeriveAnyClass
           , DerivingStrategies
           , PatternSynonyms
           , TemplateHaskell
           #-}

module Weaver
  ( Weaver
  , withWeaver
  , setResolution
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
  )
import qualified Foreign.Storable as C
import qualified Foreign.C.String as C
import qualified Foreign.Ptr as C
import qualified Foreign.Marshal.Array as C
import qualified Data.ByteString as BS
import qualified GI.HarfBuzz.Functions as HB
import qualified GI.HarfBuzz.Structs as HB
import qualified GI.HarfBuzz.Enums as HB
import Control.Exception (Exception, throwIO, assert)
import Control.Monad (when, forM, foldM)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word32)

import GL (withProgram, withVAO, bindVAO, withArrayBuffer, bindArrayBuffer, writeArrayBuffer, withTexture2D)
import Atlas (withAtlas, Atlas (..), addGlyph)
import Config (Config(..))

data Weaver = Weaver
  { weaverProgram :: GLuint
  , weaverAtlas :: Atlas
  , weaverFtLib :: FT_Library
  , weaverFtFace :: FT_Face
  , weaverHbFace :: HB.FaceT
  , weaverHbFont :: HB.FontT
  }

withWeaver :: Config -> (Weaver -> IO a) -> IO a
withWeaver config action = do
  withProgram vertexShaderSource geometryShaderSource fragmentShaderSource \weaverProgram -> do
    tex <- C.withCAString "atlas" (glGetUniformLocation weaverProgram)
    glUniform1i tex 0
    withFace (configFontPath config) (configFontSizePx config) \weaverFtLib weaverFtFace -> do
      (cellW, cellH) <- globalBboxSize (configFontSizePx config) weaverFtFace
      withAtlas 100 cellW cellH \weaverAtlas -> do
        texWidth <- C.withCAString "tex_width" (glGetUniformLocation weaverProgram)
        glUniform1f texWidth (fromIntegral (atlasWidth weaverAtlas))
        texHeight <- C.withCAString "tex_height" (glGetUniformLocation weaverProgram)
        glUniform1f texHeight (fromIntegral (atlasHeight weaverAtlas))
        penColor <- C.withCAString "pen_color" (glGetUniformLocation weaverProgram)
        let (penColorR, penColorG, penColorB) = configForeground config
        glUniform3f penColor penColorR penColorG penColorB
        (weaverHbFace, weaverHbFont) <- createHBFontFromPath (configFontSizePx config) (configFontPath config)
        action Weaver
          { weaverProgram
          , weaverAtlas
          , weaverFtLib
          , weaverFtFace
          , weaverHbFace
          , weaverHbFont
          }

setResolution :: Int -> Int -> Weaver -> IO ()
setResolution w h Weaver{weaverProgram = prog} = do
  horiPPU <- C.withCAString "hori_ppu" (glGetUniformLocation prog)
  glUniform1f horiPPU (fromIntegral w / 2)
  vertPPU <- C.withCAString "vert_ppu" (glGetUniformLocation prog)
  glUniform1f vertPPU (fromIntegral h / 2)

drawText :: Weaver -> Text.Text -> IO ()
drawText weaver text = do
  withTexture2D (atlasTexture (weaverAtlas weaver)) do
    array <- genVertexArray
    glGenerateMipmap GL_TEXTURE_2D
    withVAO \vao -> bindVAO vao do
      withArrayBuffer \vbo -> do
        bindArrayBuffer vbo do
          writeArrayBuffer array
          -- let pa [] = pure ()
          --     pa arr = print (take 6 arr) >> pa (drop 6 arr)
          -- pa array
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
      (ppem, _) <- HB.fontGetPpem (weaverHbFont weaver)
      upem <- HB.faceGetUpem (weaverHbFace weaver)
      let scaleFactor = fromIntegral ppem / fromIntegral upem
      (glyphInfos, glyphPoss) <- unzip <$> shapeText weaver text
      glyphIds <- mapM HB.getGlyphInfoTCodepoint glyphInfos
      glyphs <- renderGlyphToAtlas weaver glyphIds
      concat . reverse . snd <$> foldM (renderGlyph scaleFactor) (0, []) (zip glyphPoss glyphs)
      where
        renderGlyph scaleFactor (penX, res) (offset, mGlyph) = do
          xOffset <- scale . fromIntegral <$> HB.getGlyphPositionTXOffset offset
          yOffset <- scale . fromIntegral <$> HB.getGlyphPositionTYOffset offset
          xAdvance <- scale . fromIntegral <$> HB.getGlyphPositionTXAdvance offset
          case mGlyph of
            Just glyph ->
              let leftBearing = fromIntegral (gLeftBearing glyph)
                  topBearing = fromIntegral (gTopBearing glyph)
                  x = penX + leftBearing + xOffset
                  y = topBearing - fromIntegral (gHeight glyph) + yOffset
              in pure (penX + xAdvance, [x, y, fromIntegral (gWidth glyph), fromIntegral (gHeight glyph), fromIntegral (gAtlasX glyph), fromIntegral (gAtlasY glyph)] : res)
            Nothing ->
              pure (penX + xAdvance, res)
          where
            scale = (* scaleFactor)

withFace :: FilePath -> Int -> (FT_Library -> FT_Face -> IO a) -> IO a
withFace fontPath fontSizePx action = do
  ft_With_FreeType \lib -> do
    ft_With_Face lib fontPath 0 \face -> do
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

data Glyph = Glyph
  { gAtlasX :: Int
  , gAtlasY :: Int
  , gWidth :: Word32
  , gHeight :: Word32
  , gLeftBearing :: FT_Int
  , gTopBearing :: FT_Int
  }

renderGlyphToAtlas :: Integral a => Weaver -> [a] -> IO [Maybe Glyph]
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
        pure (Just (Glyph x y eWidth height leftBearing topBearing))
    where
      withReversed len ncol necol arr action = flip C.withArray action . concat . groupNRev ncol necol [] =<< C.peekArray len arr
      groupNRev _ _ acc [] = acc
      groupNRev ncol necol acc xs = groupNRev ncol necol (take necol xs : acc) (drop ncol xs)

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

newtype HBFontCreationError = HBFontCreationError FilePath
  deriving Show
  deriving anyclass Exception

createHBFontFromPath :: Int -> FilePath -> IO (HB.FaceT, HB.FontT)
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
  pure (face, font)

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

shapeText :: Weaver -> Text.Text -> IO [(HB.GlyphInfoT, HB.GlyphPositionT)]
shapeText weaver text = do
  buffer <- createHBBufferFromText text
  HB.shape (weaverHbFont weaver) buffer . Just =<< defaultFeatures
  liftA2 zip (HB.bufferGetGlyphInfos buffer) (HB.bufferGetGlyphPositions buffer)
