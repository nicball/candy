{-# LANGUAGE DeriveAnyClass
           , DerivingStrategies
           , TemplateHaskell
           #-}

module Weaver
  ( Weaver
  , withWeaver
  , getLineHeight
  , getAscender
  , getDescender
  , drawText
  , layoutTextCached
  ) where

import Data.FileEmbed (embedFileRelative)
import Graphics.GL
import FreeType
import qualified Foreign.Storable as C
import qualified Foreign.C.String as C
import qualified Foreign.Ptr as C
import qualified Foreign.Marshal.Array as C
import qualified Data.ByteString as BS
import Control.Monad (forM, foldM)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Cache.LRU.IO as LRU
import Control.Monad (when)

import GL (withProgram, bindProgram, writeArrayBuffer, withSlot, withObject, texture2DSlot, arrayBufferSlot, vertexArraySlot, Resolution(..))
import Atlas (withAtlas, Atlas (..), addGlyph)
import Config (Config(..), FaceID(..))
import qualified Raqm
import qualified GL

data Weaver = Weaver
  { weaverProgram :: GLuint
  , weaverVBO :: GL.Buffer
  , weaverVAO :: GL.VertexArray
  , weaverAtlas :: Atlas RenderedGlyph
  , weaverFace :: FaceID
  , weaverFtFace :: FT_Face
  }

withWeaver :: Config -> (Weaver -> IO a) -> IO a
withWeaver config action = do
  withProgram (Just vertexShaderSource) (Just geometryShaderSource) (Just fragmentShaderSource) \weaverProgram -> bindProgram weaverProgram do
    tex <- C.withCAString "atlas" (glGetUniformLocation weaverProgram)
    glUniform1i tex 0
    let weaverFace = configFace config
    weaverFtFace <- getFaceCached weaverFace
    (cellW, cellH) <- globalBboxSize (faceIDSizePx weaverFace) weaverFtFace
    withAtlas 500 cellW cellH \weaverAtlas -> do
      texWidth <- C.withCAString "tex_width" (glGetUniformLocation weaverProgram)
      glUniform1f texWidth (fromIntegral (atlasWidth weaverAtlas))
      texHeight <- C.withCAString "tex_height" (glGetUniformLocation weaverProgram)
      glUniform1f texHeight (fromIntegral (atlasHeight weaverAtlas))
      penColor <- C.withCAString "pen_color" (glGetUniformLocation weaverProgram)
      let (penColorR, penColorG, penColorB, penColorA) = configForeground config
      glUniform4f penColor penColorR penColorG penColorB penColorA
      withObject \weaverVBO -> do
        withObject \weaverVAO -> do
          withSlot arrayBufferSlot weaverVBO do
            withSlot vertexArraySlot weaverVAO do
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
          action Weaver {..}

setResolution :: Resolution -> Weaver -> IO ()
setResolution (Resolution w h) Weaver{weaverProgram = prog} = do
  horiRes <- C.withCAString "hori_res" (glGetUniformLocation prog)
  glUniform1f horiRes (fromIntegral w)
  vertRes <- C.withCAString "vert_res" (glGetUniformLocation prog)
  glUniform1f vertRes (fromIntegral h)

getLineHeight :: Weaver -> IO Int
getLineHeight Weaver{weaverFtFace = face} = do
  fromIntegral . (`div` 64) . smHeight . srMetrics <$> (C.peek . frSize =<< C.peek face)

getAscender :: Weaver -> IO Int
getAscender Weaver{weaverFtFace = face} = do
  fromIntegral . (`div` 64) . smAscender . srMetrics <$> (C.peek . frSize =<< C.peek face)

getDescender :: Weaver -> IO Int
getDescender Weaver{weaverFtFace = face} = do
  fromIntegral . (`div` 64) . smDescender . srMetrics <$> (C.peek . frSize =<< C.peek face)

drawText :: Weaver -> Resolution -> Int -> Int -> Text.Text -> IO ()
drawText weaver res originX originY text = do
  bindProgram (weaverProgram weaver) do
    setResolution res weaver
    withSlot texture2DSlot (atlasTexture (weaverAtlas weaver)) do
      array <- fmap fromIntegral <$> genVertexArray
      glGenerateMipmap GL_TEXTURE_2D
      withSlot vertexArraySlot (weaverVAO weaver) do
        withSlot arrayBufferSlot (weaverVBO weaver) do
          writeArrayBuffer array
          glDrawArrays GL_POINTS 0 (fromIntegral (length array `div` 6))
  where
    genVertexArray = do
      glyphs <- Raqm.getGlyphs =<< layoutTextCached (weaverFace weaver) text
      renderedGlyphs <- renderGlyphToAtlas weaver (Raqm.gIndex <$> glyphs)
      concat . reverse . snd <$> foldM renderGlyph (0, []) (zip glyphs renderedGlyphs)
      where
        renderGlyph (penX, result) (glyph, RenderedGlyph{..}) =
          let
            xOffset = fromIntegral . (`div` 64) . Raqm.gXOffset $ glyph
            yOffset = fromIntegral . (`div` 64) . Raqm.gYOffset $ glyph
            xAdvance = fromIntegral . Raqm.gXAdvance $ glyph
            x = (penX `div` 64) + gLeftBearing + xOffset + originX
            y = -gTopBearing + gHeight - yOffset + originY
          in
            pure (penX + xAdvance, [x, y, gWidth, gHeight, gAtlasX, gAtlasY] : result)

{-# NOINLINE globalFtLib #-}
globalFtLib :: FT_Library
globalFtLib = unsafePerformIO ft_Init_FreeType

{-# NOINLINE faceCache #-}
faceCache :: LRU.AtomicLRU FaceID FT_Face
faceCache = unsafePerformIO $ LRU.newAtomicLRU (Just 5)

lookupCached :: Ord key => key -> IO value -> ((key, value) -> IO ()) -> LRU.AtomicLRU key value -> IO value
lookupCached key new delete lru = do
  LRU.lookup key lru >>= flip maybe pure do
    value <- new
    LRU.maxSize lru >>= maybe (pure ()) \limit -> do
      s <- LRU.size lru
      when (s == fromIntegral limit) do
        LRU.pop lru >>= maybe (pure ()) delete
    LRU.insert key value lru
    pure value

getFaceCached :: FaceID -> IO FT_Face
getFaceCached faceID = lookupCached faceID new (ft_Done_Face . snd) faceCache
  where
    new = do
      face <- ft_New_Face globalFtLib (faceIDPath faceID) (fromIntegral (faceIDIndex faceID))
      ft_Set_Pixel_Sizes face 0 (fromIntegral (faceIDSizePx faceID))
      pure face

{-# NOINLINE raqmCache #-}
raqmCache :: LRU.AtomicLRU (FaceID, Text.Text) Raqm.Raqm
raqmCache = unsafePerformIO $ LRU.newAtomicLRU (Just 500)

layoutTextCached :: FaceID -> Text.Text -> IO Raqm.Raqm
layoutTextCached faceID text = lookupCached (faceID, text) new (Raqm.destroy . snd) raqmCache
  where
    new = do
      rq <- Raqm.create
      Raqm.setText rq text
      Raqm.setLanguage rq "en" 0 (Text.lengthWord8 text)
      face <- getFaceCached faceID
      Raqm.setFreetypeFace rq face
      Raqm.layout rq
      pure rq

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
  , gWidth :: Int
  , gHeight :: Int
  , gLeftBearing :: Int
  , gTopBearing :: Int
  }
  deriving Show

renderGlyphToAtlas :: Weaver -> [Int] -> IO [RenderedGlyph]
renderGlyphToAtlas weaver glyphIds =
  forM glyphIds \glyphId ->
    fst <$> addGlyph glyphId (render glyphId) (\rg -> (gAtlasX rg, gAtlasY rg)) (weaverAtlas weaver)
  where
    render glyphId = do
      ft_Load_Glyph (weaverFtFace weaver) (fromIntegral glyphId) 0
      glyphSlot <- frGlyph <$> C.peek (weaverFtFace weaver)
      ft_Render_Glyph glyphSlot FT_RENDER_MODE_NORMAL
      bitmap <- gsrBitmap <$> C.peek glyphSlot
      leftBearing <- fromIntegral . gsrBitmap_left <$> C.peek glyphSlot
      topBearing <- fromIntegral . gsrBitmap_top <$> C.peek glyphSlot
      let
        width = fromIntegral . bPitch $ bitmap
        height = fromIntegral . bRows $ bitmap
        eWidth = fromIntegral . bWidth $ bitmap
      pure
        ( eWidth
        , height
        , withReversed (width * height) width eWidth (bBuffer bitmap)
        , \x y -> RenderedGlyph
          { gAtlasX = x
          , gAtlasY = y
          , gWidth = eWidth
          , gHeight = height
          , gLeftBearing = leftBearing
          , gTopBearing = topBearing
          }
        )
    withReversed len ncol necol arr action = flip C.withArray (action . C.castPtr) . concat . groupNRev ncol necol [] =<< C.peekArray len arr
    groupNRev _ _ acc [] = acc
    groupNRev ncol necol acc xs = groupNRev ncol necol (take necol xs : acc) (drop ncol xs)
