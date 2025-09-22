{-# LANGUAGE DeriveAnyClass
           , DerivingStrategies
           , QuasiQuotes
           #-}
module Weaver
  ( Weaver
  , newWeaver
  , deleteWeaver
  , withWeaver
  , getLineHeight
  , getAscender
  , getDescender
  , drawText
  , drawTextCached
  , layoutTextCached
  , getFaceCached
  ) where

import Control.Exception (assert, bracket)
import Control.Monad (forM, foldM, when)
import Data.ByteString qualified as BS
import Data.Cache.LRU.IO qualified as LRU
import Data.String.Interpolate (__i)
import Data.Text.Foreign qualified as Text
import Data.Text qualified as Text
import Data.Text (Text)
import Foreign.C.String qualified as C
import Foreign.Marshal.Array qualified as C
import Foreign.Ptr qualified as C
import Foreign.Storable qualified as C
import FreeType
import Graphics.GL
import System.IO.Unsafe (unsafePerformIO)

import GL 
import Atlas 
import Config 
import Raqm qualified

data Weaver = Weaver
  { program :: GLuint
  , vbo :: GL.Buffer
  , vao :: GL.VertexArray
  , atlas :: Atlas RenderedGlyph
  , face :: FaceID
  , ftFace :: FT_Face
  }

newWeaver :: FaceID -> IO Weaver
newWeaver face = do
  program <- newProgram (Just vertexShaderSource) (Just geometryShaderSource) (Just fragmentShaderSource)
  ftFace <- getFaceCached face
  (cellW, cellH) <- globalBboxSize face.sizePx ftFace
  atlas <- newAtlas 500 cellW cellH
  bindProgram program do
    tex <- C.withCAString "atlas" (glGetUniformLocation program)
    glUniform1i tex 0
    texWidth <- C.withCAString "tex_width" (glGetUniformLocation program)
    glUniform1f texWidth (fromIntegral atlas.width)
    texHeight <- C.withCAString "tex_height" (glGetUniformLocation program)
    glUniform1f texHeight (fromIntegral atlas.height)
  vbo <- genObject
  vao <- genObject
  withSlot arrayBufferSlot vbo do
    withSlot vertexArraySlot vao do
      let
        floatSize :: Num a => a
        floatSize = fromIntegral (C.sizeOf (0 :: GLfloat))
        stride = 10 * floatSize
      glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE stride C.nullPtr
      glEnableVertexAttribArray 0
      glVertexAttribPointer 1 1 GL_FLOAT GL_FALSE stride (C.plusPtr C.nullPtr (2 * floatSize))
      glEnableVertexAttribArray 1
      glVertexAttribPointer 2 1 GL_FLOAT GL_FALSE stride (C.plusPtr C.nullPtr (3 * floatSize))
      glEnableVertexAttribArray 2
      glVertexAttribPointer 3 2 GL_FLOAT GL_FALSE stride (C.plusPtr C.nullPtr (4 * floatSize))
      glEnableVertexAttribArray 3
      glVertexAttribPointer 4 4 GL_FLOAT GL_FALSE stride (C.plusPtr C.nullPtr (6 * floatSize))
      glEnableVertexAttribArray 4
  pure Weaver{..}

deleteWeaver :: Weaver -> IO ()
deleteWeaver weaver = do
  deleteProgram weaver.program
  deleteObject weaver.vbo
  deleteObject weaver.vao
  deleteAtlas weaver.atlas

withWeaver :: FaceID -> (Weaver -> IO a) -> IO a
withWeaver face = bracket (newWeaver face) deleteWeaver

getLineHeight :: FT_Face -> IO Int
getLineHeight ftFace = do
  fromIntegral . (`div` 64) . smHeight . srMetrics <$> (C.peek . frSize =<< C.peek ftFace)

getAscender :: FT_Face -> IO Int
getAscender ftFace = do
  fromIntegral . (`div` 64) . smAscender . srMetrics <$> (C.peek . frSize =<< C.peek ftFace)

getDescender :: FT_Face -> IO Int
getDescender ftFace = do
  fromIntegral . (`div` 64) . smDescender . srMetrics <$> (C.peek . frSize =<< C.peek ftFace)

type ColorSpec = [(Int, Int, Color)]

drawText :: Weaver -> Texture -> ColorSpec -> Text -> IO Resolution
drawText weaver texture colorspec text = assert (not . Text.null $ text) do
  bindProgram weaver.program do
    withSlot texture2DSlot weaver.atlas.texture do
      ftFace <- getFaceCached weaver.face
      lineHeight <- getLineHeight ftFace
      descender <- getDescender ftFace
      (textWidth, array) <- genVertexArray 0 (lineHeight + descender - 1)
      glGenerateMipmap GL_TEXTURE_2D
      let res = Resolution textWidth lineHeight
      horiRes <- C.withCAString "hori_res" (glGetUniformLocation weaver.program)
      glUniform1f horiRes (fromIntegral res.w)
      vertRes <- C.withCAString "vert_res" (glGetUniformLocation weaver.program)
      glUniform1f vertRes (fromIntegral res.h)
      renderToTexture res texture do
        withSlot vertexArraySlot weaver.vao do
          withSlot arrayBufferSlot weaver.vbo do
            writeArrayBuffer array
            b <- glIsEnabled GL_BLEND
            glDisable GL_BLEND
            glDrawArrays GL_POINTS 0 (fromIntegral (length array `div` 10))
            when (b /= 0) (glEnable GL_BLEND)
      pure res
  where
    genVertexArray originX originY = do
      glyphs <- Raqm.getGlyphs =<< layoutTextCached weaver.face text
      renderedGlyphs <- renderGlyphToAtlas weaver (fmap (.index) glyphs)
      (_, maxX, vertices) <- foldM renderGlyph (0, 0, []) (zip glyphs renderedGlyphs)
      pure (maxX, concat . reverse $ vertices)
      where
        renderGlyph (penX, _, result) (glyph, rg) =
          let
            xOffset = fromIntegral . (`div` 64) $ glyph.xOffset
            yOffset = fromIntegral . (`div` 64) $ glyph.yOffset
            xAdvance = fromIntegral glyph.xAdvance
            x = (penX `div` 64) + rg.leftBearing + xOffset + originX
            y = -rg.topBearing + rg.height - yOffset + originY
            cluster = fromIntegral glyph.cluster
            vertices = fmap fromIntegral [x, y, rg.width, rg.height, rg.atlasX, rg.atlasY] ++ findColor cluster colorspec
          in
            pure (penX + xAdvance, x + rg.width, vertices : result)
    findColor _ [] = undefined
    findColor idx ((begin, end, color) : cs)
      | begin <= idx && idx < end = colorToRGBA color
      | otherwise = findColor idx cs

{-# NOINLINE textTexCache #-}
textTexCache :: LRU.AtomicLRU (FaceID, ColorSpec, Text) (Texture, Resolution)
textTexCache = unsafePerformIO $ LRU.newAtomicLRU (Just 500)

drawTextCached :: Weaver -> ColorSpec -> Text -> IO (Texture, Resolution)
drawTextCached weaver colorspec text = do
  lookupCached (weaver.face, colorspec, text) new (deleteObject . fst . snd) textTexCache
  where
    new = do
      tex <- genObject
      res <- drawText weaver tex colorspec text
      pure (tex, res)

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
      face <- ft_New_Face globalFtLib faceID.path (fromIntegral faceID.index)
      ft_Set_Pixel_Sizes face 0 (fromIntegral faceID.sizePx)
      pure face

{-# NOINLINE raqmCache #-}
raqmCache :: LRU.AtomicLRU (FaceID, Text) Raqm.Raqm
raqmCache = unsafePerformIO $ LRU.newAtomicLRU (Just 500)

layoutTextCached :: FaceID -> Text -> IO Raqm.Raqm
layoutTextCached faceID text = lookupCached (faceID, text) new (Raqm.destroy . snd) raqmCache
  where
    new = do
      rq <- Raqm.create
      Raqm.setText rq text
      Raqm.setLanguage rq "en" 0 (Text.lengthWord8 text)
      face <- getFaceCached faceID
      Raqm.setFreetypeFace rq face
      -- Raqm.addFontFeature rq "dlig"
      Raqm.layout rq
      pure rq

globalBboxSize :: Int -> FT_Face -> IO (Int, Int)
globalBboxSize fontSizePx facePtr = do
  face <- C.peek facePtr
  let
    FT_BBox{..} = face.frBbox
    w = bbXMax - bbXMin
    h = bbYMax - bbYMin
    scale x = ceiling (fromIntegral x / fromIntegral face.frUnits_per_EM * fromIntegral fontSizePx :: Double)
  pure (scale w, scale h)

vertexShaderSource :: BS.ByteString
vertexShaderSource =
  [__i|
    \#version 330 core

    layout (location = 0) in vec2 v_pos;
    layout (location = 1) in float v_width;
    layout (location = 2) in float v_height;
    layout (location = 3) in vec2 v_tex_coord;
    layout (location = 4) in vec4 v_color;

    out vec2 g_tex_coord;
    out float g_width;
    out float g_height;
    out vec4 g_color;

    uniform float hori_res;
    uniform float vert_res;

    void main() {
      gl_Position = vec4(v_pos.x * 2 / hori_res - 1, -(v_pos.y + 1) * 2 / vert_res + 1, 0, 1);
      g_tex_coord = v_tex_coord;
      g_width = v_width;
      g_height = v_height;
      g_color = v_color;
    }
  |]

geometryShaderSource :: BS.ByteString
geometryShaderSource =
  [__i|
    \#version 330 core

    layout (points) in;
    layout (triangle_strip, max_vertices = 4) out;

    in vec2 g_tex_coord[];
    in float g_width[];
    in float g_height[];
    in vec4 g_color[];

    uniform float hori_res;
    uniform float vert_res;
    uniform float tex_width;
    uniform float tex_height;

    out vec2 f_tex_coord;
    out vec4 f_color;

    void main() {
      float vw = g_width[0] * 2 / hori_res;
      float vh = g_height[0] * 2 / vert_res;
      float tw = g_width[0] / tex_width;
      float th = g_height[0] / tex_height;
      vec4 pos = gl_in[0].gl_Position;
      vec2 tpos = g_tex_coord[0].xy / vec2(tex_width, tex_height);

      gl_Position = pos;
      f_tex_coord = tpos;
      f_color = g_color[0];
      EmitVertex();

      gl_Position = pos + vec4(vw, 0, 0, 0);
      f_tex_coord = tpos + vec2(tw, 0);
      f_color = g_color[0];
      EmitVertex();

      gl_Position = pos + vec4(0, vh, 0, 0);
      f_tex_coord = tpos + vec2(0, th);
      f_color = g_color[0];
      EmitVertex();

      gl_Position = pos + vec4(vw, vh, 0, 0);
      f_tex_coord = tpos + vec2(tw, th);
      f_color = g_color[0];
      EmitVertex();

      EndPrimitive();
    }
  |]

fragmentShaderSource :: BS.ByteString
fragmentShaderSource =
  [__i|
    \#version 330 core

    in vec2 f_tex_coord;
    in vec4 f_color;

    uniform sampler2D atlas;

    out vec4 color;

    void main() {
      float font_grey = texture(atlas, f_tex_coord).r;
      color = vec4(f_color.rgb, font_grey * f_color.a);
    }
  |]

data RenderedGlyph = RenderedGlyph
  { atlasX :: Int
  , atlasY :: Int
  , width :: Int
  , height :: Int
  , leftBearing :: Int
  , topBearing :: Int
  }
  deriving Show

renderGlyphToAtlas :: Weaver -> [Int] -> IO [RenderedGlyph]
renderGlyphToAtlas weaver glyphIds =
  forM glyphIds \glyphId ->
    fst <$> addGlyph glyphId (render glyphId) (\rg -> (rg.atlasX, rg.atlasY)) weaver.atlas
  where
    render glyphId = do
      ft_Load_Glyph weaver.ftFace (fromIntegral glyphId) 0
      glyphSlotPtr <- frGlyph <$> C.peek weaver.ftFace
      ft_Render_Glyph glyphSlotPtr FT_RENDER_MODE_NORMAL
      glyphSlot <- C.peek glyphSlotPtr
      let
        bitmap = glyphSlot.gsrBitmap
        leftBearing = fromIntegral glyphSlot.gsrBitmap_left
        topBearing = fromIntegral glyphSlot.gsrBitmap_top
        width = fromIntegral bitmap.bPitch
        height = fromIntegral bitmap.bRows
        eWidth = fromIntegral bitmap.bWidth
      pure
        ( eWidth
        , height
        , withReversed (width * height) width eWidth (bBuffer bitmap)
        , \x y -> RenderedGlyph
          { atlasX = x
          , atlasY = y
          , width = eWidth
          , height
          , leftBearing
          , topBearing
          }
        )
    withReversed len ncol necol arr action = flip C.withArray (action . C.castPtr) . concat . groupNRev ncol necol [] =<< C.peekArray len arr
    groupNRev _ _ acc [] = acc
    groupNRev ncol necol acc xs = groupNRev ncol necol (take necol xs : acc) (drop ncol xs)
