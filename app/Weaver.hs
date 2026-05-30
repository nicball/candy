{-# LANGUAGE DeriveAnyClass
           , DerivingStrategies
           , QuasiQuotes
           , TemplateHaskell
           , ForeignFunctionInterface
           #-}

module Weaver
  ( drawTextCached
  , drawTextCachedDefaultFont
  , layoutTextCached
  , layoutTextCachedDefaultFont
  ) where

import Control.Monad (forM, forM_)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Foreign.C qualified as C
import Foreign qualified as C
import GI.Pango qualified as GI
import Graphics.GL
import Language.C.Inline qualified as C
import System.IO.Unsafe (unsafePerformIO)

import Atlas
import Config 
import GL 
import PangoFix.Foreign (pangoCtx)
import Refcount
import TextLayout
import Weaver.Foreign

C.context (C.baseCtx <> pangoCtx <> weaverCtx)
C.include "<hb.h>"
C.include "<hb-raster.h>"
C.include "<pango/pango.h>"
C.include "<string.h>"
C.include "<stdlib.h>"

data Weaver = Weaver
  { program :: GLuint
  , vbo :: GL.Buffer
  , vao :: GL.VertexArray
  , atlas :: Atlas (FontDesc, Int) AtlasGlyph
  }

data AtlasGlyph = AtlasGlyph
  { width :: Int
  , height :: Int
  , xOrigin :: Int
  , yOrigin :: Int
  }

newWeaver :: IO Weaver
newWeaver = do
  program <- newProgram (Just vertexShaderSource) (Just geometryShaderSource) (Just fragmentShaderSource)
  atlas <- newAtlas 1024 1024
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

{-# NOINLINE defaultWeaver #-}
defaultWeaver :: Weaver
defaultWeaver = unsafePerformIO newWeaver

type ColorSpec = [(Int, Int, Color)]

renderLineLayout :: Weaver -> ColorSpec -> LineLayout -> IO Texture
renderLineLayout weaver colorSpec layout = do
  texture <- genObject
  res <- layoutRes layout
  glyphs <- layoutGlyphs layout
  atlasGlyphs <- forM glyphs \glyph -> do
    font <- describeFont glyph.font
    queryGlyph weaver.atlas (font, glyph.gid) >>= \case
      Just item -> pure (glyph, item)
      Nothing -> do
        image <- renderGlyphCached glyph.font glyph.gid
        item <- C.withForeignPtr image.pixels \p ->
          addGlyph weaver.atlas (font, glyph.gid) image.width image.height p
            AtlasGlyph{width=image.width, height=image.height, xOrigin=image.xOrigin, yOrigin=image.yOrigin}
        pure (glyph, item)
  let glyphsByTexture = Map.fromListWith (++) . map (\g -> ((snd g).texture, [g])) $ atlasGlyphs
  bindProgram weaver.program do
    forM_ (Map.toList glyphsByTexture) \(atlasTexture, glyphItems) -> do
      withSlot texture2DSlot atlasTexture do
          glGenerateMipmap GL_TEXTURE_2D
          horiRes <- C.withCAString "hori_res" (glGetUniformLocation weaver.program)
          glUniform1f horiRes (fromIntegral res.w)
          vertRes <- C.withCAString "vert_res" (glGetUniformLocation weaver.program)
          glUniform1f vertRes (fromIntegral res.h)
          renderToTexture res texture do
            withSlot vertexArraySlot weaver.vao do
              withSlot arrayBufferSlot weaver.vbo do
                let array = genVertexArray glyphItems
                writeArrayBuffer array
                withSlot blendFuncSlot (GL_ONE, GL_ONE, GL_ONE, GL_ONE) do
                  glDrawArrays GL_POINTS 0 (fromIntegral (length array `div` 10))
  pure texture
  where
    genVertexArray glyphs = concatMap genGlyph glyphs
    genGlyph (glyph, item) = map fromIntegral
      [ glyph.x + item.userdata.xOrigin
      , glyph.y - item.userdata.yOrigin
      , item.userdata.width
      , item.userdata.height
      , item.x
      , item.y
      ] ++ findColor glyph.cluster colorSpec
    findColor _ [] = error "no color"
    findColor idx ((begin, end, color) : cs)
      | begin <= idx && idx < end = colorToRGBA color
      | otherwise = findColor idx cs

data GlyphImage = GlyphImage
  { width :: Int
  , height :: Int
  , pixels :: C.ForeignPtr ()
  , xOrigin :: Int
  , yOrigin :: Int
  }
  deriving Show

newGlyphRenderer :: IO GlyphRenderer
newGlyphRenderer = GlyphRenderer <$> (C.newForeignPtr p_hb_raster_draw_destroy  =<< [C.exp| hb_raster_draw_t* { hb_raster_draw_create_or_fail() } |])

foreign import ccall "hb-raster.h &hb_raster_draw_destroy"
  p_hb_raster_draw_destroy :: C.FunPtr (C.Ptr GlyphRenderer -> IO ())

{-# NOINLINE glyphCache #-}
glyphCache :: Cache (FontDesc, Int) GlyphImage
glyphCache = unsafePerformIO $ newCache 500

renderGlyphCached :: Font -> Int -> IO GlyphImage
renderGlyphCached font gid = do
  fontDesc <- describeFont font
  let new = do
        rdr <- newGlyphRenderer
        renderGlyph rdr font gid
  deref =<< lookupCache (fontDesc, gid) new (const (pure ())) glyphCache

renderGlyph :: GlyphRenderer -> Font -> Int -> IO GlyphImage
renderGlyph gr font gid =
  GI.withManagedPtr font \font' ->
  C.withForeignPtr gr.ptr \draw ->
  C.alloca \pWidth ->
  C.alloca \pHeight ->
  C.alloca \pXOrigin ->
  C.alloca \pYOrigin ->
  do
  let gid' = fromIntegral gid
  buf <- [C.block| void* {
    hb_codepoint_t gid = $(int gid');
    unsigned int* p_width = $(unsigned int* pWidth);
    unsigned int* p_height = $(unsigned int* pHeight);
    int* p_x_origin = $(int* pXOrigin);
    int* p_y_origin = $(int* pYOrigin);
    if (gid == PANGO_GLYPH_EMPTY) {
      *p_width = *p_height = *p_x_origin = *p_y_origin = 0;
      return 0;
    }
    hb_raster_draw_t* draw = $(hb_raster_draw_t* draw);
    hb_font_t* font = pango_font_get_hb_font($(PangoFont* font'));
    float font_pt = hb_font_get_ptem(font);
    int x_scale, y_scale;
    hb_font_get_scale(font, &x_scale, &y_scale);
    hb_raster_draw_set_scale_factor(draw, x_scale / font_pt, y_scale / font_pt);
    hb_glyph_extents_t glyph_extents;
    hb_font_get_glyph_extents(font, gid, &glyph_extents);
    hb_raster_draw_set_glyph_extents(draw, &glyph_extents);
    hb_raster_draw_glyph(draw, font, gid);
    hb_raster_image_t* image = hb_raster_draw_render(draw);
    hb_raster_extents_t raster_extents;
    hb_raster_image_get_extents(image, &raster_extents);
    *p_width = raster_extents.width;
    *p_height = raster_extents.height;
    *p_x_origin = raster_extents.x_origin;
    *p_y_origin = raster_extents.y_origin;
    size_t count = raster_extents.width * raster_extents.height;
    char* buf = malloc(count);
    const char* data = hb_raster_image_get_buffer(image);
    for (int i = 0; i < raster_extents.height; ++i)
      memcpy(buf + i * raster_extents.width, data + i * raster_extents.stride, raster_extents.width);
    hb_raster_draw_recycle_image(draw, image);
    return buf;
  } |]
  width <- fromIntegral <$> C.peek pWidth
  height <- fromIntegral <$> C.peek pHeight
  xOrigin <- fromIntegral <$> C.peek pXOrigin
  yOrigin <- fromIntegral <$> C.peek pYOrigin
  pixels <- C.newForeignPtr p_free buf
  pure GlyphImage{..}

foreign import ccall "stdlib.h &free"
  p_free :: C.FunPtr (C.Ptr () -> IO ())

{-# NOINLINE textTexCache #-}
textTexCache :: Cache (Text, ColorSpec, LayoutOpt) (Texture, Resolution)
textTexCache = unsafePerformIO $ newCache 2000

drawTextCached :: LayoutOpt -> ColorSpec -> Text -> IO (Refcount (Texture, Resolution))
drawTextCached opts colorSpec text = lookupCache (text, colorSpec, opts) new (deleteObject . fst) textTexCache
  where new = do
          layout <- layoutTextCached opts text
          tex <- renderLineLayout defaultWeaver colorSpec layout
          res <- layoutRes layout
          pure (tex, res)

drawTextCachedDefaultFont :: FontDesc -> ColorSpec -> Text -> IO (Refcount (Texture, Resolution))
drawTextCachedDefaultFont font = drawTextCached defaultLayoutOpt{defaultFont=Just font}

{-# NOINLINE layoutCache #-}
layoutCache :: Cache (Text, LayoutOpt) LineLayout
layoutCache = unsafePerformIO $ newCache 2000

layoutTextCached :: LayoutOpt -> Text -> IO LineLayout
layoutTextCached opts text = deref =<< lookupCache (text, opts) new (const (pure ())) layoutCache
  where
    new = newLineLayout text opts

layoutTextCachedDefaultFont :: FontDesc -> Text -> IO LineLayout
layoutTextCachedDefaultFont font = layoutTextCached defaultLayoutOpt{defaultFont=Just font}

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
      vec3 rgb = font_grey == 0 ? vec3(0, 0, 0) : f_color.rgb;
      color = vec4(rgb, font_grey * f_color.a);
    }
  |]
