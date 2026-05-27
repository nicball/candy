{-# LANGUAGE TemplateHaskell, QuasiQuotes, ForeignFunctionInterface #-}

module TextLayout
  ( newLineLayout
  , LayoutOpt(..)
  , LineLayout
  , layoutGlyphs
  , renderLineLayout
  , layoutRes
  , indexToQuad
  , positionToIndex
  ) where

import Config
import Control.Monad (forM_, when)
import Data.Function (fix)
import Data.GI.Base qualified as GI
import Data.IORef
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Data.Text (Text)
import Foreign qualified as C
import GI.Pango qualified as Pango
import GL
import Graphics.GL
import Language.C.Inline qualified as C
import PangoFix.Foreign
import PangoFix qualified as Pango
import TextLayout.Foreign
import System.IO.Unsafe (unsafePerformIO)
import Refcount

C.context (C.baseCtx <> pangoCtx <> textLayoutCtx)
C.include "hb.h"
C.include "hb-raster.h"
C.include "<pango/pango.h>"
C.include "<pango/pangoft2.h>"
C.include "<string.h>"
C.include "<stdlib.h>"

type LayoutContext = Pango.Context

{-# NOINLINE defaultLayoutContext #-}
defaultLayoutContext :: LayoutContext
defaultLayoutContext = unsafePerformIO $ GI.newObject Pango.Context =<< [C.block| PangoContext* {
    PangoFontMap* font_map = pango_ft2_font_map_new();
    return pango_font_map_create_context(font_map);
  } |]

type LineLayout = Pango.Layout
data LayoutOpt = LayoutOpt
  { width :: Maybe Int
  , ellipsize :: Bool
  , fontSpec :: FontSpec
  }
type FontSpec = [(Int, Int, FontDesc)]
data GlyphInfo = GlyphInfo
  { gid :: Int
  , font :: Font
  , cluster :: Int
  , x :: Int
  , y :: Int
  , advance :: Int
  }
  deriving Show
type Font = Pango.Font

newLineLayout :: Text -> LayoutOpt -> IO LineLayout
newLineLayout text opts = do
  layout <- Pango.layoutNew defaultLayoutContext
  if opts.ellipsize
    then Pango.layoutSetEllipsize layout Pango.EllipsizeModeEnd
    else Pango.layoutSetEllipsize layout Pango.EllipsizeModeNone
  Pango.layoutSetWidth layout (maybe (-1) ((* 1024) . fromIntegral) opts.width)
  Pango.layoutSetText layout text (-1)
  attrs <- Pango.attrListNew
  forM_ opts.fontSpec \(start, end, descStr) -> do
    desc <- Pango.fontDescriptionFromString descStr
    attr <- Pango.attrFontDescNew desc
    Pango.setAttributeStartIndex attr (fromIntegral start)
    Pango.setAttributeEndIndex attr (fromIntegral end)
    Pango.attrListInsert attrs attr
  Pango.layoutSetAttributes layout (Just attrs)
  pure layout

layoutRes :: LineLayout -> IO Resolution
layoutRes layout = do
  (w, h) <- Pango.layoutGetPixelSize layout
  pure $ Resolution (fromIntegral w) (fromIntegral h)

positionToIndex :: LineLayout -> Int -> Int -> IO Int
positionToIndex layout x y = do
  (_, index, _) <- Pango.layoutXyToIndex layout (fromIntegral x * 1024) (fromIntegral y * 1024)
  pure $ fromIntegral index

indexToQuad :: LineLayout -> Int -> IO Quad
indexToQuad layout index = do
  (p1, _) <- Pango.layoutGetCursorPos layout (fromIntegral index)
  (p2, _) <- Pango.layoutGetCursorPos layout (fromIntegral index + 1)
  lineHeight <- scale <$> Pango.getRectangleHeight p1
  top <- scale <$> Pango.getRectangleY p1
  x1 <- scale <$> Pango.getRectangleX p1
  x2 <- scale <$> Pango.getRectangleX p2
  pure $ quadFromYXRange top (top + lineHeight - 1) (min x1 x2) (max x1 x2)
  where
    scale x = fromIntegral (x `div` 1024)

layoutGlyphs :: LineLayout -> IO [GlyphInfo]
layoutGlyphs layout = do
  iter <- Pango.layoutGetIter layout
  glyphs <- newIORef []
  fix \loop -> do
    baseline <- scale <$> Pango.layoutIterGetBaseline iter
    line <- fromJust <$> Pango.layoutIterGetLineReadonly iter
    (_, lineExts) <- Pango.layoutLineGetPixelExtents line
    lineX <- fromIntegral <$> Pango.getRectangleX lineExts
    runX <- newIORef lineX
    runs <- Pango.getLayoutLineRuns line
    forM_ runs \run -> do
      glyphStrX <- (+) <$> readIORef runX <*> (scale <$> Pango.getGlyphItemStartXOffset run)
      runBaseLine <- (baseline -) . scale <$> Pango.getGlyphItemYOffset run
      glyphStr <- fromJust <$> Pango.getGlyphItemGlyphs run
      font <- fromJust <$> (Pango.getAnalysisFont =<< Pango.getItemAnalysis . fromJust =<< Pango.getGlyphItemItem run)
      glyphInfos <- Pango.getGlyphStringGlyphs glyphStr
      glyphClusters <- Pango.getGlyphStringLogClusters' glyphStr
      glyphX <- newIORef glyphStrX
      forM_ (zip glyphInfos glyphClusters) \(glyphInfo, cluster) -> do
        gid <- fromIntegral <$> Pango.getGlyphInfoGlyph glyphInfo
        geo <- Pango.getGlyphInfoGeometry glyphInfo
        advance <- scale <$> Pango.getGlyphGeometryWidth geo
        x <- (+) <$> readIORef glyphX <*> (scale <$> Pango.getGlyphGeometryXOffset geo)
        y <- (runBaseLine +) . scale <$> Pango.getGlyphGeometryYOffset geo
        modifyIORef glyphX (+ advance)
        modifyIORef glyphs (GlyphInfo{..} :)
      runEndX <- (+) <$> readIORef glyphX <*> (scale <$> Pango.getGlyphItemEndXOffset run)
      writeIORef runX runEndX
    moved <- Pango.layoutIterNextLine iter
    when moved loop
  reverse <$> readIORef glyphs
  where
    scale x = fromIntegral (div x 1024)

type ColorSpec = [(Int, Int, Color)]

renderLineLayout :: LineLayout -> ColorSpec -> IO Texture
renderLineLayout layout colorSpec = do
  texture <- genObject
  Resolution width height <- layoutRes layout
  glyphs <- layoutGlyphs layout
  let bufSize = width * height * 4
  buf <- C.mallocForeignPtrBytes bufSize
  C.withForeignPtr buf \pBuf -> do
    C.fillBytes pBuf 0 bufSize
    forM_ glyphs \gi -> do
      let color = findColor gi.cluster colorSpec
      image <- renderGlyphCached gi.font gi.gid
      let left = gi.x + image.xOrigin
      let bottom = height - (gi.y - image.yOrigin)
      forM_ [0 .. image.height - 1] \row -> do
        let dst = ((bottom + row) * width + left) * 4
        let src = row * image.width
        C.withForeignPtr image.pixels \pixels -> do
          forM_ [0 .. image.width - 1] \col -> do
            C.poke (pBuf `C.plusPtr` (dst + col * 4 + 0)) (cvt color.red)
            C.poke (pBuf `C.plusPtr` (dst + col * 4 + 1)) (cvt color.green)
            C.poke (pBuf `C.plusPtr` (dst + col * 4 + 2)) (cvt color.blue)
            grey <- C.peek (pixels `C.plusPtr` (src + col))
            C.poke (pBuf `C.plusPtr` (dst + col * 4 + 3)) (grey :: C.CChar)
    withSlot texture2DSlot texture do
      glPixelStorei GL_UNPACK_ALIGNMENT 1
      glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (fromIntegral width) (fromIntegral height) 0 GL_RGBA GL_UNSIGNED_BYTE pBuf
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
      glGenerateMipmap GL_TEXTURE_2D
  pure texture
  where
    findColor _ [] = error "no color"
    findColor idx ((begin, end, color) : cs)
      | begin <= idx && idx <= end = color
      | otherwise = findColor idx cs
    cvt :: Float -> C.CChar
    cvt f = truncate $ f * 255

loadFont :: FontDesc -> IO (Maybe Font)
loadFont desc = Pango.contextLoadFont defaultLayoutContext =<< Pango.fontDescriptionFromString desc

describeFont :: Font -> IO FontDesc
describeFont font = Pango.fontDescriptionToString =<< Pango.fontDescribeWithAbsoluteSize font

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

instance Show Font where
  show = Text.unpack . unsafePerformIO . describeFont
