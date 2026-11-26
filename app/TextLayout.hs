{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TextLayout
  ( newLineLayout
  , layoutTextCached
  , layoutTextCachedDefaultFont
  , LayoutOpt(..)
  , LineLayout
  , layoutGlyphs
  , layoutRes
  , indexToQuad
  , positionToIndex
  , Font
  , GlyphInfo(..)
  , describeFont
  , defaultLayoutOpt
  , test
  ) where

import Config (FontDesc)
import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, when, (<=<))
import Data.Function (fix)
import Data.GI.Base qualified as GI
import Data.Hashable (Hashable)
import Data.IORef
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Data.Text (Text)
import GHC.Generics (Generic)
import GI.Pango qualified as Pango
import GL
import Language.C.Inline qualified as C
import PangoFix.Foreign (pangoCtx)
import PangoFix qualified as Pango
import Refcount
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)
import Lines

import Data.Text.IO qualified as Text

C.context (C.baseCtx <> pangoCtx)
C.include "<pango/pango.h>"
C.include "<pango/pangoft2.h>"

type LayoutContext = Pango.Context

type LineLayout = Pango.Layout

data LayoutOpt = LayoutOpt
  { width :: Maybe Int
  , ellipsize :: Bool
  , fontSpec :: FontSpec
  , defaultFont :: Maybe FontDesc
  }
  deriving stock (Generic, Eq, Ord)
  deriving anyclass Hashable

type FontSpec = Text

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

{-# NOINLINE defaultLayoutContext #-}
defaultLayoutContext :: LayoutContext
defaultLayoutContext = unsafePerformIO $ GI.newObject Pango.Context =<< [C.block| PangoContext* {
    PangoFontMap* font_map = pango_ft2_font_map_new();
    return pango_font_map_create_context(font_map);
  } |]

defaultLayoutOpt :: LayoutOpt
defaultLayoutOpt = LayoutOpt
  { width = Nothing
  , ellipsize = False
  , fontSpec = ""
    -- mconcat
    -- [ Text.pack (show i) <> " " <> Text.pack (show (i + 1)) <> " font-desc \"" <> desc <> "\"\n"
    -- | i <- [0 .. 400] :: [Int]
    -- , italic <- [if i `mod` 8 < 4 then "italic " else " "]
    -- , bold <- [if i `mod` 10 < 5 then "bold " else " "]
    -- , size <- [Text.pack (show (i `mod` 20 + 20)) <> "px"]
    -- , desc <- [italic <> bold <> size]
    -- ]
  , defaultFont = Nothing
  }

test = do
  Text.putStrLn defaultLayoutOpt.fontSpec
  attrs <- Pango.attrListFromString defaultLayoutOpt.fontSpec
  str <- maybe (pure "parse failed") Pango.attrListToString attrs
  Text.putStrLn str

newLineLayout :: Text -> LayoutOpt -> LineLayout
newLineLayout text opts = unsafePerformIO do
  layout <- Pango.layoutNew defaultLayoutContext
  Pango.layoutSetWidth layout (maybe (-1) ((* 1024) . fromIntegral) opts.width)
  Pango.layoutSetWrap layout Pango.WrapModeWordChar
  Pango.layoutSetEllipsize layout (if opts.ellipsize then Pango.EllipsizeModeEnd else Pango.EllipsizeModeNone)
  maybe (pure ()) (Pango.layoutSetFontDescription layout . Just <=< Pango.fontDescriptionFromString) opts.defaultFont
  Pango.layoutSetText layout text (-1)
  attrs <- attrListFromStringCached opts.fontSpec
  Pango.layoutSetAttributes layout (Just attrs)
  pure layout

{-# NOINLINE layoutCache #-}
layoutCache :: Cache (Text, LayoutOpt) LineLayout
layoutCache = unsafePerformIO $ newCache 128

layoutTextCached :: LayoutOpt -> Text -> IO LineLayout
layoutTextCached opts text = unsafeInterleaveIO $ deref =<< lookupCache (text, opts) new (const (pure ())) layoutCache
  where
    new = pure $ newLineLayout text opts

layoutTextCachedDefaultFont :: FontDesc -> Text -> IO LineLayout
layoutTextCachedDefaultFont font = layoutTextCached defaultLayoutOpt{defaultFont=Just font}

{-# NOINLINE attrListCache #-}
attrListCache :: Cache Text Pango.AttrList
attrListCache = unsafePerformIO $ newCache 128

data FontSpecParseError = FontSpecParseError Text
  deriving stock Show
  deriving anyclass Exception

attrListFromStringCached :: Text -> IO Pango.AttrList
attrListFromStringCached str = unsafeInterleaveIO $ deref =<< lookupCache str new (const (pure ())) attrListCache
  where
    new = maybe (throwIO (FontSpecParseError str)) pure =<< Pango.attrListFromString str

layoutRes :: LineLayout -> Resolution
layoutRes layout = unsafePerformIO do
  (w, h) <- Pango.layoutGetPixelSize layout
  pure $ Resolution (fromIntegral w) (fromIntegral h)

positionToIndex :: LineLayout -> Int -> Int -> Int
positionToIndex layout x y = unsafePerformIO do
  (_, index, _) <- Pango.layoutXyToIndex layout (fromIntegral x * 1024) (fromIntegral y * 1024)
  pure $ fromIntegral index

indexToQuad :: LineLayout -> Int -> Quad
indexToQuad layout index = unsafePerformIO do
  -- (p1, _) <- Pango.layoutGetCursorPos layout (fromIntegral index)
  -- (p2, _) <- Pango.layoutGetCursorPos layout (fromIntegral index + 1)
  -- lineHeight <- scale <$> Pango.getRectangleHeight p1
  -- top <- scale <$> Pango.getRectangleY p1
  -- x1 <- scale <$> Pango.getRectangleX p1
  -- x2 <- scale <$> Pango.getRectangleX p2
  -- pure $ quadFromYXRange top (top + lineHeight - 1) (min x1 x2) (max x1 x2)
  p <- Pango.layoutIndexToPos layout (fromIntegral index)
  y <- scale <$> Pango.getRectangleY p
  x <- scale <$> Pango.getRectangleX p
  w <- scale <$> Pango.getRectangleWidth p
  h <- scale <$> Pango.getRectangleHeight p
  pure $ quadFromTopLeftWH (min x (x + w)) y (max 1 (abs w)) h {- DIRTY HACK! -}
  where
    scale x = fromIntegral (x `div` 1024)

layoutGlyphs :: LineLayout -> [GlyphInfo]
layoutGlyphs layout = unsafePerformIO do
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
      runOffset <- fromIntegral <$> (Pango.getItemOffset . fromJust =<< Pango.getGlyphItemItem run)
      glyphInfos <- Pango.getGlyphStringGlyphs glyphStr
      glyphClusters <- map (runOffset +) <$> Pango.getGlyphStringLogClusters' glyphStr
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

{-
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
-}

-- loadFont :: FontDesc -> IO (Maybe Font)
-- loadFont desc = Pango.contextLoadFont defaultLayoutContext =<< Pango.fontDescriptionFromString desc

describeFont :: Font -> FontDesc
describeFont font = unsafePerformIO $ Pango.fontDescriptionToString =<< Pango.fontDescribeWithAbsoluteSize font

instance Show Font where
  show = Text.unpack . describeFont
