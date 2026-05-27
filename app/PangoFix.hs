{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module PangoFix
  ( getGlyphStringGlyphs
  , getGlyphStringLogClusters'
  ) where

import Data.GI.Base qualified as GI
import Foreign qualified as C
import GI.Pango qualified as Pango
import Language.C.Inline qualified as C
import PangoFix.Foreign (pangoCtx)

C.context (C.baseCtx <> pangoCtx)
C.include "<pango/pango.h>"

getGlyphStringGlyphs :: Pango.GlyphString -> IO [Pango.GlyphInfo]
getGlyphStringGlyphs gs = GI.withManagedPtr gs \pGs -> do
  let pG = [C.pure| PangoGlyphInfo* { $(PangoGlyphString* pGs)->glyphs } |]
  n <- Pango.getGlyphStringNumGlyphs gs
  mapM (GI.wrapPtr Pango.GlyphInfo) =<< GI.unpackBlockArrayWithLength (fromIntegral [C.pure| size_t { sizeof(PangoGlyphInfo) } |]) n pG

getGlyphStringLogClusters' :: Pango.GlyphString -> IO [Int]
getGlyphStringLogClusters' gs = GI.withManagedPtr gs \pGs -> do
  let pL = [C.pure| int* { $(PangoGlyphString* pGs)->log_clusters } |]
  n <- fromIntegral <$> Pango.getGlyphStringNumGlyphs gs
  map fromIntegral <$> C.peekArray n pL
