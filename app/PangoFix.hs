{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module PangoFix
  ( getGlyphStringGlyphs
  ) where

import GI.Pango qualified as Pango
import PangoFix.Foreign (pangoCtx)
import Language.C.Inline qualified as C
import Data.GI.Base qualified as C

C.context (C.baseCtx <> pangoCtx)
C.include "<pango/pango.h>"

getGlyphStringGlyphs :: Pango.GlyphString -> IO [Pango.GlyphInfo]
getGlyphStringGlyphs gs = C.withManagedPtr gs \pGs -> do
  let pG = [C.pure| PangoGlyphInfo* { $(PangoGlyphString* pGs)->glyphs } |]
  n <- Pango.getGlyphStringNumGlyphs gs
  mapM (C.wrapPtr Pango.GlyphInfo) =<< C.unpackBlockArrayWithLength (fromIntegral [C.pure| size_t { sizeof(PangoGlyphInfo) } |]) n pG
