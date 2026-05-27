{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module PangoFix.Foreign
  ( pangoCtx
  ) where

import Language.C.Inline qualified as C
import Language.C.Inline.Context qualified as C
import Language.C.Types qualified as C
import Data.Map qualified as Map
import GI.Pango qualified as Pango

pangoCtx :: C.Context
pangoCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "PangoGlyphString", [t| Pango.GlyphString |])
    , (C.TypeName "PangoGlyphInfo", [t| Pango.GlyphInfo |])
    , (C.TypeName "PangoGlyphItem", [t| Pango.GlyphItem |])
    , (C.TypeName "PangoFont", [t| Pango.Font |])
    , (C.TypeName "PangoFontMap", [t| Pango.FontMap |])
    , (C.TypeName "PangoContext", [t| Pango.Context|])
    , (C.TypeName "PangoLayout", [t| Pango.Layout|])
    ]
  }
