{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TextLayout.Foreign
  ( textLayoutCtx
  , GlyphRenderer(..)
  ) where

import Language.C.Inline qualified as C
import Language.C.Inline.Context qualified as C
import Language.C.Types qualified as C
import Data.Map qualified as Map
import Foreign qualified as C

newtype GlyphRenderer = GlyphRenderer { ptr :: C.ForeignPtr GlyphRenderer }

textLayoutCtx :: C.Context
textLayoutCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "hb_raster_draw_t", [t| GlyphRenderer |])
    ]
  }
