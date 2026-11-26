module Atlas
 ( Atlas(texture)
 , AtlasItem(..)
 , newAtlas
 , deleteAtlas
 , clearAtlas
 , queryGlyph
 , addGlyph
 ) where

import Control.Exception (bracket)
import Control.Monad.Primitive (PrimState)
import Data.Atlas qualified as Skyline
import Data.IORef
import Data.Map.Strict qualified as Map
import Foreign qualified as C
import Graphics.GL

import GL (Texture, GLObject(..), texture2DSlot, withSlot)

data Atlas k a = Atlas
  { texture :: Texture
  , alloc :: Skyline.Atlas (PrimState IO)
  , glyphs :: IORef (Map.Map k (AtlasItem a))
  }

data AtlasItem a = AtlasItem
  { x :: Int
  , y :: Int
  , userdata :: a
  }

newAtlas :: Int -> Int -> IO (Atlas k a)
newAtlas width height = do
  texture <- genObject
  withSlot texture2DSlot texture do
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    bracket (C.callocBytes (width * height)) C.free $
      glTexImage2D GL_TEXTURE_2D 0 GL_RED (fromIntegral width) (fromIntegral height) 0 GL_RED GL_UNSIGNED_BYTE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  glyphs <- newIORef Map.empty
  alloc <- Skyline.create width height
  pure Atlas{..}

deleteAtlas :: Atlas k a -> IO ()
deleteAtlas atlas = deleteObject atlas.texture

-- Invalidates every AtlasItem. New glyph uploads overwrite the reused regions.
clearAtlas :: Atlas k a -> IO ()
clearAtlas atlas = do
  Skyline.reset atlas.alloc
  writeIORef atlas.glyphs Map.empty

queryGlyph :: Ord k => Atlas k a -> k -> IO (Maybe (AtlasItem a))
queryGlyph atlas key = Map.lookup key <$> readIORef atlas.glyphs

-- Nothing means the glyph could not be packed into the current page.
addGlyph :: Ord k => Atlas k a -> k -> Int -> Int -> C.Ptr () -> a -> IO (Maybe (AtlasItem a))
addGlyph atlas gid width height pixels userdata = do
  Skyline.pack1 atlas.alloc (Skyline.Pt width height) >>= \case
    Nothing -> pure Nothing
    Just (Skyline.Pt x y) -> do
      let item = AtlasItem{..}
      withSlot texture2DSlot atlas.texture do
        glPixelStorei GL_UNPACK_ALIGNMENT 1
        glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height) GL_RED GL_UNSIGNED_BYTE pixels
      modifyIORef' atlas.glyphs (Map.insert gid item)
      pure (Just item)
