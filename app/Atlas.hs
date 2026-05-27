module Atlas
 ( Atlas(width, height)
 , AtlasItem(..)
 , newAtlas
 , deleteAtlas
 , queryGlyph
 , addGlyph
 ) where

import Control.Exception (bracket)
import Foreign qualified as C
import Graphics.GL
import Data.Map.Strict qualified as Map
import Data.Atlas qualified as Skyline
import Control.Monad.Primitive (PrimState)
import Data.IORef
import Data.List.NonEmpty qualified as NonEmpty
import Control.Monad ((<=<))
import Data.Monoid (First(..))

import GL (Texture, GLObject(..), texture2DSlot, withSlot)

data Atlas k a = Atlas
  { pages :: IORef (NonEmpty.NonEmpty (AtlasPage k a))
  , width :: Int
  , height :: Int
  }

data AtlasPage k a = AtlasPage
  { glyphIdToPos :: IORef (Map.Map k (AtlasItem a))
  , texture :: Texture
  , alloc :: Skyline.Atlas (PrimState IO)
  }

data AtlasItem a = AtlasItem
  { x :: Int
  , y :: Int
  , texture :: Texture
  , userdata :: a
  }

newAtlas :: Int -> Int -> IO (Atlas k a)
newAtlas width height = do
  page <- newAtlasPage width height
  pages <- newIORef (NonEmpty.singleton page)
  pure Atlas{..}

newAtlasPage :: Int -> Int -> IO (AtlasPage k a)
newAtlasPage width height = do
  texture <- genObject
  withSlot texture2DSlot texture do
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    bracket (C.callocBytes (width * height)) C.free $
      glTexImage2D GL_TEXTURE_2D 0 GL_RED (fromIntegral width) (fromIntegral height) 0 GL_RED GL_UNSIGNED_BYTE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  glyphIdToPos <- newIORef Map.empty
  alloc <- Skyline.create width height
  pure AtlasPage{..}

deleteAtlas :: Atlas k a -> IO ()
deleteAtlas = mapM_ (deleteObject . (.texture)) <=< readIORef . (.pages)

queryGlyph :: Ord k => Atlas k a -> k -> IO (Maybe (AtlasItem a))
queryGlyph atlas key = getFirst <$> (foldMap (fmap (First . Map.lookup key) . readIORef . (.glyphIdToPos)) =<< readIORef atlas.pages)

addGlyph :: Ord k => Atlas k a -> k -> Int -> Int -> C.Ptr () -> a -> IO (AtlasItem a)
addGlyph atlas gid width height pixels userdata = do
  page@AtlasPage{texture} <- NonEmpty.head <$> readIORef atlas.pages
  Skyline.pack1 page.alloc (Skyline.Pt width height) >>= \case
    Nothing -> do
      newPage <- newAtlasPage width height
      modifyIORef' atlas.pages (NonEmpty.cons newPage)
      addGlyph atlas gid width height pixels userdata
    Just (Skyline.Pt x y) -> do
      let item = AtlasItem{..}
      modifyIORef' page.glyphIdToPos (Map.insert gid item)
      withSlot texture2DSlot texture do
        glPixelStorei GL_UNPACK_ALIGNMENT 1
        glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height) GL_RED GL_UNSIGNED_BYTE pixels
      pure item
