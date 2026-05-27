module Atlas
 ( Atlas
   ( width
   , height
   , texture
   )
 , newAtlas
 , deleteAtlas
 ) where

import Control.Exception (bracket)
import Foreign qualified as C
import Graphics.GL
import Data.IntMap qualified as Map
import Data.Atlas qualified as Skyline
import Control.Monad.Primitive (PrimState)
import Data.IORef

import GL (Texture, GLObject(..), texture2DSlot, withSlot)

data Atlas a = Atlas
  { glyphIdToPos :: IORef (Map.IntMap (AtlasItem a))
  , texture :: Texture
  , width :: Int
  , height :: Int
  , alloc :: Skyline.Atlas (PrimState IO)
  }

data AtlasItem a = AtlasItem
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  , userdata :: a
  }

newAtlas :: Int -> Int -> IO (Atlas a)
newAtlas width height = do
  texture <- genObject
  withSlot texture2DSlot texture do
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    bracket (C.callocBytes (width * height)) C.free $
      glTexImage2D GL_TEXTURE_2D 0 GL_RED (fromIntegral width) (fromIntegral height) 0 GL_RED GL_UNSIGNED_BYTE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  glyphIdToPos <- newIORef Map.empty
  alloc <- Skyline.create width height
  pure Atlas{..}

deleteAtlas :: Atlas a -> IO ()
deleteAtlas Atlas{texture} = deleteObject texture

queryGlyph :: Atlas a -> Int -> IO (Maybe (AtlasItem a))
queryGlyph atlas gid = do
  m <- readIORef atlas.glyphIdToPos
  pure $ Map.lookup gid m

addGlyph :: Atlas a -> Int -> Int -> Int -> C.Ptr () -> a -> IO Bool
addGlyph atlas gid width height pixels userdata = do
  Skyline.pack1 atlas.alloc (Skyline.Pt width height) >>= \case
    Nothing -> pure False
    Just (Skyline.Pt x y) -> do
      modifyIORef' atlas.glyphIdToPos (Map.insert gid AtlasItem{..})
      withSlot texture2DSlot atlas.texture do
        glPixelStorei GL_UNPACK_ALIGNMENT 1
        glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height) GL_RED GL_UNSIGNED_BYTE pixels
      pure True

{-
addGlyph :: Int -> IO (Int, Int, (C.Ptr () -> IO ()) -> IO (), Int -> Int -> a) -> (a -> (Int, Int)) -> Atlas a -> IO (a, Maybe Int)
addGlyph glyphId render cellFromUserdata atlas = do
  found <- LRU.lookup glyphId atlas.glyphIdToCell
  case found of
    Just userdata -> pure (userdata, Nothing)
    Nothing -> do
      (w, h, withImage, userdataFromCell) <- render
      ((x, y), kicked) <- modifyMVar atlas.freeCells \case
        cell : fc -> pure (fc, (cell, Nothing))
        [] -> do
          Just (kicked, userdata) <- LRU.pop atlas.glyphIdToCell
          pure ([], (cellFromUserdata userdata, Just kicked))
      let userdata = userdataFromCell x y
      LRU.insert glyphId userdata atlas.glyphIdToCell
      let
      withSlot texture2DSlot atlas.texture do
        glPixelStorei GL_UNPACK_ALIGNMENT 1
        bracket (C.callocBytes (atlas.cellWidth * atlas.cellHeight)) C.free $
          glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral atlas.cellWidth) (fromIntegral atlas.cellHeight) GL_RED GL_UNSIGNED_BYTE
        withImage (glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RED GL_UNSIGNED_BYTE)
      pure (userdata, kicked)
-}
