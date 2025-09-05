module Atlas
 ( Atlas
   ( width
   , height
   , texture
   )
 , newAtlas
 , freeAtlas
 , withAtlas
 , addGlyph
 ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (bracket)
import Data.Cache.LRU.IO qualified as LRU
import Foreign qualified as C
import Graphics.GL

import GL (Texture, GLObject(..), texture2DSlot, withSlot)

data Atlas a = Atlas
  { cellWidth :: Int
  , cellHeight :: Int
  , freeCells :: MVar [(Int, Int)]
  , glyphIdToCell :: LRU.AtomicLRU Int a
  , texture :: Texture
  , width :: Int
  , height :: Int
  }

newAtlas :: Int -> Int -> Int -> IO (Atlas a)
newAtlas len w h = do
  texture <- genObject
  let numColumns = ceiling (sqrt (fromIntegral len :: Double))
      numRows = floor (sqrt (fromIntegral len :: Double))
  withSlot texture2DSlot texture do
    glPixelStorei GL_UNPACK_ALIGNMENT 1
    bracket (C.callocBytes (w * h * numColumns * numRows)) C.free $
      glTexImage2D GL_TEXTURE_2D 0 GL_RED (fromIntegral (w * numColumns)) (fromIntegral (h * numRows)) 0 GL_RED GL_UNSIGNED_BYTE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  glyphIdToCell <- LRU.newAtomicLRU . Just . fromIntegral $ len
  freeCells <- newMVar . take len $ [(x * w, y * h) | y <- [0 .. numRows - 1], x <- [0 .. numColumns - 1]]
  pure Atlas
    { cellWidth = w
    , cellHeight = h
    , glyphIdToCell
    , freeCells
    , texture
    , width = w * numColumns
    , height = h * numRows
    }

freeAtlas :: Atlas a -> IO ()
freeAtlas Atlas{texture} = deleteObject texture

withAtlas :: Int -> Int -> Int -> (Atlas a -> IO b) -> IO b
withAtlas len w h = bracket (newAtlas len w h) freeAtlas

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
