module Atlas
 ( Atlas
   ( atlasWidth
   , atlasHeight
   , atlasTexture
   )
 , newAtlas
 , freeAtlas
 , withAtlas
 , addGlyph
 ) where

import Graphics.GL
import qualified Data.Cache.LRU.IO as LRU
import qualified Foreign as C
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (bracket)

import GL (Texture, GLObject(..), texture2DSlot, withSlot)

data Atlas a = Atlas
  { atlasCellWidth :: Int
  , atlasCellHeight :: Int
  , atlasFreeCells :: MVar [(Int, Int)]
  , atlasGlyphIdToCell :: LRU.AtomicLRU Int a
  , atlasTexture :: Texture
  , atlasWidth :: Int
  , atlasHeight :: Int
  }

newAtlas :: Int -> Int -> Int -> IO (Atlas a)
newAtlas len w h = do
  atlasTexture <- genObject
  let numColumns = truncate (sqrt (fromIntegral len :: Double))
      numRows = len `div` numColumns
  withSlot texture2DSlot atlasTexture do
    bracket (C.callocBytes (w * h * numColumns * numRows)) C.free $
      glTexImage2D GL_TEXTURE_2D 0 GL_RED (fromIntegral (w * numColumns)) (fromIntegral (h * numRows)) 0 GL_RED GL_UNSIGNED_BYTE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST_MIPMAP_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  atlasGlyphIdToCell <- LRU.newAtomicLRU . Just . fromIntegral $ len
  atlasFreeCells <- newMVar [(x * w, y * h) | y <- [0 .. numRows - 1], x <- [0 .. numColumns - 1]]
  pure Atlas
    { atlasCellWidth = w
    , atlasCellHeight = h
    , atlasGlyphIdToCell
    , atlasFreeCells
    , atlasTexture
    , atlasWidth = w * numColumns
    , atlasHeight = h * numRows
    }

freeAtlas :: Atlas a -> IO ()
freeAtlas Atlas{atlasTexture = tex} = deleteObject tex

withAtlas :: Int -> Int -> Int -> (Atlas a -> IO b) -> IO b
withAtlas len w h = bracket (newAtlas len w h) freeAtlas

addGlyph :: Int -> IO (Int, Int, (C.Ptr () -> IO ()) -> IO (), Int -> Int -> a) -> (a -> (Int, Int)) -> Atlas a -> IO (a, Maybe Int)
addGlyph glyphId render cellFromUserdata atlas = do
  glPixelStorei GL_UNPACK_ALIGNMENT 1
  found <- LRU.lookup glyphId (atlasGlyphIdToCell atlas)
  case found of
    Just userdata -> pure (userdata, Nothing)
    Nothing -> do
      (w, h, withImage, userdataFromCell) <- render
      ((x, y), kicked) <- modifyMVar (atlasFreeCells atlas) \case
        cell : fc -> pure (fc, (cell, Nothing))
        [] -> do
          Just (kicked, userdata) <- LRU.pop (atlasGlyphIdToCell atlas)
          pure ([], (cellFromUserdata userdata, Just kicked))
      let userdata = userdataFromCell x y
      LRU.insert glyphId userdata (atlasGlyphIdToCell atlas)
      let
        cellW = atlasCellWidth atlas
        cellH = atlasCellHeight atlas
      withSlot texture2DSlot (atlasTexture atlas) do
        bracket (C.callocBytes (cellW * cellH)) C.free $
          glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral cellW) (fromIntegral cellH) GL_RED GL_UNSIGNED_BYTE
        withImage (glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RED GL_UNSIGNED_BYTE)
      pure (userdata, kicked)
