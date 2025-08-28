{-# LANGUAGE BlockArguments
           , PatternSynonyms
           , OverloadedStrings
           , LambdaCase
           #-}

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
 , getGlyphCoord
 ) where

import Graphics.GL
import qualified Data.Cache.LRU as LRU
import qualified Foreign as C
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (assert, bracket)
import Data.Maybe (isNothing)

import GL (Texture, GLObject(..), texture2DSlot, withSlot, checkGLError)

data Atlas = Atlas
  { atlasCellWidth :: Int
  , atlasCellHeight :: Int
  , atlasFreeCells :: MVar [(Int, Int)]
  , atlasGlyphIdToCell :: IORef (LRU.LRU Int (Int, Int))
  , atlasTexture :: Texture
  , atlasWidth :: Int
  , atlasHeight :: Int
  }

newAtlas :: Int -> Int -> Int -> IO Atlas
newAtlas len w h = do
  atlasTexture <- genObject
  let numColumns = truncate (sqrt (fromIntegral len :: Double))
      numRows = len `div` numColumns
  withSlot texture2DSlot atlasTexture do
    bracket (C.callocBytes (w * h * numColumns * numRows)) C.free $
      glTexImage2D GL_TEXTURE_2D 0 GL_RED (fromIntegral (w * numColumns)) (fromIntegral (h * numRows)) 0 GL_RED GL_UNSIGNED_BYTE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST_MIPMAP_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  atlasGlyphIdToCell <- newIORef . LRU.newLRU . Just . fromIntegral $ len
  atlasFreeCells <- newMVar [(x, y) | y <- [0 .. numRows - 1], x <- [0 .. numColumns - 1]]
  pure $ Atlas
    { atlasCellWidth = w
    , atlasCellHeight = h
    , atlasGlyphIdToCell
    , atlasFreeCells
    , atlasTexture
    , atlasWidth = w * numColumns
    , atlasHeight = h * numRows
    }

freeAtlas :: Atlas -> IO ()
freeAtlas Atlas{atlasTexture = tex} = deleteObject tex

withAtlas :: Int -> Int -> Int -> (Atlas -> IO a) -> IO a
withAtlas len w h = bracket (newAtlas len w h) freeAtlas

addGlyph :: Int -> Int -> Int -> C.Ptr () -> Atlas -> IO (Int, Int, Maybe Int)
addGlyph glyphId w h image atlas = do
  glPixelStorei GL_UNPACK_ALIGNMENT 1
  found <- atomicModifyIORef' (atlasGlyphIdToCell atlas) (LRU.lookup glyphId)
  case found of
    Just (cellX, cellY) -> pure (fromIntegral (cellX * atlasCellWidth atlas), fromIntegral (cellY * atlasCellHeight atlas), Nothing)
    Nothing -> do
      (cell@(cellX, cellY), kicked) <- modifyMVar (atlasFreeCells atlas) \case
        cell : fc -> pure (fc, (cell, Nothing))
        [] -> do
          Just (kicked, cell) <- atomicModifyIORef' (atlasGlyphIdToCell atlas) LRU.pop
          pure ([], (cell, Just kicked))
      k <- atomicModifyIORef' (atlasGlyphIdToCell atlas) (LRU.insertInforming glyphId cell)
      assert (isNothing k) (pure ())
      let
        cellW = atlasCellWidth atlas
        cellH = atlasCellHeight atlas
        x = cellX * cellW
        y = cellY * cellH
      withSlot texture2DSlot (atlasTexture atlas) do
        bracket (C.callocBytes (cellW * cellH)) C.free $
          glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral cellW) (fromIntegral cellH) GL_RED GL_UNSIGNED_BYTE
        glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RED GL_UNSIGNED_BYTE image
      pure (x, y, kicked)

getGlyphCoord :: Int -> Atlas -> IO (Maybe (Int, Int))
getGlyphCoord glyphId atlas = atomicModifyIORef' (atlasGlyphIdToCell atlas) (LRU.lookup glyphId)
