{-# LANGUAGE BlockArguments
           , PatternSynonyms
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
    ( pattern GL_RED
    , pattern GL_TEXTURE_2D
    , pattern GL_UNSIGNED_BYTE
    , pattern GL_UNPACK_ALIGNMENT
    , glTexImage2D
    , glTexSubImage2D
    , glPixelStorei
    )
import qualified Data.Cache.LRU as LRU
import qualified Foreign.Ptr as C
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (assert, bracket)
import Data.Maybe (isNothing)

import GL (checkGlError, Texture, GLObject(..), texture2DSlot, withSlot)

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
    -- C.withArray (replicate (w * h * numColumns * numRows) (0 :: GLubyte) ) \arr -> do
    glTexImage2D GL_TEXTURE_2D 0 GL_RED (fromIntegral (w * numColumns)) (fromIntegral (h * numRows)) 0 GL_RED GL_UNSIGNED_BYTE C.nullPtr
    checkGlError
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
      let x = cellX * atlasCellWidth atlas
          y = cellY * atlasCellHeight atlas
      withSlot texture2DSlot (atlasTexture atlas) do
        glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RED GL_UNSIGNED_BYTE image
        checkGlError
      pure (x, y, kicked)

getGlyphCoord :: Int -> Atlas -> IO (Maybe (Int, Int))
getGlyphCoord glyphId atlas = atomicModifyIORef' (atlasGlyphIdToCell atlas) (LRU.lookup glyphId)
