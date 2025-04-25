{-# LANGUAGE BlockArguments
           , PatternSynonyms
           , LambdaCase
           #-}

module Atlas
 ( Atlas
 , newAtlas
 , freeAtlas
 , addGlyph
 ) where

import Graphics.GL
    ( pattern GL_R8UI
    , pattern GL_RED
    , pattern GL_TEXTURE_2D
    , pattern GL_UNSIGNED_BYTE
    , glDeleteTextures
    , glGenTextures
    , glTexImage2D
    , GLfloat
    , GLubyte
    , GLuint, glTexSubImage2D
    )
-- import qualified Data.IntMap.Strict as IntMap
import qualified Data.Cache.LRU as LRU
import qualified Foreign.Marshal.Utils as C
import qualified Foreign.Marshal.Array as C
import qualified Foreign.Ptr as C
import qualified Foreign.Storable as C
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (assert)
import Data.Maybe (isNothing)

import GL (checkGlError, withTexture2D)

data Atlas = Atlas
  { atlasLength :: Int
  , atlasCellWidth :: Int
  , atlasCellHeight :: Int
  , atlasFreeCells :: MVar [Int]
  , atlasGlyphIdToCell :: IORef (LRU.LRU Int Int)
  , atlasTexture :: GLuint
  }

newAtlas :: Int -> Int -> Int -> IO Atlas
newAtlas len w h = do
  tex <- C.with 0 \p -> do
    glGenTextures 1 p
    C.peek p
  withTexture2D tex do
    C.withArray (replicate (w * h * len) (0 :: GLubyte) ) \arr -> do
      glTexImage2D GL_TEXTURE_2D 0 GL_R8UI (fromIntegral (w * len)) (fromIntegral h) 0 GL_RED GL_UNSIGNED_BYTE (C.castPtr arr)
      checkGlError
  atlasGlyphIdToCell <- newIORef . LRU.newLRU . Just . fromIntegral $ len
  atlasFreeCells <- newMVar [0 .. len - 1]
  pure $ Atlas
    { atlasLength = len
    , atlasCellWidth = w
    , atlasCellHeight = h
    , atlasGlyphIdToCell
    , atlasFreeCells
    , atlasTexture = tex
    }

freeAtlas :: Atlas -> IO ()
freeAtlas Atlas{ atlasTexture = tex } = do
  C.with tex \p -> glDeleteTextures 1 p

addGlyph :: Int -> Int -> Int -> C.Ptr () -> Atlas -> IO (GLfloat, GLfloat, Maybe Int)
addGlyph glyphId w h image atlas = do
  found <- atomicModifyIORef' (atlasGlyphIdToCell atlas) (LRU.lookup glyphId)
  case found of
    Just cell -> pure (fromIntegral (cell * atlasCellWidth atlas), 0, Nothing)
    Nothing -> do
      (cell, kicked) <- modifyMVar (atlasFreeCells atlas) \case
        cell : fc -> pure (fc, (cell, Nothing))
        [] -> do
          Just (kicked, cell) <- atomicModifyIORef' (atlasGlyphIdToCell atlas) LRU.pop
          pure ([], (cell, Just kicked))
      k <- atomicModifyIORef' (atlasGlyphIdToCell atlas) (LRU.insertInforming glyphId cell)
      assert (isNothing k) (pure ())
      let x = cell * atlasCellWidth atlas
          y = 0 :: Int
      withTexture2D (atlasTexture atlas) do
        glTexSubImage2D GL_TEXTURE_2D 0 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RED GL_UNSIGNED_BYTE image
        checkGlError
      pure (fromIntegral x, fromIntegral y, kicked)
