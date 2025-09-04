{-# LANGUAGE DeriveAnyClass #-}

module Raqm
  ( Raqm
  , RaqmError
  , create
  , destroy
  , withRaqm
  , clearContents
  , Direction
  , directionDefault
  , directionLTR
  , directionRTL
  , directionTTB
  , setParDirection
  , setText
  , setLanguage
  , setFreetypeFace
  , addFontFeature
  , layout
  , getGlyphs
  , Glyph(..)
  , indexToPosition
  , positionToIndex
  ) where

#include <raqm.h>

import Control.Exception (Exception, throwIO, finally)
import Data.Text.Foreign qualified as Text
import Data.Text (Text)
import Data.Word
import Foreign.C.String qualified as C
import Foreign.C.Types qualified as C
import Foreign.Marshal (alloca, with, peekArray)
import Foreign.Ptr qualified as C
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke, peekByteOff, pokeByteOff)
import FreeType (FT_Face)

newtype Raqm = Raqm (C.Ptr Raqm_)
data Raqm_

data RaqmError = RaqmError Text deriving (Show, Exception)

checkBool :: Text -> Bool -> IO ()
checkBool msg False = throwIO (RaqmError msg)
checkBool _ True = pure ()

foreign import ccall "raqm_create"
  create :: IO Raqm

foreign import ccall "raqm_destroy"
  destroy :: Raqm -> IO ()

withRaqm :: (Raqm -> IO a) -> IO a
withRaqm action = do
  rq <- create
  action rq `finally` destroy rq

foreign import ccall "raqm_clear_contents"
  clearContents :: Raqm -> IO ()

newtype Direction = Direction #{type raqm_direction_t}

#enum Direction, Direction, directionDefault = RAQM_DIRECTION_DEFAULT, directionLTR = RAQM_DIRECTION_LTR, directionRTL = RAQM_DIRECTION_RTL, directionTTB = RAQM_DIRECTION_TTB

foreign import ccall "raqm_set_par_direction"
  setParDirection_ :: Raqm -> Direction -> IO Bool

setParDirection :: Raqm -> Direction -> IO ()
setParDirection rq dir = checkBool "setParDirection" =<< setParDirection_ rq dir

foreign import ccall "raqm_set_text_utf8"
  setTextUtf8_ :: Raqm -> C.Ptr a -> C.CSize -> IO Bool

setText :: Raqm -> Text -> IO ()
setText rq text = checkBool "setText" =<< Text.useAsPtr text (\p n -> setTextUtf8_ rq p (fromIntegral n))

foreign import ccall "raqm_set_language"
  setLanguage_ :: Raqm -> C.Ptr a -> C.CSize -> C.CSize -> IO Bool
  
setLanguage :: Raqm -> Text -> Int -> Int -> IO ()
setLanguage rq lang start end = checkBool "setLanguage" =<< Text.withCString lang (\l -> setLanguage_ rq l (fromIntegral start) (fromIntegral end))

foreign import ccall "raqm_set_freetype_face"
  setFreetypeFace_ :: Raqm -> FT_Face -> IO Bool

setFreetypeFace :: Raqm -> FT_Face -> IO ()
setFreetypeFace rq face = checkBool "setFreetypeFace" =<< setFreetypeFace_ rq face

foreign import ccall "raqm_add_font_feature"
  addFontFeature_ :: Raqm -> C.CString -> C.CInt -> IO Bool

addFontFeature :: Raqm -> Text -> IO ()
addFontFeature rq feature = checkBool "addFontFeature" =<< Text.withCString feature (\f -> addFontFeature_ rq f (-1))

foreign import ccall "raqm_layout"
  layout_ :: Raqm -> IO Bool

layout :: Raqm -> IO ()
layout rq = checkBool "layout" =<< layout_ rq

foreign import ccall "raqm_get_glyphs"
 getGlyphs_ :: Raqm -> C.Ptr C.CSize -> IO (C.Ptr Glyph)

getGlyphs :: Raqm -> IO [Glyph]
getGlyphs rq = do
  alloca $ \size -> do
    pglyphs <- getGlyphs_ rq size
    s <- peek size
    peekArray (fromIntegral s) pglyphs

data Glyph = Glyph
  { gIndex :: Int
  , gXAdvance :: Int
  , gYAdvance :: Int
  , gXOffset :: Int
  , gYOffset :: Int
  , gCluster :: Int
  , gFtFace :: FT_Face
  }
  deriving Show

instance Storable Glyph where
  sizeOf _ = #{size raqm_glyph_t}
  alignment _ = #{alignment raqm_glyph_t}
  peek addr = Glyph
    <$> fmap (fromIntegral :: C.CUInt -> Int) (#{peek raqm_glyph_t, index}     addr)
    <*> fmap (fromIntegral :: C.CInt -> Int)  (#{peek raqm_glyph_t, x_advance} addr)
    <*> fmap (fromIntegral :: C.CInt -> Int)  (#{peek raqm_glyph_t, y_advance} addr)
    <*> fmap (fromIntegral :: C.CInt -> Int)  (#{peek raqm_glyph_t, x_offset}  addr)
    <*> fmap (fromIntegral :: C.CInt -> Int)  (#{peek raqm_glyph_t, y_offset}  addr)
    <*> fmap (fromIntegral :: Word32 -> Int)  (#{peek raqm_glyph_t, cluster}   addr)
    <*>                                       (#{peek raqm_glyph_t, ftface}    addr)
  poke addr Glyph{..} = do
    #{poke raqm_glyph_t, index}     addr (fromIntegral gIndex    :: C.CUInt)
    #{poke raqm_glyph_t, x_advance} addr (fromIntegral gXAdvance :: C.CInt)
    #{poke raqm_glyph_t, y_advance} addr (fromIntegral gYAdvance :: C.CInt)
    #{poke raqm_glyph_t, x_offset}  addr (fromIntegral gXOffset  :: C.CInt)
    #{poke raqm_glyph_t, y_offset}  addr (fromIntegral gYOffset  :: C.CInt)
    #{poke raqm_glyph_t, cluster}   addr (fromIntegral gCluster  :: Word32)
    #{poke raqm_glyph_t, ftface}    addr               gFtFace

foreign import ccall "raqm_index_to_position"
  indexToPosition_ :: Raqm -> C.Ptr C.CSize -> C.Ptr C.CInt -> C.Ptr C.CInt -> IO Bool

indexToPosition :: Raqm -> Int -> IO (Int, Int, Int)
indexToPosition rq pos =
  with (fromIntegral pos) $ \ppos ->
    alloca $ \px ->
      alloca $ \py -> do
        checkBool "indexToPosition" =<< indexToPosition_ rq ppos px py
        (,,) <$> (fromIntegral <$> peek ppos) <*> (fromIntegral . (`div` 64) <$> peek px) <*> (fromIntegral . (`div` 64) <$> peek py)

foreign import ccall "raqm_position_to_index"
  positionToIndex_ :: Raqm -> C.CInt -> C.CInt -> C.Ptr C.CSize -> IO Bool

positionToIndex :: Raqm -> Int -> Int -> IO Int
positionToIndex rq x y =
  alloca $ \pindex -> do
    checkBool "positionToIndex" =<< positionToIndex_ rq (fromIntegral x * 64) (fromIntegral y * 64) pindex
    fromIntegral <$> peek pindex
