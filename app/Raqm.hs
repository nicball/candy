{-# LANGUAGE DeriveAnyClass #-}

module Raqm
  ( Raqm
  , RaqmError
  , create
  , destroy
  , withRaqm
  , clearContents
  , setText
  , setLanguage
  , setFreetypeFace
  , addFontFeature
  , layout
  , getGlyphs
  , Glyph(..)
  , indexToPosition
  , indexToXPosition
  , positionToIndex
  ) where

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

data Raqm

data RaqmError = RaqmError Text deriving (Show, Exception)

create :: IO Raqm
create = undefined

destroy :: Raqm -> IO ()
destroy = undefined

withRaqm :: (Raqm -> IO a) -> IO a
withRaqm action = undefined

clearContents :: Raqm -> IO ()
clearContents = undefined

setText :: Raqm -> Text -> IO ()
setText = undefined

setLanguage :: Raqm -> Text -> Int -> Int -> IO ()
setLanguage = undefined

setFreetypeFace :: Raqm -> FT_Face -> IO ()
setFreetypeFace = undefined

addFontFeature :: Raqm -> Text -> IO ()
addFontFeature = undefined

layout :: Raqm -> IO ()
layout = undefined

getGlyphs :: Raqm -> IO [Glyph]
getGlyphs = undefined

data Glyph = Glyph
  { index :: Int
  , xAdvance :: Int
  , yAdvance :: Int
  , xOffset :: Int
  , yOffset :: Int
  , cluster :: Int
  , ftFace :: FT_Face
  }
  deriving Show

indexToPosition :: Raqm -> Int -> IO (Int, Int, Int)
indexToPosition = undefined

indexToXPosition :: Raqm -> Int -> IO Int
indexToXPosition = undefined

positionToIndex :: Raqm -> Int -> Int -> IO Int
positionToIndex = undefined
