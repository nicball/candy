module Config
  ( Config(..)
  ) where

data Config = Config
  { configFontPath :: FilePath
  , configFontIndex :: Int
  , configFontSizePx :: Int
  , configBackground :: (Float, Float, Float)
  , configForeground :: (Float, Float, Float)
  }
