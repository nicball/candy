module Config
  ( Config(..)
  ) where

data Config = Config
  { configFontPath :: FilePath
  , configFontSizePx :: Int
  , configBackground :: (Float, Float, Float)
  , configForeground :: (Float, Float, Float)
  }
