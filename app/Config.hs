module Config
  ( Config(..)
  , FaceID(..)
  ) where

data Config = Config
  { configFace :: FaceID
  , configBackground :: (Float, Float, Float, Float)
  , configForeground :: (Float, Float, Float, Float)
  }

data FaceID = FaceID
  { faceIDPath :: FilePath
  , faceIDIndex :: Int
  , faceIDSizePx :: Int
  }
  deriving (Eq, Ord)
