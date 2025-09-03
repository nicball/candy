module Config
  ( Config(..)
  , FaceID(..)
  , Color(..)
  , colorToRGBA
  ) where

data Color = Color
  { colorRed :: Float
  , colorGreen :: Float
  , colorBlue :: Float
  , colorAlpha :: Float
  }

colorToRGBA :: Color -> [Float]
colorToRGBA Color{..} = [ colorRed, colorGreen, colorBlue, colorAlpha ]

data Config = Config
  { configFace :: FaceID
  , configBackground :: Color
  , configForeground :: Color
  , configPrimarySelectionForeground :: Color
  , configPrimarySelectionBackground :: Color
  , configPrimaryCursorForeground :: Color
  , configPrimaryCursorBackground :: Color
  }

data FaceID = FaceID
  { faceIDPath :: FilePath
  , faceIDIndex :: Int
  , faceIDSizePx :: Int
  }
  deriving (Eq, Ord)
