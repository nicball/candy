module Config
  ( Config(..)
  , FaceID(..)
  , Color(..)
  , colorToRGBA
  ) where

data Color = Color
  { red :: Float
  , green :: Float
  , blue :: Float
  , alpha :: Float
  }

colorToRGBA :: Color -> [Float]
colorToRGBA Color{..} = [ red, green, blue, alpha ]

data Config = Config
  { face :: FaceID
  , background :: Color
  , foreground :: Color
  , primarySelectionForeground :: Color
  , primarySelectionBackground :: Color
  , primaryCursorForeground :: Color
  , primaryCursorBackground :: Color
  , cursorRangeOnScreen :: (Float, Float)
  }

data FaceID = FaceID
  { path :: FilePath
  , index :: Int
  , sizePx :: Int
  }
  deriving (Eq, Ord)
