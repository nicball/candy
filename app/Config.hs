module Config
  ( Config(..)
  , FaceID(..)
  , Color(..)
  , colorToRGBA
  , nord0, nord1, nord2, nord3
  , nord4, nord5, nord6, nord7
  , nord8, nord9, nord10, nord11
  , nord12, nord13, nord14, nord15
  , config
  ) where

import Data.IORef (IORef, newIORef)
import System.IO.Unsafe (unsafePerformIO)

data Color = Color
  { red :: Float
  , green :: Float
  , blue :: Float
  , alpha :: Float
  }
  deriving (Show, Eq, Ord)

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
  , cursorVerticalRangeOnScreen :: (Float, Float)
  , cursorHorizontalRangeOnScreen :: (Float, Float)
  , lineNumbersForeground :: Color
  , lineNumbersBackground :: Color
  , lineNumbersCurrentForeground :: Color
  , lineNumbersCurrentBackground :: Color
  , barBackground :: Color
  , barForeground :: Color
  , tilingMargin :: Int
  }

data FaceID = FaceID
  { path :: FilePath
  , index :: Int
  , sizePx :: Int
  }
  deriving (Eq, Ord)

nord0, nord1, nord2, nord3 :: Color
nord4, nord5, nord6, nord7 :: Color
nord8, nord9, nord10, nord11 :: Color
nord12, nord13, nord14, nord15 :: Color

nord0 = Color 0.180392 0.203922 0.250980 1
nord1 = Color 0.231373 0.258824 0.321569 1
nord2 = Color 0.262745 0.298039 0.368627 1
nord3 = Color 0.298039 0.337255 0.415686 1
nord4 = Color 0.847059 0.870588 0.913725 1
nord5 = Color 0.898039 0.913725 0.941176 1
nord6 = Color 0.925490 0.937255 0.956863 1
nord7 = Color 0.560784 0.737255 0.733333 1
nord8 = Color 0.533333 0.752941 0.815686 1
nord9 = Color 0.505882 0.631373 0.756863 1
nord10 = Color 0.368627 0.505882 0.674510 1
nord11 = Color 0.749020 0.380392 0.415686 1
nord12 = Color 0.815686 0.529412 0.439216 1
nord13 = Color 0.921569 0.796078 0.545098 1
nord14 = Color 0.639216 0.745098 0.549020 1
nord15 = Color 0.705882 0.556863 0.678431 1

{-# NOINLINE config #-}
config :: IORef Config
config = unsafePerformIO . newIORef $ Config
  { face = FaceID
    { sizePx = 24
    -- , path = "/nix/store/4c819hv4pvz4l37yxf391mwwvwdhvia9-source-han-serif-2.003/share/fonts/opentype/source-han-serif/SourceHanSerif.ttc"
    -- , index = 17

    , path = "/nix/store/569nxifmwb4r26phghxyn4xszdg7xjxm-source-han-sans-2.004/share/fonts/opentype/source-han-sans/SourceHanSans.ttc"
    , index = 27

    -- , path = "/nix/store/vl44mgyhq46plr28vfj06zj9lk89jyaw-liberation-fonts-2.1.5/share/fonts/truetype/LiberationSans-Regular.ttf"
    -- , path = "/nix/store/hibcvpqv3w7s7fpl3wgd8c33hs0znywq-Iosevka-33.2.3/share/fonts/truetype/Iosevka-ExtendedMedium.ttf"
    -- , path = "/nix/store/46g6p6698lc50ypik6mgg0wf3q23gzqz-dejavu-fonts-2.37/share/fonts/truetype/DejaVuSansMono.ttf"
    -- , index = 0
    }

  , foreground = nord6
  , background = nord0 -- { alpha = 0.8 }
  , primarySelectionForeground = nord0
  , primarySelectionBackground = nord10
  , primaryCursorForeground = nord0
  , primaryCursorBackground = nord8
  , cursorVerticalRangeOnScreen = (0.1, 0.9)
  , cursorHorizontalRangeOnScreen = (0.1, 0.9)
  , lineNumbersForeground = nord3
  , lineNumbersBackground = Color 0 0 0 0
  , lineNumbersCurrentForeground = nord0
  , lineNumbersCurrentBackground = nord3
  , barBackground = nord3
  , barForeground = nord6
  , tilingMargin = 10
  }
