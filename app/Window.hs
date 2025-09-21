module Window
  ( Scroll(..)
  , SendKey(..)
  , SendChar(..)
  , Draw(..)
  , WindowManager(..)
  , WindowID(..)
  , EditorWindow(..)
  , Status(..)
  , Mode(..)
  , Bar(..)
  ) where

import Graphics.UI.GLFW qualified as GLFW
import GL (Resolution)
import Selection (Selection)

class Scroll a where
  scroll :: Double -> Double -> a -> IO ()

class SendKey a where
  sendKey :: GLFW.Key -> GLFW.ModifierKeys -> a -> IO ()

class SendChar a where
  sendChar :: Char -> a -> IO ()

class Draw a where
  draw :: Resolution -> a -> IO ()

newtype WindowID = WindowID Int

class (Scroll a, SendKey a, SendChar a, Draw a) => WindowManager a where
  registerEditorWindow :: EditorWindow w => w -> a -> IO WindowID
  setBar :: Bar w => w -> a -> IO ()

class (Scroll a, SendKey a, SendChar a, Draw a) => EditorWindow a where
  -- fromPath :: FilePath -> IO a
  -- fork :: a -> IO a
  -- close :: a -> IO ()
  getStatus :: a -> IO Status

class Draw a => Bar a where
  setStatus :: a -> Status -> IO ()

data Status = Status
  { selection :: Selection
  , mode :: Mode
  }

data Mode = NormalMode | InsertMode
  deriving (Eq, Show)
