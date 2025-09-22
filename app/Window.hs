module Window
  ( Scroll(..)
  , SendKey(..)
  , SendChar(..)
  , Draw(..)
  , WindowManager(..)
  , EditorWindow(..)
  , Status(..)
  , Mode(..)
  , Bar(..)
  , pattern GMKNone
  , pattern GMKCtrl
  , pattern GMKShift
  , pattern GMKAlt
  ) where

import Graphics.UI.GLFW qualified as GLFW
import GL (Resolution)
import Selection (Selection)
import Data.Text (Text)

class Scroll a where
  scroll :: Double -> Double -> a -> IO ()

class SendKey a where
  sendKey :: GLFW.Key -> GLFW.ModifierKeys -> a -> IO ()

class SendChar a where
  sendChar :: Char -> a -> IO ()

class Draw a where
  draw :: Resolution -> a -> IO ()

class (Scroll a, SendKey a, SendChar a, Draw a) => WindowManager a where
  addEditorWindow :: EditorWindow w => w -> a -> IO ()
  setBar :: Bar w => w -> a -> IO ()

class (Scroll a, SendKey a, SendChar a, Draw a) => EditorWindow a where
  fromPath :: FilePath -> IO a
  fork :: a -> IO a
  close :: a -> IO ()
  getStatus :: a -> IO Status

class Draw a => Bar a where
  setStatus :: a -> Status -> IO ()

data Status = Status
  { selection :: Selection
  , mode :: Mode
  , name :: Text
  }

data Mode = NormalMode | InsertMode
  deriving (Eq, Show)

pattern GMKNone :: GLFW.ModifierKeys
pattern GMKNone <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = False, modifierKeysAlt = False }

pattern GMKShift :: Bool -> GLFW.ModifierKeys
pattern GMKShift s <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = s, modifierKeysAlt = False }

pattern GMKAlt :: Bool -> GLFW.ModifierKeys
pattern GMKAlt s <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = False, modifierKeysAlt = s }

pattern GMKCtrl :: Bool -> GLFW.ModifierKeys
pattern GMKCtrl s <- GLFW.ModifierKeys { modifierKeysControl = s, modifierKeysShift = False, modifierKeysAlt = False }
