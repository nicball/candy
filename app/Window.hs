module Window
  ( Scroll(..)
  , SendKey(..)
  , SendChar(..)
  , Draw(..)
  , Box(..)
  , AnyBox(..)
  , GetBox(..)
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
import Selection (Selections)
import Data.Text (Text)
import Document (Document)

class Scroll a where
  scroll :: Double -> Double -> a -> IO ()

class SendKey a where
  sendKey :: GLFW.Key -> GLFW.ModifierKeys -> a -> IO ()

class SendChar a where
  sendChar :: Char -> a -> IO ()

class Draw a where
  draw :: Resolution -> a -> IO ()

class Box a where
  minimumSize :: a -> Resolution
  expandableX :: a -> Bool
  expandableY :: a -> Bool
  safeDraw :: Resolution -> a -> IO ()

data AnyBox = forall b. Box b => AnyBox b

instance Box AnyBox where
  minimumSize (AnyBox b) = minimumSize b
  expandableX (AnyBox b) = expandableX b
  expandableY (AnyBox b) = expandableY b
  safeDraw res (AnyBox b) = safeDraw res b

class GetBox a where
  getBox :: a -> IO AnyBox

class (Scroll a, SendKey a, SendChar a, Draw a) => WindowManager a where
  addEditorWindow :: EditorWindow w => w -> a -> IO ()
  setBar :: Bar w => w -> a -> IO ()

class (Scroll a, SendKey a, SendChar a, GetBox a) => EditorWindow a where
  new :: Document -> IO a
  getDocument :: a -> IO Document
  fork :: a -> IO a
  close :: a -> IO ()
  getStatus :: a -> IO Status

class GetBox a => Bar a where
  setStatus :: a -> Status -> IO ()

data Status = Status
  { selections :: Selections
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
