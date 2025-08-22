{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms #-}

module Window
  ( Resolution(..)
  , Window(..)
  , WindowID
  , WindowManager(..)
  , DefaultWindowManager
  , newDefaultWindowManager

  -- demo
  , TimeWindow(..)
  , flush
  ) where

import GL
  ( arrayBufferSlot
  , bindProgram
  , checkGLError
  , renderToTexture
  , texture2DSlot
  , vertexArraySlot
  , withObject
  , withProgram
  , withSlot
  , writeArrayBuffer
  , GLObject(deleteObject)
  , Texture
  )
import qualified Data.IntMap as IntMap
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Weaver (drawText, setResolution, withWeaver)
import Config (Config)
import Graphics.GL
  ( pattern GL_COLOR_BUFFER_BIT
  , pattern GL_FALSE
  , pattern GL_FLOAT
  , pattern GL_TEXTURE_2D
  , pattern GL_TRIANGLE_STRIP
  , glClear
  , glClearColor
  , glDrawArrays
  , glEnableVertexAttribArray
  , glGenerateMipmap
  , glGetUniformLocation
  , glUniform1i
  , glVertexAttribPointer
  )
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.C (withCAString)
import qualified Data.ByteString.Char8 as BS

data Resolution = Resolution { resHori :: Int, resVert :: Int }

class Window a where
  drawWindow :: Resolution -> a -> IO Texture

newtype WindowID = WindowID Int

class WindowManager a where
  registerWindow :: Window w => w -> a -> IO WindowID
  needRedraw :: WindowID -> a -> IO ()

data DefaultWindowManager = DefaultWindowManager
  { dwmWindows :: IORef (IntMap.IntMap WindowInfo)
  }

data WindowInfo = forall w. Window w => WindowInfo
  { wiWindow :: w
  , wiNeedRedraw :: Bool
  }

newDefaultWindowManager :: IO DefaultWindowManager
newDefaultWindowManager = DefaultWindowManager <$> newIORef IntMap.empty

instance WindowManager DefaultWindowManager where
  registerWindow w wm = do
    wid <- IntMap.size <$> readIORef (dwmWindows wm)
    modifyIORef (dwmWindows wm) (IntMap.insert wid (WindowInfo w False))
    pure (WindowID wid)
  needRedraw (WindowID wid) wm = do
    modifyIORef (dwmWindows wm) (IntMap.alter (\case { Just wi -> Just wi { wiNeedRedraw = True }; Nothing -> Nothing; }) wid)

flush :: Resolution -> DefaultWindowManager -> IO ()
flush (Resolution w h) dwm = do
  [(0, onlyWin)] <- IntMap.assocs <$> readIORef (dwmWindows dwm)
  tex <- case onlyWin of
    WindowInfo win _ -> drawWindow (Resolution w h) win
  withProgram dtVs Nothing dtFs $ \prog -> bindProgram prog $ do
    withSlot texture2DSlot tex $ do
      withObject $ \vao -> withSlot vertexArraySlot vao $ do
        withObject $ \vbo -> withSlot arrayBufferSlot vbo $ do
          writeArrayBuffer
            [ -0.9,  0.9, 0, 1
            ,  0.9,  0.9, 1, 1
            , -0.9, -0.9, 0, 0
            ,  0.9, -0.9, 1, 0
            ]
          glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE 16 nullPtr
          glEnableVertexAttribArray 0
          glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE 16 (plusPtr nullPtr 8)
          glEnableVertexAttribArray 1
          texVar <- withCAString "tex" (glGetUniformLocation prog)
          glUniform1i texVar 0
          glDrawArrays GL_TRIANGLE_STRIP 0 4
  deleteObject tex
  where
    dtVs = Just . BS.unlines $
      [ "#version 330 core"
      , "layout (location = 0) in vec2 v_pos;"
      , "layout (location = 1) in vec2 v_tex_coord;"
      , "out vec2 f_tex_coord;"
      , "void main() {"
      , "  gl_Position = vec4(v_pos, 0, 1);"
      , "  f_tex_coord = v_tex_coord;"
      , "}"
      ]
    dtFs = Just . BS.unlines $
      [ "#version 330 core"
      , "in vec2 f_tex_coord;"
      , "uniform sampler2D tex;"
      , "out vec4 color;"
      , "void main() {"
      , "  color = vec4(texture(tex, f_tex_coord).rgb, 1);"
      , "}"
      ]


data TimeWindow = forall w. WindowManager w => TimeWindow WindowID w Config

instance Window TimeWindow where
  drawWindow (Resolution w h) (TimeWindow wid wm cfg) = do
    renderToTexture w h $ do
      glClearColor 0.7 0.2 0.2 1
      glClear GL_COLOR_BUFFER_BIT
      withWeaver cfg $ \weaver -> do
        setResolution w h weaver
        drawText weaver (w `div` 2) (-h `div` 2) "Hahaha"
