{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms #-}

module Window
  ( Window(..)
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
  , renderToTexture
  , texture2DSlot
  , vertexArraySlot
  , withObject
  , withProgram
  , withSlot
  , writeArrayBuffer
  , GLObject(deleteObject)
  , Resolution(..)
  , screenCoordToGL
  )
import qualified Data.IntMap as IntMap
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Weaver (drawText, withWeaver)
import Config (Config(..))
import Graphics.GL
  ( pattern GL_COLOR_BUFFER_BIT
  , pattern GL_FALSE
  , pattern GL_FLOAT
  , pattern GL_TRIANGLE_STRIP
  , glClear
  , glClearColor
  , glDrawArrays
  , glEnableVertexAttribArray
  , glGetUniformLocation
  , glUniform1i
  , glVertexAttribPointer
  )
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.C (withCAString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forM_)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Weaver (drawText, withWeaver, getLineHeight, getDescender)

class Window a where
  drawWindow :: Resolution -> a -> IO ()

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
flush res@(Resolution w h) dwm = do
  [(0, onlyWin)] <- IntMap.assocs <$> readIORef (dwmWindows dwm)
  let texRes = Resolution (w - 99 * 2) (h - 99 * 2)
  tex <- case onlyWin of
    WindowInfo win _ -> renderToTexture texRes (drawWindow texRes win)
  withProgram dtVs Nothing dtFs $ \prog -> bindProgram prog $ do
    withSlot texture2DSlot tex $ do
      withObject $ \vao -> withSlot vertexArraySlot vao $ do
        withObject $ \vbo -> withSlot arrayBufferSlot vbo $ do
          let
            (topLeftX    , topLeftY    ) = screenCoordToGL res 100           100
            (topRightX   , topRightY   ) = screenCoordToGL res (w - 1 - 100) 100
            (bottomLeftX , bottomLeftY ) = screenCoordToGL res 100           (h - 1 - 100)
            (bottomRightX, bottomRightY) = screenCoordToGL res (w - 1 - 100) (h - 1 - 100)
          writeArrayBuffer
            [ topLeftX    , topLeftY    , 0, 1
            , topRightX   , topRightY   , 1, 1
            , bottomLeftX , bottomLeftY , 0, 0
            , bottomRightX, bottomRightY, 1, 0
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
  drawWindow res@(Resolution w h) (TimeWindow wid wm cfg) = do
    glClearColor 0.2 0.2 0.2 1
    glClear GL_COLOR_BUFFER_BIT
    -- forM_ [16, 32, 64, 128] $ \s -> do
    --   withWeaver cfg { configFontSizePx = s, configForeground = (0, 0, 0) } $ \weaver -> do
    --     drawText weaver (Resolution w h) (w `div` 2) (h `div` 2 + 2 * s - 1) "haha"
    --   withWeaver cfg { configFontSizePx = s } $ \weaver -> do
    --     drawText weaver (Resolution w h) (w `div` 2) (h `div` 2 + 2 * s) "haha"
    withWeaver cfg $ \weaver -> do
      height <- getLineHeight weaver
      descender <- getDescender weaver
      Text.lines <$> Text.readFile "./app/Weaver.hs" >>= \lns -> forM_ (zip [1 ..] lns) $ \(idx, ln) -> do
        drawText weaver res 5 (height * idx + descender) (Text.pack (show idx))
        drawText weaver res 50 (height * idx + descender) ln
      -- drawText weaver 30 height "file is filling the office."
      -- drawText weaver res 30 height "OPPO回应苹果起诉员工窃密：并未侵犯苹果公司商业秘密，相信公正的司法审理能够澄清事实。"
