module Window
  ( Window(..)
  , WindowID
  , Scroll(..)
  , WindowManager(..)
  , DefaultWindowManager
  , withDefaultWindowManager

  -- demo
  , DemoWindow(..)
  , withDemoWindow
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
  , screenCoordToNDC
  )
import qualified GL
import qualified Data.IntMap as IntMap
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Weaver (drawText, withWeaver)
import Config (Config(..))
import Graphics.GL
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.C (withCAString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forM_)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Weaver (Weaver, drawText, withWeaver, getLineHeight, getDescender)

class Scroll a where
  scroll :: Double -> Double -> a -> IO ()

class Scroll a => Window a where
  drawWindow :: Resolution -> a -> IO ()

newtype WindowID = WindowID Int

class Scroll a => WindowManager a where
  registerWindow :: Window w => w -> a -> IO WindowID
  needRedraw :: WindowID -> a -> IO ()

data DefaultWindowManager = DefaultWindowManager
  { dwmWindows :: IORef (IntMap.IntMap WindowInfo)
  , dwmProg :: GLuint
  , dwmVAO :: GL.VertexArray
  , dwmVBO :: GL.Buffer
  }

data WindowInfo = forall w. Window w => WindowInfo
  { wiWindow :: w
  , wiNeedRedraw :: Bool
  }

withDefaultWindowManager :: (DefaultWindowManager -> IO a) -> IO a
withDefaultWindowManager action =
  withProgram dtVs Nothing dtFs \dwmProg -> do
    dwmWindows <- newIORef IntMap.empty
    withObject \dwmVAO -> do
      withObject \dwmVBO -> do
        withSlot arrayBufferSlot dwmVBO do
          withSlot vertexArraySlot dwmVAO do
            glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE 16 nullPtr
            glEnableVertexAttribArray 0
            glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE 16 (plusPtr nullPtr 8)
            glEnableVertexAttribArray 1
        action DefaultWindowManager {..}
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
      , "  color = texture(tex, f_tex_coord);"
      , "}"
      ]

instance Scroll DefaultWindowManager where
  scroll x y dwm = readIORef (dwmWindows dwm) >>= mapM_ (\case WindowInfo w _ -> scroll x y w) . IntMap.elems

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
  case onlyWin of
    WindowInfo win _ -> renderToTexture texRes (drawWindow texRes win) \tex -> do
      bindProgram (dwmProg dwm) do
        withSlot texture2DSlot tex do
          withSlot vertexArraySlot (dwmVAO dwm) do
            withSlot arrayBufferSlot (dwmVBO dwm) do
              let
                (topLeftX    , topLeftY    ) = screenCoordToNDC res 100           100
                (topRightX   , topRightY   ) = screenCoordToNDC res (w - 1 - 100) 100
                (bottomLeftX , bottomLeftY ) = screenCoordToNDC res 100           (h - 1 - 100)
                (bottomRightX, bottomRightY) = screenCoordToNDC res (w - 1 - 100) (h - 1 - 100)
              writeArrayBuffer
                [ topLeftX    , topLeftY    , 0, 1
                , topRightX   , topRightY   , 1, 1
                , bottomLeftX , bottomLeftY , 0, 0
                , bottomRightX, bottomRightY, 1, 0
                ]
              texVar <- withCAString "tex" (glGetUniformLocation (dwmProg dwm))
              glUniform1i texVar 0
              glDrawArrays GL_TRIANGLE_STRIP 0 4

data DemoWindow = DemoWindow
  { dwScrollPos :: IORef Double
  , dwWeaver :: Weaver
  }

withDemoWindow :: Config -> (DemoWindow -> IO a) -> IO a
withDemoWindow config action = do
  dwScrollPos <- newIORef 0
  withWeaver config \dwWeaver -> do
    action DemoWindow {..}

instance Scroll DemoWindow where
  scroll _ y DemoWindow{..} = modifyIORef dwScrollPos (min 0 . (+ y))

instance Window DemoWindow where
  drawWindow res DemoWindow{..} = do
    glClearColor 0.2 0.2 0.2 1
    glClear GL_COLOR_BUFFER_BIT
    -- forM_ [16, 32, 64, 128] \s -> do
    --   withWeaver cfg { configFontSizePx = s, configForeground = (0, 0, 0) } \weaver -> do
    --     drawText weaver (Resolution w h) (w `div` 2) (h `div` 2 + 2 * s - 1) "haha"
    --   withWeaver cfg { configFontSizePx = s } \weaver -> do
    --     drawText weaver (Resolution w h) (w `div` 2) (h `div` 2 + 2 * s) "haha"
    height <- getLineHeight dwWeaver
    descender <- getDescender dwWeaver
    offset <- (* height) . truncate <$> readIORef dwScrollPos
    let linePos idx = height * idx + descender + offset
        onScreen pos = pos >= (height * (-5)) && pos <= (resVert res + height * 5)
        filter' p = takeWhile p . dropWhile (not . p)
    Text.lines <$> Text.readFile "./app/Weaver.hs" >>= \lns -> forM_ (filter' (onScreen . linePos . fst) . zip [1 ..] $ lns) \(idx, ln) -> do
      let y = linePos idx
      drawText dwWeaver res 5 y (Text.pack (show idx))
      drawText dwWeaver res 50 y ln
    -- drawText weaver res 30 height "file is filling the office."
    -- drawText weaver res 30 (height * 2) "OPPO回应苹果起诉员工窃密：并未侵犯苹果公司商业秘密，相信公正的司法审理能够澄清事实。"
