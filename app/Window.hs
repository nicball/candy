module Window
  ( Window(..)
  , WindowID
  , Scroll(..)
  , SendKey(..)
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
  , pixelQuadToNDC
  )
import qualified GL
import qualified Data.IntMap as IntMap
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Weaver (drawText, withWeaver)
import Config (Config(..))
import Graphics.GL
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.C (withCAString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forM_)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Foreign as Text
import Weaver (Weaver, drawText, withWeaver, getLineHeight, getDescender, layoutTextCached)
import qualified Data.Text.ICU as ICU
import qualified Data.Text.ICU.Break as ICUIO
import qualified Graphics.UI.GLFW as GLFW
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad (when)
import qualified Raqm
import qualified Foreign.Marshal.Array as C

class Scroll a where
  scroll :: Double -> Double -> a -> IO ()

class SendKey a where
  sendKey :: GLFW.Key -> GLFW.ModifierKeys -> a -> IO ()

class (Scroll a, SendKey a) => Window a where
  drawWindow :: Resolution -> a -> IO ()

newtype WindowID = WindowID Int

class (Scroll a, SendKey a) => WindowManager a where
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
            glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE 8 nullPtr
            glEnableVertexAttribArray 0
            glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE 8 (plusPtr nullPtr 32)
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

instance SendKey DefaultWindowManager where
  sendKey key mods dwm = readIORef (dwmWindows dwm) >>= mapM_ (\case WindowInfo w _ -> sendKey key mods w) . IntMap.elems

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
  let margin = 20
  let texRes = Resolution (w - margin * 2) (h - margin * 2)
  case onlyWin of
    WindowInfo win _ -> drawWindow res win ... renderToTexture texRes (drawWindow texRes win) \tex -> do
      bindProgram (dwmProg dwm) do
        withSlot texture2DSlot tex do
          withSlot vertexArraySlot (dwmVAO dwm) do
            withSlot arrayBufferSlot (dwmVBO dwm) do
              writeArrayBuffer $
                pixelQuadToNDC res
                  ( (margin, margin)
                  , (w - 1 - margin, margin)
                  , (margin, h - 1 - margin)
                  , (w - 1 - margin, h - 1 - margin)
                  )
                ++
                [ 0, 1
                , 1, 1
                , 0, 0
                , 1, 0
                ]
              texVar <- withCAString "tex" (glGetUniformLocation (dwmProg dwm))
              glUniform1i texVar 0
              glDrawArrays GL_TRIANGLE_STRIP 0 4
  where _ ... a = a

data DemoWindow = DemoWindow
  { dwScrollPos :: IORef Double
  , dwWeaver :: Weaver
  , dwText :: Text.Text
  , dwBreakIter :: ICUIO.BreakIterator ()
  , dwCursorPos :: IORef Text.I8
  , dwConfig :: Config
  , dwCursor :: Cursor
  }

withDemoWindow :: Config -> (DemoWindow -> IO a) -> IO a
withDemoWindow dwConfig action = do
  dwScrollPos <- newIORef 0
  dwText <- Text.readFile "./app/Weaver.hs"
  dwBreakIter <- ICUIO.breakCharacter ICU.Current dwText
  dwCursorPos <- newIORef 0
  withWeaver dwConfig \dwWeaver -> do
    withCursor \dwCursor -> do
      action DemoWindow {..}

instance Scroll DemoWindow where
  scroll _ y DemoWindow{..} = modifyIORef dwScrollPos (min 0 . (+ y))

instance SendKey DemoWindow where
  sendKey key mods DemoWindow{..} = do
    when (mods == GLFW.ModifierKeys False False False False False False) do
      case key of
        GLFW.Key'L -> do
          pos <- readIORef dwCursorPos
          next <- maybe pos unsafeCoerce <$> ICUIO.following dwBreakIter (fromIntegral pos)
          writeIORef dwCursorPos next
        GLFW.Key'H -> do
          pos <- readIORef dwCursorPos
          next <- maybe pos unsafeCoerce <$> ICUIO.preceding dwBreakIter (fromIntegral pos)
          writeIORef dwCursorPos next
        _ -> pure ()

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
    screenOffset <- (* height) . truncate <$> readIORef dwScrollPos
    let linePos idx = height * idx + descender + screenOffset
        onScreen pos = pos >= (height * (-5)) && pos <= (resVert res + height * 5)
        filter' p = takeWhile p . dropWhile (not . p)
    forM_ (filter' (onScreen . linePos . fst) . zip [1 ..] . withLnOffset 0 . Text.lines $ dwText) \(idx, (lnOffset, ln)) -> do
      let y = linePos idx
      cursorPos <- fromIntegral <$> readIORef dwCursorPos
      let cursorPosInline = cursorPos - lnOffset
      when (lnOffset <= cursorPos && cursorPosInline < Text.lengthWord8 ln) do
        rq <- layoutTextCached (configFace dwConfig) ln
        (_, xStart, _) <- if cursorPosInline == 0 then pure (0, 0, 0) else Raqm.indexToPosition rq (cursorPosInline - 1)
        (_, xEnd, _) <- Raqm.indexToPosition rq cursorPosInline
        drawCursor res dwCursor y (50 + xStart) (50 + xEnd)
      drawText dwWeaver res 5 y (Text.pack (show idx))
      drawText dwWeaver res 50 y ln
    -- drawText dwWeaver res 30 (height + descender) "file is filling the office."
    -- drawText dwWeaver res 30 (height * 2 + descender) "OPPO回应苹果起诉员工窃密：并未侵犯苹果公司商业秘密，相信公正的司法审理能够澄清事实。"
    where
      withLnOffset _ [] = []
      withLnOffset o (x : xs) = (o, x) : withLnOffset (o + Text.lengthWord8 x + 1) xs

data Cursor = Cursor
  { cursorProg :: GLuint
  , cursorVAO :: GL.VertexArray
  , cursorVBO :: GL.Buffer
  }

withCursor :: (Cursor -> IO a) -> IO a
withCursor action =
  withProgram vs Nothing fs \cursorProg -> do
    withObject \cursorVAO -> do
      withObject \cursorVBO -> do
        withSlot arrayBufferSlot cursorVBO do
          withSlot vertexArraySlot cursorVAO do
            glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE 8 nullPtr
            glEnableVertexAttribArray 0
        action Cursor {..}
  where
    vs = Just . BS.unlines $
      [ "#version 330 core"
      , "layout (location = 0) in vec2 v_pos;"
      , "void main() {"
      , "  gl_Position = vec4(v_pos, 0, 1);"
      , "}"
      ]
    fs = Just . BS.unlines $
      [ "#version 330 core"
      , "out vec4 color;"
      , "void main() {"
      , "  color = vec4(0.54, 0.75, 0.81, 1);"
      , "}"
      ]

drawCursor :: Resolution -> Cursor -> Int -> Int -> Int -> IO ()
drawCursor (Resolution w h) Cursor{..} y xStart xEnd = do
  let
    ndcY = scaleY y
    ndcXStart = scaleX xStart
    ndcXEnd = scaleX xEnd
    scaleX p = -1 + (fromIntegral p + 0.5) * 2 / fromIntegral w
    scaleY p = 1 - (fromIntegral p + 0.5) * 2 / fromIntegral h
  bindProgram cursorProg do
    withSlot vertexArraySlot cursorVAO do
      withSlot arrayBufferSlot cursorVBO do
        forM_ [0 :: Int .. 4] \dy -> do
          let ndcDy = fromIntegral dy * 2 / fromIntegral h
          writeArrayBuffer [ ndcXStart, ndcY - ndcDy, ndcXEnd, ndcY - ndcDy ]
          glDrawArrays GL_LINES 0 2
