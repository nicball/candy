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
  , Resolution(..)
  , pixelQuadToNDC
  )
import qualified GL
import qualified Data.IntMap as IntMap
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Weaver (drawText, withWeaver)
import Config (Config(..), FaceID(..), Color(..))
import Graphics.GL
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.C (withCAString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forM_)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Foreign as Text
import Weaver (Weaver, getLineHeight, getDescender, layoutTextCached)
import qualified Data.Text.ICU as ICU
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (when)
import qualified Raqm
import Document (Document, Coord(..))
import qualified Document

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
  { _wiWindow :: w
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
  , dwDocument :: Document
  , dwCursorPos :: IORef Coord
  , dwConfig :: Config
  , dwCursor :: Cursor
  }

withDemoWindow :: Config -> (DemoWindow -> IO a) -> IO a
withDemoWindow dwConfig action = do
  dwScrollPos <- newIORef 0
  dwDocument <- Document.fromText <$> Text.readFile "./app/Weaver.hs"
  dwCursorPos <- newIORef (Coord 0 0)
  withWeaver dwConfig \dwWeaver -> do
    withCursor dwConfig \dwCursor -> do
      action DemoWindow {..}

instance Scroll DemoWindow where
  scroll _ y DemoWindow{..} = modifyIORef dwScrollPos (min 0 . (+ y))

instance SendKey DemoWindow where
  sendKey key mods DemoWindow{..} = do
    case key of
      GLFW.Key'L -> modifyIORef dwCursorPos (Document.moveCoord dwDocument (scale 1))
      GLFW.Key'H -> modifyIORef dwCursorPos (Document.moveCoord dwDocument (scale (-1)))
      _ -> pure ()
    where scale x = if GLFW.modifierKeysControl mods then x * 100 else x

instance Window DemoWindow where
  drawWindow res DemoWindow{..} = do
    let Color{..} = configBackground dwConfig in
      glClearColor colorRed colorGreen colorBlue colorAlpha
    glClear GL_COLOR_BUFFER_BIT
    -- forM_ [16, 32, 64, 128] \s -> do
    --   withWeaver cfg { configFontSizePx = s, configForeground = (0, 0, 0) } \weaver -> do
    --     drawText weaver (Resolution w h) (w `div` 2) (h `div` 2 + 2 * s - 1) "haha"
    --   withWeaver cfg { configFontSizePx = s } \weaver -> do
    --     drawText weaver (Resolution w h) (w `div` 2) (h `div` 2 + 2 * s) "haha"
    height <- getLineHeight dwWeaver
    descender <- getDescender dwWeaver
    beginLine <- negate . truncate <$> readIORef dwScrollPos
    let linePos idx = height * (idx - beginLine + 1) + descender
        numLines = resVert res `divSat` height
        divSat a b = let (r, m) = divMod a b in if m /= 0 then r + 1 else r
    Coord cln ccol <- readIORef dwCursorPos
    forM_ [ beginLine, beginLine + 1 .. min (beginLine + numLines) (Document.countLines dwDocument - 1) ] \idx -> do
      let y = linePos idx
      let ln = Document.getLine idx dwDocument
      let content = Text.dropWhileEnd (== '\n') ln
      let getMid (_, x, _) = x
      when (idx == cln) do
        rq <- layoutTextCached (configFace dwConfig) content
        xStart <- if ccol == 0 then pure 0 else getMid <$> Raqm.indexToPosition rq (ccol - 1)
        xEnd <- if ccol == Text.lengthWord8 ln - 1 then pure (xStart + (faceIDSizePx (configFace dwConfig) `div` 2)) else getMid <$> Raqm.indexToPosition rq ccol
        drawCursor res dwCursor (y - descender - height) (y - descender) (50 + xStart) (50 + xEnd)
      let charLen = Text.lengthWord8 . ICU.brkBreak . (!! 0) . ICU.breaks (ICU.breakCharacter ICU.Current) . Text.dropWord8 (fromIntegral ccol) $ ln
      drawText dwWeaver res 5 y [(0, 100, configForeground dwConfig)] (Text.pack (show idx))
      let addCursorColorSpec = if idx == cln then ((ccol, ccol + charLen, configPrimaryCursorForeground dwConfig) :) else id
      -- let rainbow = fmap (\(n, c) -> (n, n + 1, c)) . zip [0 ..] . cycle $ [ Color 0.93 0.94 0.96 1, Color 0.53 0.75 0.82 1, Color 0.37 0.51 0.67 1, Color 0.75 0.38 0.42 1, Color 0.86 0.53 0.44 1, Color 0.92 0.80 0.55 1, Color 0.64 0.75 0.55 1, Color 0.71 0.56 0.68 1 ]
      drawText dwWeaver res 50 y (addCursorColorSpec [(0, Text.lengthWord8 content, configForeground dwConfig)]) content
      -- drawText dwWeaver res 50 y rainbow content
    -- drawText dwWeaver res 30 (height + descender) "file is filling the office."
    -- drawText dwWeaver res 30 (height * 2 + descender) "OPPO回应苹果起诉员工窃密：并未侵犯苹果公司商业秘密，相信公正的司法审理能够澄清事实。"

data Cursor = Cursor
  { cursorProg :: GLuint
  , cursorVAO :: GL.VertexArray
  , cursorVBO :: GL.Buffer
  }

withCursor :: Config -> (Cursor -> IO a) -> IO a
withCursor config action =
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
    fs =
      let Color{..} = configPrimaryCursorBackground config
      in Just . BS.unlines $
        [ "#version 330 core"
        , "out vec4 color;"
        , "void main() {"
        , "  color = vec4(" <> BS.pack (show colorRed) <> ", " <> BS.pack (show colorGreen) <> ", " <> BS.pack (show colorBlue) <> ", " <> BS.pack (show colorAlpha) <> ");"
        , "}"
        ]

drawCursor :: Resolution -> Cursor -> Int -> Int -> Int -> Int -> IO ()
drawCursor res Cursor{..} yStart yEnd xStart xEnd = do
  bindProgram cursorProg do
    withSlot vertexArraySlot cursorVAO do
      withSlot arrayBufferSlot cursorVBO do
        writeArrayBuffer . pixelQuadToNDC res $
          ( (xStart, yStart)
          , (xEnd, yStart)
          , (xStart, yEnd)
          , (xEnd, yEnd)
          )
        glDrawArrays GL_TRIANGLE_STRIP 0 4
