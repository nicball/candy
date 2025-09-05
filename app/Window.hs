{-# LANGUAGE MultiWayIf #-}

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

import Control.Monad (forM_)
import Data.IntMap qualified as IntMap
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Text.Foreign qualified as Text
import Data.Text.IO qualified as Text
import Data.Text qualified as Text
import Graphics.GL
import Graphics.UI.GLFW qualified as GLFW

import Config (Config(..), Color(..))
import Document (Document, Coord(..))
import Document qualified
import GL (drawQuadColor, drawQuadTexture, renderToTexture, withQuadRenderer, QuadRenderer, Resolution(..))
import Raqm qualified
import Selection qualified
import Selection (Selection(..))
import Weaver (Weaver, getLineHeight, getDescender, layoutTextCached, drawText, withWeaver)

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
  { windows :: IORef (IntMap.IntMap WindowInfo)
  , quadRenderer :: QuadRenderer
  }

data WindowInfo = forall w. Window w => WindowInfo
  { _window :: w
  , needRedraw :: Bool
  }

withDefaultWindowManager :: (DefaultWindowManager -> IO a) -> IO a
withDefaultWindowManager action = do
  windows <- newIORef IntMap.empty
  withQuadRenderer \quadRenderer -> do
    action DefaultWindowManager{..}

instance Scroll DefaultWindowManager where
  scroll x y dwm = readIORef dwm.windows >>= mapM_ (\case WindowInfo w _ -> scroll x y w) . IntMap.elems

instance SendKey DefaultWindowManager where
  sendKey key mods dwm = readIORef dwm.windows >>= mapM_ (\case WindowInfo w _ -> sendKey key mods w) . IntMap.elems

instance WindowManager DefaultWindowManager where
  registerWindow w dwm = do
    wid <- IntMap.size <$> readIORef dwm.windows
    modifyIORef dwm.windows (IntMap.insert wid (WindowInfo w False))
    pure (WindowID wid)
  needRedraw (WindowID wid) dwm = do
    modifyIORef dwm.windows (IntMap.alter (\case { Just wi -> Just wi { needRedraw = True }; Nothing -> Nothing; }) wid)

flush :: Resolution -> DefaultWindowManager -> IO ()
flush res@(Resolution w h) dwm = do
  [(0, onlyWin)] <- IntMap.assocs <$> readIORef dwm.windows
  let margin = 20
  let texRes = Resolution (w - margin * 2) (h - margin * 2)
  case onlyWin of
    WindowInfo win _ -> drawWindow res win ... renderToTexture texRes (drawWindow texRes win) \tex -> do
      drawQuadTexture dwm.quadRenderer res tex margin (h - 1 - margin) margin (w - 1 - margin)
  where _ ... a = a

data DemoWindow = DemoWindow
  { scrollPos :: IORef Double
  , weaver :: Weaver
  , document :: Document
  , selection :: IORef Selection
  , config :: Config
  , quadRenderer :: QuadRenderer
  }

withDemoWindow :: Config -> (DemoWindow -> IO a) -> IO a
withDemoWindow config action = do
  scrollPos <- newIORef 0
  document <- Document.fromText <$> Text.readFile "./app/Weaver.hs"
  selection <- newIORef (Selection (Coord 0 0) (Coord 0 0))
  withWeaver config \weaver -> do
    withQuadRenderer \quadRenderer -> do
      action DemoWindow{..}

instance Scroll DemoWindow where
  scroll _ y DemoWindow{scrollPos} = modifyIORef scrollPos (min 0 . (+ y))

instance SendKey DemoWindow where
  sendKey key mods DemoWindow{selection, document} = do
    case key of
      GLFW.Key'H -> modifyIORef selection (ext Selection.moveLeft document)
      GLFW.Key'L -> modifyIORef selection (ext Selection.moveRight document)
      GLFW.Key'E -> modifyIORef selection (ext Selection.selectToWordEnd document)
      GLFW.Key'W -> modifyIORef selection (ext Selection.selectToWordStart document)
      _ -> pure ()
    where ext m = if GLFW.modifierKeysShift mods then Selection.extend m else m

instance Window DemoWindow where
  drawWindow res dw = do
    let Color{..} = dw.config.background in
      glClearColor red green blue alpha
    glClear GL_COLOR_BUFFER_BIT
    -- forM_ [16, 32, 64, 128] \s -> do
    --   withWeaver cfg { configFontSizePx = s, configForeground = (0, 0, 0) } \weaver -> do
    --     drawText weaver (Resolution w h) (w `div` 2) (h `div` 2 + 2 * s - 1) "haha"
    --   withWeaver cfg { configFontSizePx = s } \weaver -> do
    --     drawText weaver (Resolution w h) (w `div` 2) (h `div` 2 + 2 * s) "haha"
    height <- getLineHeight dw.weaver
    descender <- getDescender dw.weaver
    beginLine <- negate . truncate <$> readIORef dw.scrollPos
    let
      linePos idx = height * (idx - beginLine + 1) + descender
      numLines = res.h `divSat` height
      divSat a b = let (r, m) = divMod a b in if m /= 0 then r + 1 else r
      sanitize ln = if "\n" `Text.isSuffixOf` ln then (Text.dropEnd 1 ln <> " ") else ln
    sel <- readIORef dw.selection
    let selRanges = Selection.selLines dw.document sel
    forM_ [ beginLine, beginLine + 1 .. min (beginLine + numLines) (Document.countLines dw.document - 1) ] \idx -> do
      let y = linePos idx
      let ln = sanitize . Document.getLine idx $ dw.document
      let selRange = filter (\(l, _, _) -> l == idx) selRanges
      let selColorSpec = fmap
            (\(_, begin, end) ->
              ( begin
              , end + Document.charLengthAt dw.document (Coord idx end)
              , dw.config.primarySelectionForeground
              ))
            selRange
      let cursorRange = if idx /= sel.mark.line then [] else [ sel.mark.column ]
      let cursorColorSpec = fmap
            (\pos ->
              ( pos
              , pos + Document.charLengthAt dw.document (Coord idx pos)
              , dw.config.primaryCursorForeground
              ))
            cursorRange
      let quads = fmap (\(_, begin, end) -> (begin, end, dw.config.primarySelectionBackground)) selRange
            ++ fmap (\pos -> (pos, pos, dw.config.primaryCursorBackground)) cursorRange
      forM_ quads \(begin, end, color) -> do
        let getMid (_, x, _) = x
        rq <- layoutTextCached dw.config.face ln
        xStart <- if begin == 0 then pure 0 else getMid <$> Raqm.indexToPosition rq (begin - 1)
        xEnd <- getMid <$> Raqm.indexToPosition rq end
        drawQuadColor dw.quadRenderer res color (y - descender - height) (y - descender) (50 + xStart) (50 + xEnd)
      drawText dw.weaver res 5 y [(0, 100, dw.config.foreground)] (Text.pack (show idx))
      -- let rainbow = fmap (\(n, c) -> (n, n + 1, c)) . zip [0 ..] . cycle $ [ Color 0.93 0.94 0.96 1, Color 0.53 0.75 0.82 1, Color 0.37 0.51 0.67 1, Color 0.75 0.38 0.42 1, Color 0.86 0.53 0.44 1, Color 0.92 0.80 0.55 1, Color 0.64 0.75 0.55 1, Color 0.71 0.56 0.68 1 ]
      drawText dw.weaver res 50 y (cursorColorSpec ++ selColorSpec ++ [(0, Text.lengthWord8 ln, dw.config.foreground)]) ln
      -- drawText dwWeaver res 50 y rainbow content
    -- drawText dwWeaver res 30 (height + descender) "file is filling the office."
    -- drawText dwWeaver res 30 (height * 2 + descender) "OPPO回应苹果起诉员工窃密：并未侵犯苹果公司商业秘密，相信公正的司法审理能够澄清事实。"
