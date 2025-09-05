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
  { dwmWindows :: IORef (IntMap.IntMap WindowInfo)
  , dwmQuadRenderer :: QuadRenderer
  }

data WindowInfo = forall w. Window w => WindowInfo
  { _wiWindow :: w
  , wiNeedRedraw :: Bool
  }

withDefaultWindowManager :: (DefaultWindowManager -> IO a) -> IO a
withDefaultWindowManager action = do
  dwmWindows <- newIORef IntMap.empty
  withQuadRenderer \dwmQuadRenderer -> do
    action DefaultWindowManager {..}

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
      drawQuadTexture (dwmQuadRenderer dwm) res tex margin (h - 1 - margin) margin (w - 1 - margin)
  where _ ... a = a

data DemoWindow = DemoWindow
  { dwScrollPos :: IORef Double
  , dwWeaver :: Weaver
  , dwDocument :: Document
  , dwSelection :: IORef Selection
  , dwConfig :: Config
  , dwQuadRenderer :: QuadRenderer
  }

withDemoWindow :: Config -> (DemoWindow -> IO a) -> IO a
withDemoWindow dwConfig action = do
  dwScrollPos <- newIORef 0
  dwDocument <- Document.fromText <$> Text.readFile "./app/Weaver.hs"
  dwSelection <- newIORef (Selection (Coord 0 0) (Coord 0 0))
  withWeaver dwConfig \dwWeaver -> do
    withQuadRenderer \dwQuadRenderer -> do
      action DemoWindow {..}

instance Scroll DemoWindow where
  scroll _ y DemoWindow{..} = modifyIORef dwScrollPos (min 0 . (+ y))

instance SendKey DemoWindow where
  sendKey key mods DemoWindow{..} = do
    case key of
      GLFW.Key'H -> modifyIORef dwSelection (ext Selection.moveLeft dwDocument)
      GLFW.Key'L -> modifyIORef dwSelection (ext Selection.moveRight dwDocument)
      GLFW.Key'E -> modifyIORef dwSelection (ext Selection.selectToWordEnd dwDocument)
      GLFW.Key'W -> modifyIORef dwSelection (ext Selection.selectToWordStart dwDocument)
      _ -> pure ()
    where ext m = if GLFW.modifierKeysShift mods then Selection.extend m else m

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
    let
      linePos idx = height * (idx - beginLine + 1) + descender
      numLines = resVert res `divSat` height
      divSat a b = let (r, m) = divMod a b in if m /= 0 then r + 1 else r
      sanitize ln = if "\n" `Text.isSuffixOf` ln then (Text.dropEnd 1 ln <> " ") else ln
    sel <- readIORef dwSelection
    let selRanges = Selection.selLines dwDocument sel
    forM_ [ beginLine, beginLine + 1 .. min (beginLine + numLines) (Document.countLines dwDocument - 1) ] \idx -> do
      let y = linePos idx
      let ln = sanitize . Document.getLine idx $ dwDocument
      let selRange = filter (\(l, _, _) -> l == idx) selRanges
      let selColorSpec = fmap
            (\(_, begin, end) ->
              ( begin
              , end + Document.charLengthAt dwDocument (Coord idx end)
              , configPrimarySelectionForeground dwConfig
              ))
            selRange
      let cursorRange = if idx /= coordLine (selMark sel) then [] else [coordColumn (selMark sel)]
      let cursorColorSpec = fmap
            (\pos ->
              ( pos
              , pos + Document.charLengthAt dwDocument (Coord idx pos)
              , configPrimaryCursorForeground dwConfig
              ))
            cursorRange
      let quads = fmap (\(_, begin, end) -> (begin, end, configPrimarySelectionBackground dwConfig)) selRange
            ++ fmap (\pos -> (pos, pos, configPrimaryCursorBackground dwConfig)) cursorRange
      forM_ quads \(begin, end, color) -> do
        let getMid (_, x, _) = x
        rq <- layoutTextCached (configFace dwConfig) ln
        xStart <- if begin == 0 then pure 0 else getMid <$> Raqm.indexToPosition rq (begin - 1)
        xEnd <- getMid <$> Raqm.indexToPosition rq end
        drawQuadColor dwQuadRenderer res color (y - descender - height) (y - descender) (50 + xStart) (50 + xEnd)
      drawText dwWeaver res 5 y [(0, 100, configForeground dwConfig)] (Text.pack (show idx))
      -- let rainbow = fmap (\(n, c) -> (n, n + 1, c)) . zip [0 ..] . cycle $ [ Color 0.93 0.94 0.96 1, Color 0.53 0.75 0.82 1, Color 0.37 0.51 0.67 1, Color 0.75 0.38 0.42 1, Color 0.86 0.53 0.44 1, Color 0.92 0.80 0.55 1, Color 0.64 0.75 0.55 1, Color 0.71 0.56 0.68 1 ]
      drawText dwWeaver res 50 y (cursorColorSpec ++ selColorSpec ++ [(0, Text.lengthWord8 ln, configForeground dwConfig)]) ln
      -- drawText dwWeaver res 50 y rainbow content
    -- drawText dwWeaver res 30 (height + descender) "file is filling the office."
    -- drawText dwWeaver res 30 (height * 2 + descender) "OPPO回应苹果起诉员工窃密：并未侵犯苹果公司商业秘密，相信公正的司法审理能够澄清事实。"
