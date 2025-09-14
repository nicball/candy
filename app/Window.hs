{-# LANGUAGE PatternSynonyms #-}

module Window
  ( Window(..)
  , WindowID
  , Scroll(..)
  , SendKey(..)
  , SendChar(..)
  , WindowManager(..)
  , DefaultWindowManager
  , withDefaultWindowManager

  -- demo
  , DemoWindow(..)
  , withDemoWindow
  , flush
  ) where

import Control.Monad (forM_, join, when)
import Data.IntMap qualified as IntMap
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (maybeToList)
import Data.Text.Foreign qualified as Text
import Data.Text.IO qualified as Text
import Data.Text qualified as Text
import Data.Text (Text)
import Graphics.GL
import Graphics.UI.GLFW qualified as GLFW

import Config (Config(..), Color(..), FaceID(..))
import Document (Document, Coord(..))
import Document qualified
import GL (drawQuadColor, drawQuadTexture, renderToTexture, withQuadRenderer, QuadRenderer, Resolution(..), getSlot, setSlot, Viewport(..), viewportSlot)
import Raqm qualified
import Selection qualified
import Selection (Selection(..))
import Weaver (Weaver, getLineHeight, getDescender, layoutTextCached, drawText, withWeaver)

class Scroll a where
  scroll :: Double -> Double -> a -> IO ()

class SendKey a where
  sendKey :: GLFW.Key -> GLFW.ModifierKeys -> a -> IO ()

class SendChar a where
  sendChar :: Char -> a -> IO ()

class (Scroll a, SendKey a, SendChar a) => Window a where
  drawWindow :: Resolution -> a -> IO ()

newtype WindowID = WindowID Int

class (Scroll a, SendKey a, SendChar a) => WindowManager a where
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

instance SendChar DefaultWindowManager where
  sendChar char dwm = readIORef dwm.windows >>= mapM_ (\case WindowInfo w _ -> sendChar char w) . IntMap.elems

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
    WindowInfo win _ -> renderToTexture texRes (drawWindow texRes win) \tex -> do
      drawQuadTexture dwm.quadRenderer res tex margin (h - 1 - margin) margin (w - 1 - margin)

data DemoWindow = DemoWindow
  { screenXPos :: IORef Int
  , screenYPos :: IORef Int
  , weaver :: Weaver
  , document :: IORef Document
  , mode :: IORef Mode
  , eatFirstChar :: IORef Bool
  , insertExitCallback :: IORef (IO ())
  , selection :: IORef Selection
  , config :: Config
  , quadRenderer :: QuadRenderer
  }

data Mode = Normal | Insert
  deriving Eq

withDemoWindow :: Config -> (DemoWindow -> IO a) -> IO a
withDemoWindow config action = do
  screenXPos <- newIORef 0
  screenYPos <- newIORef 0
  document <- newIORef . Document.fromText =<< Text.readFile "./app/Window.hs"
  selection <- newIORef (Selection (Coord 0 0) (Coord 0 0))
  mode <- newIORef Normal
  eatFirstChar <- newIORef False
  insertExitCallback <- newIORef (pure ())
  withWeaver config \weaver -> do
    withQuadRenderer \quadRenderer -> do
      action DemoWindow{..}

instance Scroll DemoWindow where
  scroll _ y DemoWindow{screenYPos, weaver} = do
    height <- getLineHeight weaver
    modifyIORef screenYPos (max 0 . (\p -> p - truncate y * height))

pattern GMKNone :: GLFW.ModifierKeys
pattern GMKNone <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = False, modifierKeysAlt = False }

pattern GMKShift :: Bool -> GLFW.ModifierKeys
pattern GMKShift s <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = s, modifierKeysAlt = False }

pattern GMKAlt :: Bool -> GLFW.ModifierKeys
pattern GMKAlt s <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = False, modifierKeysAlt = s }

instance SendKey DemoWindow where
  sendKey key mods dw = do
    readIORef dw.mode >>= \case
      Normal -> do
        case (mods, key) of
          (GMKShift s, GLFW.Key'H) -> modifyIORef dw.selection . ext s Selection.moveLeft =<< readIORef dw.document
          (GMKShift s, GLFW.Key'L) -> modifyIORef dw.selection . ext s Selection.moveRight =<< readIORef dw.document
          (GMKShift s, GLFW.Key'E) -> modifyIORef dw.selection . ext s Selection.selectToWordEnd =<< readIORef dw.document
          (GMKShift s, GLFW.Key'B) -> modifyIORef dw.selection . ext s Selection.selectToWordBegin =<< readIORef dw.document
          (GMKShift s, GLFW.Key'W) -> modifyIORef dw.selection . ext s Selection.selectToWordStart =<< readIORef dw.document
          (GMKShift s, GLFW.Key'J) -> do
            sel <- readIORef dw.selection
            doc <- readIORef dw.document
            newSel <- if sel.mark.line /= Document.countLines doc - 1
              then do
                target <- maybe (getTarget doc sel.mark) pure sel.target
                let nextLine = stripNewLine . Document.getLine (sel.mark.line + 1) $ doc
                newCol <- if nextLine == "" then pure 0 else do
                  rqNext <- layoutTextCached dw.config.face nextLine
                  col <- Raqm.positionToIndex rqNext target 0
                  pure $ min col (Document.lastCharOffset nextLine)
                let newMark = Coord (sel.mark.line + 1) newCol
                pure $ SelectionWithTarget newMark newMark (Just target)
              else pure $ Selection sel.mark sel.mark
            modifyIORef dw.selection . ext s (\_ _ -> newSel) $ doc
          (GMKShift s, GLFW.Key'K) -> do
            sel <- readIORef dw.selection
            doc <- readIORef dw.document
            newSel <- if sel.mark.line /= 0
              then do
                target <- maybe (getTarget doc sel.mark) pure sel.target
                let prevLine = stripNewLine . Document.getLine (sel.mark.line - 1) $ doc
                newCol <- if prevLine == "" then pure 0 else do
                  rqPrev <- layoutTextCached dw.config.face prevLine
                  col <- Raqm.positionToIndex rqPrev target 0
                  pure $ min col (Document.lastCharOffset prevLine)
                let newMark = Coord (sel.mark.line - 1) newCol
                pure $ SelectionWithTarget newMark newMark (Just target)
              else pure $ Selection sel.mark sel.mark
            modifyIORef dw.selection . ext s (\_ _ -> newSel) $ doc
          (GMKNone, GLFW.Key'X) -> modifyIORef dw.selection . Selection.expandToLine =<< readIORef dw.document
          (GMKNone, GLFW.Key'D) -> do
            sel <- readIORef dw.selection
            doc <- readIORef dw.document
            let (newDoc, translate) = Document.patch (Selection.selToIv doc sel) "" doc
            writeIORef dw.document newDoc
            writeIORef dw.selection $ Selection (translate sel.anchor) (translate sel.mark)
          (GMKNone, GLFW.Key'I) -> do
            modifyIORef dw.selection Selection.turnLeft
            writeIORef dw.eatFirstChar True
            writeIORef dw.mode Insert
          (GMKNone, GLFW.Key'A) -> do
            modifyIORef dw.selection Selection.turnRight
            sel <- readIORef dw.selection
            doc <- readIORef dw.document
            when (sel.mark == Document.endOfDocument doc) do
              let
                len = Document.charLengthAt doc sel.mark
                c = sel.mark { column = sel.mark.column + len }
              modifyIORef dw.document (fst . Document.patch (Document.Iv c c) "\n")
            doc' <- readIORef dw.document
            writeIORef dw.selection sel { mark = Document.moveCoord doc' 1 sel.mark }
            writeIORef dw.eatFirstChar True
            writeIORef dw.mode Insert
          (GMKShift True, GLFW.Key'5) -> do
            doc <- readIORef dw.document
            writeIORef dw.selection (Selection (Coord 0 0) (Document.endOfDocument doc))
          (GMKAlt True, GLFW.Key'Semicolon) -> modifyIORef dw.selection Selection.alternate
          (GMKNone, GLFW.Key'Semicolon) -> modifyIORef dw.selection \sel -> sel { anchor = sel.mark }
          _ -> pure ()
      Insert -> do
        case (mods, key) of
          (GMKNone, GLFW.Key'Escape) -> do
            join $ readIORef dw.insertExitCallback
            writeIORef dw.insertExitCallback (pure ())
            writeIORef dw.mode Normal
          (GMKNone, GLFW.Key'Backspace) -> do
            sel <- readIORef dw.selection
            doc <- readIORef dw.document
            when (sel.mark /= Coord 0 0) do
              let
                pos = Document.moveCoord doc (-1) sel.mark
                (newDoc, translate) = Document.patch (Document.Iv pos sel.mark) "" doc
              writeIORef dw.document newDoc
              writeIORef dw.selection $ Selection (translate sel.anchor) (translate sel.mark)
          (GMKNone, GLFW.Key'Delete) -> do
            sel <- readIORef dw.selection
            doc <- readIORef dw.document
            when (sel.mark /= Document.endOfDocument doc) do
              let
                len = Document.charLengthAt doc sel.mark
                (newDoc, translate) = Document.patch (Document.Iv sel.mark sel.mark { column = sel.mark.column + len }) "" doc
              writeIORef dw.document newDoc
              writeIORef dw.selection $ Selection (translate sel.anchor) (translate sel.mark)
          (GMKNone, GLFW.Key'Enter) -> sendChar '\n' dw
          _ -> pure ()
    ensureCursorRangeOnScreen
    where
      ext c m = if c then Selection.extend m else m
      ensureCursorRangeOnScreen = do
        sel <- readIORef dw.selection
        doc <- readIORef dw.document
        height <- getLineHeight dw.weaver
        let markYPos = height * sel.mark.line
        markXPos <- getTarget doc sel.mark
        screenYPos <- readIORef dw.screenYPos
        screenXPos <- readIORef dw.screenXPos
        Viewport _ _ screenWidth screenHeight <- getSlot viewportSlot
        let
          (topRange, bottomRange) = dw.config.cursorVerticalRangeOnScreen
          maxScreenYPos = markYPos - truncate (fromIntegral screenHeight * topRange)
          minScreenYPos = markYPos - truncate (fromIntegral screenHeight * bottomRange) + height
          (leftRange, rightRange) = dw.config.cursorHorizontalRangeOnScreen
          maxScreenXPos = markXPos - truncate (fromIntegral screenWidth * leftRange)
          minScreenXPos = markXPos - truncate (fromIntegral screenWidth * rightRange)
        writeIORef dw.screenYPos . max 0 . min maxScreenYPos . max minScreenYPos $ screenYPos
        writeIORef dw.screenXPos . max 0 . min maxScreenXPos . max minScreenXPos $ screenXPos
      getTarget doc mark = do
        let line = Document.getLine mark.line doc
        rq <- layoutTextCached dw.config.face . stripNewLine $ line
        after <- if mark.column == Text.lengthWord8 line - 1
          then (+ dw.config.face.sizePx `div` 2) <$> if mark.column == 0 then pure 0 else Raqm.indexToXPosition rq (mark.column - 1)
          else Raqm.indexToXPosition rq mark.column
        before <- if mark.column == 0
          then pure 0
          else Raqm.indexToXPosition rq (mark.column - 1)
        pure $ (before + after) `div` 2

instance SendChar DemoWindow where
  sendChar char dw = eatFirstChar do
    readIORef dw.mode >>= \case
      Normal -> pure ()
      Insert -> do
        sel <- readIORef dw.selection
        doc <- readIORef dw.document
        let (newDoc, translate) = Document.patch (Document.Iv sel.mark sel.mark) (Text.singleton char) doc
        writeIORef dw.document newDoc
        writeIORef dw.selection $ Selection (translate sel.anchor) (translate sel.mark)
    where
      eatFirstChar action =
        readIORef dw.eatFirstChar >>= \case
          True -> writeIORef dw.eatFirstChar False
          False -> action

instance Window DemoWindow where
  drawWindow res dw = do
    let Color{..} = dw.config.background in
      glClearColor red green blue alpha
    glClear GL_COLOR_BUFFER_BIT
    Viewport 0 0 screenWidth screenHeight <- getSlot viewportSlot
    document <- readIORef dw.document
    height <- getLineHeight dw.weaver
    let lineNumberMargin = 10
    digitWidth <- (`div` 64) . (.xAdvance) . (!! 0) <$> (Raqm.getGlyphs =<< layoutTextCached dw.config.face "0")
    let lineNumberWidth = (+ 2 * lineNumberMargin) . (* digitWidth) . max 1 . ceiling . logBase 10 . (+ 1) . (fromIntegral :: Int -> Double) . Document.countLines $ document
    drawQuadColor dw.quadRenderer res dw.config.lineNumbersBackground 0 (screenHeight - 1) lineNumberMargin (lineNumberWidth - lineNumberMargin - 1)
    let textRes = Resolution (screenWidth - lineNumberWidth) screenHeight
    descender <- getDescender dw.weaver
    screenYPos <- readIORef dw.screenYPos
    screenXPos <- readIORef dw.screenXPos
    let
      beginLine = screenYPos `div` height
      linePos idx = height * (idx + 1) + descender - screenYPos
      numLines = res.h `divSat` height
      divSat a b = let (r, m) = divMod a b in if m /= 0 then r + 1 else r
    sel <- readIORef dw.selection
    forM_ [ beginLine, beginLine + 1 .. min (beginLine + numLines) (Document.countLines document - 1) ] \idx -> do
      setSlot viewportSlot $ Viewport lineNumberWidth 0 (screenWidth - lineNumberWidth) screenHeight
      let y = linePos idx
      let ln = stripNewLine . Document.getLine idx $ document
      let selRange = maybeToList . Selection.selAtLine document sel $ idx
      let selColorSpec = fmap
            (\(begin, end) ->
              ( begin
              , end + Document.charLengthAt document (Coord idx end)
              , dw.config.primarySelectionForeground
              ))
            selRange
      let cursorRange = [ sel.mark.column | idx == sel.mark.line ]
      let cursorColorSpec = fmap
            (\pos ->
              ( pos
              , pos + Document.charLengthAt document (Coord idx pos)
              , dw.config.primaryCursorForeground
              ))
            cursorRange
      let quads = fmap (\(begin, end) -> (begin, end, dw.config.primarySelectionBackground)) selRange
            ++ fmap (\pos -> (pos, pos, dw.config.primaryCursorBackground)) cursorRange
      forM_ quads \(begin, end, color) -> do
        rq <- layoutTextCached dw.config.face ln
        xStart <- if begin == 0 then pure 0 else Raqm.indexToXPosition rq (begin - 1)
        xEnd <- if end == Text.lengthWord8 ln
          then (+ dw.config.face.sizePx `div` 2) <$> if end == 0 then pure 0 else Raqm.indexToXPosition rq (end - 1)
          else Raqm.indexToXPosition rq end
        drawQuadColor dw.quadRenderer textRes color (y - descender - height) (y - descender) (xStart - screenXPos) (xEnd - screenXPos)
      drawText dw.weaver textRes (-screenXPos) y (cursorColorSpec ++ selColorSpec ++ [(0, Text.lengthWord8 ln, dw.config.foreground)]) ln
      -- let rainbow = fmap (\(n, c) -> (n, n + 1, c)) . zip [0 ..] . cycle $ [ Color 0.93 0.94 0.96 1, Color 0.53 0.75 0.82 1, Color 0.37 0.51 0.67 1, Color 0.75 0.38 0.42 1, Color 0.86 0.53 0.44 1, Color 0.92 0.80 0.55 1, Color 0.64 0.75 0.55 1, Color 0.71 0.56 0.68 1 ]
      setSlot viewportSlot $ Viewport 0 0 screenWidth screenHeight
      drawText dw.weaver res lineNumberMargin y [(0, 100, dw.config.lineNumbersForeground)] (Text.pack (show idx))

stripNewLine :: Text -> Text
stripNewLine t = if "\n" `Text.isSuffixOf` t then Text.dropEnd 1 t else t
