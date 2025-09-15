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
  , EditorWindow(..)
  , withEditorWindow
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

import Data.List (partition)

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
    modifyIORef dwm.windows (IntMap.alter (fmap (\wi -> wi { needRedraw = True })) wid)

flush :: Resolution -> DefaultWindowManager -> IO ()
flush res@(Resolution w h) dwm = do
  [(0, onlyWin)] <- IntMap.assocs <$> readIORef dwm.windows
  let margin = 20
  let texRes = Resolution (w - margin * 2) (h - margin * 2)
  case onlyWin of
    WindowInfo win _ -> renderToTexture texRes (drawWindow texRes win) \tex -> do
      drawQuadTexture dwm.quadRenderer res tex margin (h - 1 - margin) margin (w - 1 - margin)

data EditorWindow = EditorWindow
  { screenXPos :: IORef Int
  , screenYPos :: IORef Int
  , weaver :: Weaver
  , context :: Context
  , keyMatchState :: IORef KeyCandidates
  , quadRenderer :: QuadRenderer
  }

data Mode = NormalMode | InsertMode
  deriving Eq

withEditorWindow :: Config -> (EditorWindow -> IO a) -> IO a
withEditorWindow config action = do
  screenXPos <- newIORef 0
  screenYPos <- newIORef 0
  document <- newIORef . Document.fromText =<< Text.readFile "./app/Window.hs"
  selection <- newIORef (Selection (Coord 0 0) (Coord 0 0))
  mode <- newIORef NormalMode
  eatFirstChar <- newIORef False
  insertExitCallback <- newIORef (pure ())
  let context = Context{..}
  keyMatchState <- newIORef normalKeymap.candidates
  withWeaver config \weaver -> do
    withQuadRenderer \quadRenderer -> do
      action EditorWindow{..}

instance Scroll EditorWindow where
  scroll _ y EditorWindow{screenYPos, weaver} = do
    height <- getLineHeight weaver
    modifyIORef screenYPos (max 0 . (\p -> p - truncate y * height))

pattern GMKNone :: GLFW.ModifierKeys
pattern GMKNone <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = False, modifierKeysAlt = False }

pattern GMKShift :: Bool -> GLFW.ModifierKeys
pattern GMKShift s <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = s, modifierKeysAlt = False }

pattern GMKAlt :: Bool -> GLFW.ModifierKeys
pattern GMKAlt s <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = False, modifierKeysAlt = s }

instance SendKey EditorWindow where
  sendKey key mods dw = do
    readIORef dw.context.mode >>= \case
      NormalMode -> do
        (cmds, cands) <- updateKeyCandidates (mods, key) <$> readIORef dw.keyMatchState
        case cmds of
          [] -> writeIORef dw.keyMatchState (if null cands then normalKeymap.candidates else cands)
          [cmd] -> cmd dw.context >> writeIORef dw.keyMatchState normalKeymap.candidates
          _ -> error "invalid keymap"
      InsertMode -> do
        case (mods, key) of
          (GMKNone, GLFW.Key'Escape) -> do
            join $ readIORef dw.context.insertExitCallback
            writeIORef dw.context.insertExitCallback (pure ())
            writeIORef dw.context.mode NormalMode
          (GMKNone, GLFW.Key'Backspace) -> do
            sel <- readIORef dw.context.selection
            doc <- readIORef dw.context.document
            when (sel.mark /= Coord 0 0) do
              let
                pos = Document.moveCoord doc (-1) sel.mark
                (newDoc, translate) = Document.patch (Document.Iv pos sel.mark) "" doc
              writeIORef dw.context.document newDoc
              writeIORef dw.context.selection $ Selection (translate sel.anchor) (translate sel.mark)
          (GMKNone, GLFW.Key'Delete) -> do
            sel <- readIORef dw.context.selection
            doc <- readIORef dw.context.document
            when (sel.mark /= Document.endOfDocument doc) do
              let
                len = Document.charLengthAt doc sel.mark
                (newDoc, translate) = Document.patch (Document.Iv sel.mark sel.mark { column = sel.mark.column + len }) "" doc
              writeIORef dw.context.document newDoc
              writeIORef dw.context.selection $ Selection (translate sel.anchor) (translate sel.mark)
          (GMKNone, GLFW.Key'Enter) -> sendChar '\n' dw
          _ -> pure ()
    ensureCursorRangeOnScreen
    where
      ensureCursorRangeOnScreen = do
        sel <- readIORef dw.context.selection
        height <- getLineHeight dw.weaver
        let markYPos = height * sel.mark.line
        markXPos <- getTarget dw.context
        screenYPos <- readIORef dw.screenYPos
        screenXPos <- readIORef dw.screenXPos
        Viewport _ _ screenWidth screenHeight <- getSlot viewportSlot
        let
          (topRange, bottomRange) = dw.context.config.cursorVerticalRangeOnScreen
          maxScreenYPos = markYPos - truncate (fromIntegral screenHeight * topRange)
          minScreenYPos = markYPos - truncate (fromIntegral screenHeight * bottomRange) + height
          (leftRange, rightRange) = dw.context.config.cursorHorizontalRangeOnScreen
          maxScreenXPos = markXPos - truncate (fromIntegral screenWidth * leftRange)
          minScreenXPos = markXPos - truncate (fromIntegral screenWidth * rightRange)
        writeIORef dw.screenYPos . max 0 . min maxScreenYPos . max minScreenYPos $ screenYPos
        writeIORef dw.screenXPos . max 0 . min maxScreenXPos . max minScreenXPos $ screenXPos

instance SendChar EditorWindow where
  sendChar char dw = eatFirstChar do
    readIORef dw.context.mode >>= \case
      NormalMode -> pure ()
      InsertMode -> do
        sel <- readIORef dw.context.selection
        doc <- readIORef dw.context.document
        let (newDoc, translate) = Document.patch (Document.Iv sel.mark sel.mark) (Text.singleton char) doc
        writeIORef dw.context.document newDoc
        writeIORef dw.context.selection $ Selection (translate sel.anchor) (translate sel.mark)
    where
      eatFirstChar action =
        readIORef dw.context.eatFirstChar >>= \case
          True -> writeIORef dw.context.eatFirstChar False
          False -> action

instance Window EditorWindow where
  drawWindow res dw = do
    let Color{..} = dw.context.config.background in
      glClearColor red green blue alpha
    glClear GL_COLOR_BUFFER_BIT
    Viewport 0 0 screenWidth screenHeight <- getSlot viewportSlot
    document <- readIORef dw.context.document
    height <- getLineHeight dw.weaver
    let lineNumberMargin = 10
    digitWidth <- (`div` 64) . (.xAdvance) . (!! 0) <$> (Raqm.getGlyphs =<< layoutTextCached dw.context.config.face "0")
    let lineNumberWidth = (+ 2 * lineNumberMargin) . (* digitWidth) . max 1 . ceiling . logBase 10 . (+ 1) . (fromIntegral :: Int -> Double) . Document.countLines $ document
    drawQuadColor dw.quadRenderer res dw.context.config.lineNumbersBackground 0 (screenHeight - 1) lineNumberMargin (lineNumberWidth - lineNumberMargin - 1)
    let textRes = Resolution (screenWidth - lineNumberWidth) screenHeight
    descender <- getDescender dw.weaver
    screenYPos <- readIORef dw.screenYPos
    screenXPos <- readIORef dw.screenXPos
    let
      beginLine = screenYPos `div` height
      linePos idx = height * (idx + 1) + descender - screenYPos
      numLines = res.h `divSat` height
      divSat a b = let (r, m) = divMod a b in if m /= 0 then r + 1 else r
    sel <- readIORef dw.context.selection
    forM_ [ beginLine, beginLine + 1 .. min (beginLine + numLines) (Document.countLines document - 1) ] \idx -> do
      setSlot viewportSlot $ Viewport lineNumberWidth 0 (screenWidth - lineNumberWidth) screenHeight
      let y = linePos idx
      let ln = stripNewLine . Document.getLine idx $ document
      let selRange = maybeToList . Selection.selAtLine document sel $ idx
      let selColorSpec = fmap
            (\(begin, end) ->
              ( begin
              , end + Document.charLengthAt document (Coord idx end)
              , dw.context.config.primarySelectionForeground
              ))
            selRange
      let cursorRange = [ sel.mark.column | idx == sel.mark.line ]
      let cursorColorSpec = fmap
            (\pos ->
              ( pos
              , pos + Document.charLengthAt document (Coord idx pos)
              , dw.context.config.primaryCursorForeground
              ))
            cursorRange
      let quads = fmap (\(begin, end) -> (begin, end, dw.context.config.primarySelectionBackground)) selRange
            ++ fmap (\pos -> (pos, pos, dw.context.config.primaryCursorBackground)) cursorRange
      forM_ quads \(begin, end, color) -> do
        rq <- layoutTextCached dw.context.config.face ln
        xStart <- if begin == 0 then pure 0 else Raqm.indexToXPosition rq (begin - 1)
        xEnd <- if end == Text.lengthWord8 ln
          then (+ dw.context.config.face.sizePx `div` 2) <$> if end == 0 then pure 0 else Raqm.indexToXPosition rq (end - 1)
          else Raqm.indexToXPosition rq end
        drawQuadColor dw.quadRenderer textRes color (y - descender - height) (y - descender) (xStart - screenXPos) (xEnd - screenXPos)
      drawText dw.weaver textRes (-screenXPos) y (cursorColorSpec ++ selColorSpec ++ [(0, Text.lengthWord8 ln, dw.context.config.foreground)]) ln
      setSlot viewportSlot $ Viewport 0 0 screenWidth screenHeight
      drawText dw.weaver res lineNumberMargin y [(0, 100, dw.context.config.lineNumbersForeground)] (Text.pack (show idx))

stripNewLine :: Text -> Text
stripNewLine t = if "\n" `Text.isSuffixOf` t then Text.dropEnd 1 t else t

data Context = Context
  { selection :: IORef Selection
  , document :: IORef Document
  , config :: Config
  , mode :: IORef Mode
  , eatFirstChar :: IORef Bool
  , insertExitCallback :: IORef (IO ())
  }

type Command = Context -> IO ()

type KeyCandidates = [([(GLFW.ModifierKeys, GLFW.Key)], Command)]
newtype Keymap = Keymap { candidates :: KeyCandidates }

updateKeyCandidates :: (GLFW.ModifierKeys, GLFW.Key) -> KeyCandidates -> ([Command], KeyCandidates)
updateKeyCandidates key = onFst (map snd) . partition (null . fst) . concatMap (\case (k : ks, c) -> if k == key then [(ks, c)] else []; ([], _) -> undefined)
  where onFst f (a, b) = (f a, b)

noneMod, shiftMod, altMod :: GLFW.ModifierKeys
noneMod = GLFW.ModifierKeys False False False False False False
shiftMod = noneMod { GLFW.modifierKeysShift = True }
altMod = noneMod { GLFW.modifierKeysAlt = True }

normalKeymap :: Keymap
normalKeymap = Keymap
  [ ( [(noneMod, GLFW.Key'H)]
    , movement Selection.moveLeft
    )
  , ( [(shiftMod, GLFW.Key'H)]
    , movement $ Selection.extend Selection.moveLeft
    )
  , ( [(noneMod, GLFW.Key'L)]
    , movement Selection.moveRight
    )
  , ( [(shiftMod, GLFW.Key'L)]
    , movement $ Selection.extend Selection.moveRight
    )
  , ( [(noneMod, GLFW.Key'E)]
    , movement Selection.selectToWordEnd
    )
  , ( [(shiftMod, GLFW.Key'E)]
    , movement $ Selection.extend Selection.selectToWordEnd
    )
  , ( [(noneMod, GLFW.Key'B)]
    , movement Selection.selectToWordBegin
    )
  , ( [(shiftMod, GLFW.Key'B)]
    , movement $ Selection.extend Selection.selectToWordBegin
    )
  , ( [(noneMod, GLFW.Key'W)]
    , movement Selection.selectToWordStart
    )
  , ( [(shiftMod, GLFW.Key'W)]
    , movement $ Selection.extend Selection.selectToWordStart
    )
  , ( [(noneMod, GLFW.Key'J)]
    , moveDown id
    )
  , ( [(shiftMod, GLFW.Key'J)]
    , moveDown Selection.extend
    )
  , ( [(noneMod, GLFW.Key'K)]
    , moveUp id
    )
  , ( [(shiftMod, GLFW.Key'K)]
    , moveUp Selection.extend
    )
  , ( [(noneMod, GLFW.Key'X)]
    , movement Selection.expandToLine
    )
  , ( [(noneMod, GLFW.Key'G), (noneMod, GLFW.Key'L)]
    , movement \doc sel ->
      let
        end = (\t -> if t == "" then 0 else Document.lastCharOffset t) . Text.dropEnd 1 . Document.getLine sel.mark.line $ doc
        mark = Coord sel.mark.line end
      in
      Selection mark mark
    )
  , ( [(noneMod, GLFW.Key'D)]
    , \ctx -> do
      sel <- readIORef ctx.selection
      doc <- readIORef ctx.document
      let (newDoc, translate) = Document.patch (Selection.selToIv doc sel) "" doc
      writeIORef ctx.document newDoc
      writeIORef ctx.selection $ Selection (translate sel.anchor) (translate sel.mark)
    )
  , ( [(noneMod, GLFW.Key'I)]
    , \ctx -> do
      modifyIORef ctx.selection Selection.turnLeft
      enterInsert ctx
    )
  , ( [(noneMod, GLFW.Key'A)]
    , \ctx -> do
      modifyIORef ctx.selection Selection.turnRight
      sel <- readIORef ctx.selection
      doc <- readIORef ctx.document
      when (sel.mark == Document.endOfDocument doc) do
        let
          len = Document.charLengthAt doc sel.mark
          c = sel.mark { column = sel.mark.column + len }
        modifyIORef ctx.document (fst . Document.patch (Document.Iv c c) "\n")
      doc' <- readIORef ctx.document
      writeIORef ctx.selection sel { mark = Document.moveCoord doc' 1 sel.mark }
      writeIORef ctx.insertExitCallback do
        doc'' <- readIORef ctx.document
        modifyIORef ctx.selection \sel' ->
          if sel'.anchor < sel'.mark
          then sel' { mark = Document.moveCoord doc'' (-1) sel'.mark }
          else sel'
      enterInsert ctx
    )
  , ( [(shiftMod, GLFW.Key'5)]
    , movement \doc _ -> Selection (Coord 0 0) (Document.endOfDocument doc)
    )
  , ( [(altMod, GLFW.Key'Semicolon)]
    , movement . const $ Selection.alternate
    )
  , ( [(noneMod, GLFW.Key'Semicolon)]
    , movement . const $ \sel -> sel { anchor = sel.mark }
    )
  ]
  where
    movement m ctx = modifyIORef ctx.selection . m =<< readIORef ctx.document
    moveDown ext ctx = do
      sel <- readIORef ctx.selection
      doc <- readIORef ctx.document
      newSel <- if sel.mark.line /= Document.countLines doc - 1
        then do
          target <- maybe (getTarget ctx) pure sel.target
          let nextLine = stripNewLine . Document.getLine (sel.mark.line + 1) $ doc
          newCol <- if nextLine == "" then pure 0 else do
            rqNext <- layoutTextCached ctx.config.face nextLine
            col <- Raqm.positionToIndex rqNext target 0
            pure $ min col (Document.lastCharOffset nextLine)
          let newMark = Coord (sel.mark.line + 1) newCol
          pure $ SelectionWithTarget newMark newMark (Just target)
        else pure $ Selection sel.mark sel.mark
      modifyIORef ctx.selection . ext (\_ _ -> newSel) $ doc
    moveUp ext ctx = do
      sel <- readIORef ctx.selection
      doc <- readIORef ctx.document
      newSel <- if sel.mark.line /= 0
        then do
          target <- maybe (getTarget ctx) pure sel.target
          let prevLine = stripNewLine . Document.getLine (sel.mark.line - 1) $ doc
          newCol <- if prevLine == "" then pure 0 else do
            rqPrev <- layoutTextCached ctx.config.face prevLine
            col <- Raqm.positionToIndex rqPrev target 0
            pure $ min col (Document.lastCharOffset prevLine)
          let newMark = Coord (sel.mark.line - 1) newCol
          pure $ SelectionWithTarget newMark newMark (Just target)
        else pure $ Selection sel.mark sel.mark
      modifyIORef ctx.selection . ext (\_ _ -> newSel) $ doc
    enterInsert ctx = do
      writeIORef ctx.eatFirstChar True
      writeIORef ctx.mode InsertMode

getTarget :: Context -> IO Int
getTarget ctx = do
  doc <- readIORef ctx.document
  mark <- (.mark) <$> readIORef ctx.selection
  let line = Document.getLine mark.line doc
  rq <- layoutTextCached ctx.config.face . stripNewLine $ line
  after <- if mark.column == Text.lengthWord8 line - 1
    then (+ ctx.config.face.sizePx `div` 2) <$> if mark.column == 0 then pure 0 else Raqm.indexToXPosition rq (mark.column - 1)
    else Raqm.indexToXPosition rq mark.column
  before <- if mark.column == 0
    then pure 0
    else Raqm.indexToXPosition rq (mark.column - 1)
  pure $ (before + after) `div` 2
