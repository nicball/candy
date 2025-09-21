module EditorWindow
  ( DefaultEditorWindow
  , withDefaultEditorWindowFromPath
  , withDefaultEditorWindowFork
  ) where

import Control.Monad (forM_, join, when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (partition)
import Data.Maybe (maybeToList)
import Data.Text.Foreign qualified as Text
import Data.Text.IO qualified as Text
import Data.Text qualified as Text
import Data.Text (Text)
import Graphics.GL (pattern GL_COLOR_BUFFER_BIT, glClear, glClearColor)
import Graphics.UI.GLFW qualified as GLFW

import Config (Color(..), Config(..), FaceID(..), config)
import Document (Document, Coord(..))
import Document qualified
import GL (drawQuadColor, drawQuadTexture, quadFromBottomLeftWH, quadFromTopLeftWH, viewportSlot, withPoster, GLSlot(..), Poster, Resolution(..), Viewport(..))
import Raqm qualified
import Selection (Selection(..))
import Selection qualified
import Weaver (drawTextCached, getFaceCached, getLineHeight, layoutTextCached, withWeaver, Weaver)
import Nexus (newNexus, ListenerID, Nexus)
import Nexus qualified
import Window (Draw(..), EditorWindow(..), Scroll(..), SendChar(..), SendKey(..), Status(..), Mode(..))

data DefaultEditorWindow = DefaultEditorWindow
  { screenXPos :: IORef Int
  , screenYPos :: IORef Int
  , weaver :: Weaver
  , context :: Context
  , keyMatchState :: IORef KeyCandidates
  , poster :: Poster
  , lastTextRes :: IORef Resolution
  }

withDefaultEditorWindowFromPath :: FilePath -> (DefaultEditorWindow -> IO a) -> IO a
withDefaultEditorWindowFromPath path action = do
  screenXPos <- newIORef 0
  screenYPos <- newIORef 0
  document <- Document.fromText <$> Text.readFile path
  lastTextRes <- newIORef $ Resolution 0 0
  context <- newContext document
  keyMatchState <- newIORef normalKeymap.candidates
  face <- (.face) <$> readIORef config
  withWeaver face \weaver -> do
    withPoster \poster -> do
      action DefaultEditorWindow{..}

withDefaultEditorWindowFork :: DefaultEditorWindow-> (DefaultEditorWindow -> IO a) -> IO a
withDefaultEditorWindowFork other action = do
  screenXPos <- newIORef 0
  screenYPos <- newIORef 0
  lastTextRes <- newIORef $ Resolution 0 0
  context <- shareContext other.context
  keyMatchState <- newIORef normalKeymap.candidates
  face <- (.face) <$> readIORef config
  withWeaver face \weaver -> do
    withPoster \poster -> do
      action DefaultEditorWindow{..}

instance Scroll DefaultEditorWindow where
  scroll _ y dew = do
    height <- getLineHeight =<< getFaceCached . (.face) =<< readIORef config
    modifyIORef dew.screenYPos (max 0 . (\p -> p - truncate y * height))

pattern GMKNone :: GLFW.ModifierKeys
pattern GMKNone <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = False, modifierKeysAlt = False }

pattern GMKShift :: Bool -> GLFW.ModifierKeys
pattern GMKShift s <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = s, modifierKeysAlt = False }

pattern GMKAlt :: Bool -> GLFW.ModifierKeys
pattern GMKAlt s <- GLFW.ModifierKeys { modifierKeysControl = False, modifierKeysShift = False, modifierKeysAlt = s }

instance SendKey DefaultEditorWindow where
  sendKey key mods dew = do
    readIORef dew.context.mode >>= \case
      NormalMode -> do
        (cmds, cands) <- updateKeyCandidates (mods, key) <$> readIORef dew.keyMatchState
        case cmds of
          [] -> writeIORef dew.keyMatchState (if null cands then normalKeymap.candidates else cands)
          [cmd] -> cmd dew.context >> writeIORef dew.keyMatchState normalKeymap.candidates
          _ -> error "invalid keymap"
      InsertMode -> do
        case (mods, key) of
          (GMKNone, GLFW.Key'Escape) -> do
            join $ readIORef dew.context.insertExitCallback
            writeIORef dew.context.insertExitCallback (pure ())
            writeIORef dew.context.mode NormalMode
          (GMKNone, GLFW.Key'Backspace) -> do
            sel <- readIORef dew.context.selection
            doc <- readIORef dew.context.document
            when (sel.mark /= Coord 0 0) do
              let
                pos = Document.moveCoord doc (-1) sel.mark
                (newDoc, translate) = Document.patch (Document.Iv pos sel.mark) "" doc
              writeIORef dew.context.document newDoc
              Nexus.notify translate dew.context.onPatch
          (GMKNone, GLFW.Key'Delete) -> do
            sel <- readIORef dew.context.selection
            doc <- readIORef dew.context.document
            when (sel.mark /= Document.endOfDocument doc) do
              let
                len = Document.charLengthAt doc sel.mark
                (newDoc, translate) = Document.patch (Document.Iv sel.mark sel.mark { column = sel.mark.column + len }) "" doc
              writeIORef dew.context.document newDoc
              Nexus.notify translate dew.context.onPatch
          (GMKNone, GLFW.Key'Enter) -> sendChar '\n' dew
          _ -> pure ()
    ensureCursorRangeOnScreen
    where
      ensureCursorRangeOnScreen = do
        cfg <- readIORef config
        sel <- readIORef dew.context.selection
        height <- getLineHeight =<< getFaceCached cfg.face
        let markYPos = height * sel.mark.line
        markXPos <- getTarget dew.context
        screenYPos <- readIORef dew.screenYPos
        screenXPos <- readIORef dew.screenXPos
        Resolution screenWidth screenHeight <- readIORef dew.lastTextRes
        let
          (topRange, bottomRange) = cfg.cursorVerticalRangeOnScreen
          maxScreenYPos = markYPos - truncate (fromIntegral screenHeight * topRange)
          minScreenYPos = markYPos - truncate (fromIntegral screenHeight * bottomRange) + height
          (leftRange, rightRange) = cfg.cursorHorizontalRangeOnScreen
          maxScreenXPos = markXPos - truncate (fromIntegral screenWidth * leftRange)
          minScreenXPos = markXPos - truncate (fromIntegral screenWidth * rightRange)
        writeIORef dew.screenYPos . max 0 . min maxScreenYPos . max minScreenYPos $ screenYPos
        writeIORef dew.screenXPos . max 0 . min maxScreenXPos . max minScreenXPos $ screenXPos

instance SendChar DefaultEditorWindow where
  sendChar char dew = eatFirstChar do
    readIORef dew.context.mode >>= \case
      NormalMode -> pure ()
      InsertMode -> do
        sel <- readIORef dew.context.selection
        doc <- readIORef dew.context.document
        let (newDoc, translate) = Document.patch (Document.Iv sel.mark sel.mark) (Text.singleton char) doc
        writeIORef dew.context.document newDoc
        Nexus.notify translate dew.context.onPatch
    where
      eatFirstChar action =
        readIORef dew.context.eatFirstChar >>= \case
          True -> writeIORef dew.context.eatFirstChar False
          False -> action

instance Draw DefaultEditorWindow where
  draw res dew = do
    cfg <- readIORef config
    let Color{..} = cfg.background in
      glClearColor red green blue alpha
    glClear GL_COLOR_BUFFER_BIT
    document <- readIORef dew.context.document
    ftFace <- getFaceCached cfg.face
    height <- getLineHeight ftFace
    let margin = 20
    digitWidth <- (`div` 64) . (.xAdvance) . (!! 0) <$> (Raqm.getGlyphs =<< layoutTextCached cfg.face "0")
    let lineNumberWidth = (+ 10) . (* digitWidth) . max 1 . ceiling . logBase 10 . (+ 1) . (fromIntegral :: Int -> Double) . Document.countLines $ document
    drawQuadColor dew.poster res cfg.lineNumbersBackground $ quadFromTopLeftWH 0 0 lineNumberWidth res.h
    let textRes = Resolution (res.w - lineNumberWidth - margin) res.h
    writeIORef dew.lastTextRes textRes
    screenYPos <- readIORef dew.screenYPos
    screenXPos <- readIORef dew.screenXPos
    let
      beginLine = screenYPos `div` height
      linePos idx = height * (idx + 1) - screenYPos - 1
      numLines = res.h `divSat` height
      divSat a b = let (r, m) = divMod a b in if m /= 0 then r + 1 else r
    sel <- readIORef dew.context.selection
    forM_ [ beginLine, beginLine + 1 .. min (beginLine + numLines) (Document.countLines document - 1) ] \idx -> do
      setSlot viewportSlot $ Viewport (lineNumberWidth + margin) 0 (res.w - lineNumberWidth - margin) res.h
      let y = linePos idx
      let ln = stripNewLine . Document.getLine idx $ document
      let selRange = maybeToList . Selection.selAtLine document sel $ idx
      let selColorSpec = fmap
            (\(begin, end) ->
              ( begin
              , end + Document.charLengthAt document (Coord idx end)
              , cfg.primarySelectionForeground
              ))
            selRange
      let cursorRange = [ sel.mark.column | idx == sel.mark.line ]
      let cursorColorSpec = fmap
            (\pos ->
              ( pos
              , pos + Document.charLengthAt document (Coord idx pos)
              , cfg.primaryCursorForeground
              ))
            cursorRange
      let quads = fmap (\(begin, end) -> (begin, end, cfg.primarySelectionBackground)) selRange
            ++ fmap (\pos -> (pos, pos, cfg.primaryCursorBackground)) cursorRange
      forM_ quads \(begin, end, color) -> do
        rq <- layoutTextCached cfg.face ln
        xBegin <- if begin == 0 then pure 0 else Raqm.indexToXPosition rq (begin - 1)
        xEnd <- if end == Text.lengthWord8 ln
          then (+ cfg.face.sizePx `div` 2) <$> if end == 0 then pure 0 else Raqm.indexToXPosition rq (end - 1)
          else Raqm.indexToXPosition rq end
        drawQuadColor dew.poster textRes color $ quadFromBottomLeftWH (xBegin - screenXPos) y (xEnd - xBegin + 1) height
      when (not . Text.null $ ln) do
        (lnTex, lnRes) <- drawTextCached dew.weaver (cursorColorSpec ++ selColorSpec ++ [(0, Text.lengthWord8 ln, cfg.foreground)]) ln
        drawQuadTexture dew.poster textRes lnTex $ quadFromBottomLeftWH (-screenXPos) y lnRes.w lnRes.h
      setSlot viewportSlot $ Viewport 0 0 res.w res.h
      let numFg = if idx == sel.mark.line
            then cfg.lineNumbersCurrentForeground
            else cfg.lineNumbersForeground
      when (idx == sel.mark.line) do
        drawQuadColor dew.poster res cfg.lineNumbersCurrentBackground $ quadFromBottomLeftWH 0 y lineNumberWidth height
      (numTex, numRes) <- drawTextCached dew.weaver [(0, 100, numFg)] (Text.pack (show idx))
      drawQuadTexture dew.poster res numTex $ quadFromBottomLeftWH 5 y numRes.w numRes.h

instance EditorWindow DefaultEditorWindow where
  getStatus w = Status <$> readIORef w.context.selection <*> readIORef w.context.mode

stripNewLine :: Text -> Text
stripNewLine t = if "\n" `Text.isSuffixOf` t then Text.dropEnd 1 t else t

data Context = Context
  { selection :: IORef Selection
  , document :: IORef Document
  , onPatch :: Nexus (Coord -> Coord)
  , onPatchToken :: ListenerID
  , mode :: IORef Mode
  , eatFirstChar :: IORef Bool
  , insertExitCallback :: IORef (IO ())
  }

newContext :: Document -> IO Context
newContext doc = do
  document <- newIORef doc
  onPatch <- newNexus
  selection <- newIORef (Selection (Coord 0 0) (Coord 0 0))
  onPatchToken <- flip Nexus.addListener onPatch \move -> do
    modifyIORef selection \sel -> Selection (move sel.anchor) (move sel.mark)
  mode <- newIORef NormalMode
  eatFirstChar <- newIORef False
  insertExitCallback <- newIORef (pure ())
  pure Context{..}

shareContext :: Context -> IO Context
shareContext Context{document, onPatch} = do
  selection <- newIORef (Selection (Coord 0 0) (Coord 0 0))
  onPatchToken <- flip Nexus.addListener onPatch \move -> do
    modifyIORef selection \sel -> Selection (move sel.anchor) (move sel.mark)
  mode <- newIORef NormalMode
  eatFirstChar <- newIORef False
  insertExitCallback <- newIORef (pure ())
  pure Context{..}

deleteContext :: Context -> IO ()
deleteContext context = do
  Nexus.removeListener context.onPatchToken context.onPatch

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
      Nexus.notify translate ctx.onPatch
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
            rqNext <- flip layoutTextCached nextLine . (.face) =<< readIORef config
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
            rqPrev <- flip layoutTextCached prevLine . (.face) =<< readIORef config
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
  face <- (.face) <$> readIORef config
  rq <- layoutTextCached face . stripNewLine $ line
  after <- if mark.column == Text.lengthWord8 line - 1
    then (+ face.sizePx `div` 2) <$> if mark.column == 0 then pure 0 else Raqm.indexToXPosition rq (mark.column - 1)
    else Raqm.indexToXPosition rq mark.column
  before <- if mark.column == 0
    then pure 0
    else Raqm.indexToXPosition rq (mark.column - 1)
  pure $ (before + after) `div` 2
