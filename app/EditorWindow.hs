module EditorWindow
  ( DefaultEditorWindow
  ) where

import Control.Monad (forM, forM_, join, when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (partition)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (maybeToList)
import Data.Semigroup (sconcat)
import Data.Text.Foreign qualified as Text
import Data.Text qualified as Text
import Data.Text (Text)
import Graphics.UI.GLFW qualified as GLFW

import Config (ConfigT(..), FaceIDT(..), config)
import Document (Document, Coord(..), DocumentType(..))
import Document qualified
import GL (drawQuadColor, drawQuadTexture, quadFromBottomLeftWH, quadFromTopLeftWH, quadToViewport, viewportSlot, posterSingleton, GLSlot(..), Resolution(..), clearViewport)
import Raqm qualified
import Selection (Selection(..), Selections(..), translateSelections, mergeSelections, selToIv)
import Selection qualified
import Weaver (drawTextCached, getFaceCached, getLineHeight, layoutTextCached)
import Nexus (ListenerID)
import Nexus qualified
import Window (Draw(..), EditorWindow(..), Scroll(..), SendChar(..), SendKey(..), Status(..), Mode(..), GetBox(..), pattern GMKNone)
import Box (drawableBox)
import Refcount (withRefcount)

data DefaultEditorWindow = DefaultEditorWindow
  { screenXPos :: IORef Int
  , screenYPos :: IORef Int
  , keyMatchState :: IORef KeyCandidates
  , lastTextRes :: IORef Resolution
  , name :: Text
  , minSize :: IORef Int
  , selections :: IORef Selections
  , document :: Document
  , onPatchToken :: ListenerID
  , mode :: IORef Mode
  , eatFirstChar :: IORef Bool
  , insertExitCallback :: IORef (IO ())
  }

instance EditorWindow DefaultEditorWindow where
  new document = do
    screenXPos <- newIORef 0
    screenYPos <- newIORef 0
    lastTextRes <- newIORef $ Resolution 0 0
    keyMatchState <- newIORef normalKeymap.candidates
    let name = case document.type_ of
          FileBacked path -> Text.pack path
          Scratch -> "*scratch*"
    minSize <- newIORef 1
    selections <- newIORef . Selections . NE.singleton $ Selection (Coord 0 0) (Coord 0 0)
    onPatchToken <- flip Nexus.addListener document.onPatch \translate -> do
      modifyIORef selections (translateSelections translate)
    mode <- newIORef NormalMode
    eatFirstChar <- newIORef False
    insertExitCallback <- newIORef (pure ())
    pure DefaultEditorWindow{..}
  fork other = new other.document
  getDocument dew = pure dew.document
  close dew = Nexus.removeListener dew.onPatchToken dew.document.onPatch
  getStatus w = Status <$> readIORef w.selections <*> readIORef w.mode <*> pure w.name

instance Scroll DefaultEditorWindow where
  scroll _ y dew = do
    height <- flip withRefcount getLineHeight =<< getFaceCached . (.face) =<< readIORef config
    modifyIORef dew.screenYPos (max 0 . (\p -> p - truncate y * height))

instance SendKey DefaultEditorWindow where
  sendKey key mods dew = do
    readIORef dew.mode >>= \case
      NormalMode -> do
        (cmds, cands) <- updateKeyCandidates (mods, key) <$> readIORef dew.keyMatchState
        case cmds of
          [] -> writeIORef dew.keyMatchState (if null cands then normalKeymap.candidates else cands)
          [cmd] -> cmd dew >> writeIORef dew.keyMatchState normalKeymap.candidates
          _ -> error "invalid keymap"
      InsertMode -> do
        case (mods, key) of
          (GMKNone, GLFW.Key'Escape) -> do
            join $ readIORef dew.insertExitCallback
            writeIORef dew.insertExitCallback (pure ())
            writeIORef dew.mode NormalMode
          (GMKNone, GLFW.Key'Backspace) -> do
            sels <- filter ((/= Coord 0 0) . (.mark)) . NE.toList . (.value) <$> readIORef dew.selections
            ivs <- traverse (\s -> flip Document.Iv s.mark <$> Document.moveCoord dew.document (-1) s.mark) sels
            Document.patch (fmap (, "") ivs) dew.document
          (GMKNone, GLFW.Key'Delete) -> do
            end <- Document.endOfDocument dew.document
            sels <- filter ((/= end) . (.mark)) . NE.toList . (.value) <$> readIORef dew.selections
            ivs <- traverse (selToIv dew.document) sels
            Document.patch (fmap (, "") ivs) dew.document
          (GMKNone, GLFW.Key'Enter) -> sendChar '\n' dew
          _ -> pure ()
    ensureCursorRangeOnScreen
    where
      ensureCursorRangeOnScreen = do
        cfg <- readIORef config
        sel <- NE.head . (.value) <$> readIORef dew.selections
        height <- flip withRefcount getLineHeight =<< getFaceCached cfg.face
        let markYPos = height * sel.mark.line
        markXPos <- getTarget dew sel
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
    readIORef dew.mode >>= \case
      NormalMode -> pure ()
      InsertMode -> do
        ivs <- fmap (\s -> Document.Iv s.mark s.mark) . NE.toList . (.value) <$> readIORef dew.selections
        Document.patch (fmap (, Text.singleton char) ivs) dew.document
    where
      eatFirstChar action =
        readIORef dew.eatFirstChar >>= \case
          True -> writeIORef dew.eatFirstChar False
          False -> action

instance Draw DefaultEditorWindow where
  draw res dew = do
    cfg <- readIORef config
    clearViewport cfg.background
    height <- flip withRefcount getLineHeight =<< getFaceCached cfg.face
    let margin = 20
    totalLines <- Document.countLines dew.document
    digitWidth <- (`div` 64) . (.xAdvance) . (!! 0) <$> (flip withRefcount Raqm.getGlyphs =<< layoutTextCached cfg.face "0")
    let lineNumberWidth = (+ 10) . (* digitWidth) . max 1 . ceiling . logBase 10 . (+ 1) . (fromIntegral :: Int -> Double) $ totalLines
    drawQuadColor posterSingleton res cfg.lineNumbersBackground $ quadFromTopLeftWH 0 0 lineNumberWidth res.h
    let textRes = Resolution (res.w - lineNumberWidth - margin) res.h
    writeIORef dew.lastTextRes textRes
    writeIORef dew.minSize (lineNumberWidth + margin + 1)
    screenYPos <- readIORef dew.screenYPos
    screenXPos <- readIORef dew.screenXPos
    let
      beginLine = screenYPos `div` height
      linePos idx = height * (idx + 1) - screenYPos - 1
      numLines = res.h `divSat` height
      divSat a b = let (r, m) = divMod a b in if m /= 0 then r + 1 else r
    sels <- (\(p NE.:| xs) -> (True, p) : fmap (False, ) xs) . (.value) <$> readIORef dew.selections
    psel <- NE.head . (.value) <$> readIORef dew.selections
    oldViewport <- getSlot viewportSlot
    forM_ [ beginLine, beginLine + 1 .. min (beginLine + numLines) (totalLines - 1) ] \idx -> do
      setSlot viewportSlot . flip quadToViewport oldViewport $ quadFromTopLeftWH (lineNumberWidth + margin) 0 (res.w - lineNumberWidth - margin) res.h
      let y = linePos idx
      ln <- stripNewLine <$> Document.getLine idx dew.document
      selRange <- concat <$> forM sels \(p, s) -> do
        range <- maybeToList <$> Selection.selAtLine dew.document s idx
        pure . fmap (\(b, e) -> (p, b, e)) $ range
      selColorSpec <- forM selRange \(p, begin, end) -> do
        len <- Document.charLengthAt dew.document (Coord idx end)
        pure
          ( begin
          , end + len
          , if p then cfg.primarySelectionForeground else cfg.secondarySelectionForeground
          )
      let cursorRange = [ (p, s.mark.column) | (p, s) <- sels, s.mark.line == idx ]
      cursorColorSpec <- forM cursorRange \(p, pos) -> do
        len <- Document.charLengthAt dew.document (Coord idx pos)
        pure
          ( pos
          , pos + len
          , if p then cfg.primaryCursorForeground else cfg.secondaryCursorForeground
          )
      let quads = fmap (\(p, begin, end) -> (begin, end, if p then cfg.primarySelectionBackground else cfg.secondarySelectionBackground)) selRange
            ++ fmap (\(p, pos) -> (pos, pos, if p then cfg.primaryCursorBackground else cfg.secondaryCursorBackground)) cursorRange
      forM_ quads \(begin, end, color) -> do
        layoutTextCached cfg.face ln >>= flip withRefcount \rq -> do
          xBegin <- if begin == 0 then pure 0 else Raqm.indexToXPosition rq (begin - 1)
          xEnd <- if end == Text.lengthWord8 ln
            then (+ cfg.face.sizePx `div` 2) <$> if end == 0 then pure 0 else Raqm.indexToXPosition rq (end - 1)
            else Raqm.indexToXPosition rq end
          drawQuadColor posterSingleton textRes color $ quadFromBottomLeftWH (xBegin - screenXPos) y (xEnd - xBegin + 1) height
      when (not . Text.null $ ln) do
        drawTextCached cfg.face (cursorColorSpec ++ selColorSpec ++ [(0, Text.lengthWord8 ln, cfg.foreground)]) ln >>= flip withRefcount \(lnTex, lnRes) -> do
          drawQuadTexture posterSingleton textRes lnTex $ quadFromBottomLeftWH (-screenXPos) y lnRes.w lnRes.h
      setSlot viewportSlot oldViewport
      let numFg = if idx == psel.mark.line
            then cfg.lineNumbersCurrentForeground
            else cfg.lineNumbersForeground
      when (idx == psel.mark.line) do
        drawQuadColor posterSingleton res cfg.lineNumbersCurrentBackground $ quadFromBottomLeftWH 0 y lineNumberWidth height
      drawTextCached cfg.face [(0, 100, numFg)] (Text.pack (show idx)) >>= flip withRefcount \(numTex, numRes) -> do
        drawQuadTexture posterSingleton res numTex $ quadFromBottomLeftWH 5 y numRes.w numRes.h

stripNewLine :: Text -> Text
stripNewLine t = if "\n" `Text.isSuffixOf` t then Text.dropEnd 1 t else t

instance GetBox DefaultEditorWindow where
  getBox dew = do
    s <- readIORef dew.minSize
    pure . drawableBox (Resolution s s) True True $ dew

type Command = DefaultEditorWindow-> IO ()

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
    , movement \doc sel -> do
      end <- (\t -> if t == "" then 0 else Document.lastCharOffset t) . Text.dropEnd 1 <$> Document.getLine sel.mark.line doc
      let mark = Coord sel.mark.line end
      pure . NE.singleton $ Selection mark mark
    )
  , ( [(noneMod, GLFW.Key'D)]
    , \dew -> do
      ivs <- traverse (selToIv dew.document) . NE.toList . (.value) =<< readIORef dew.selections
      Document.patch (fmap (, "") ivs) dew.document
    )
  , ( [(noneMod, GLFW.Key'I)]
    , \dew -> do
      modifyIORef dew.selections (Selections . fmap Selection.turnLeft . (.value))
      enterInsert dew
    )
  , ( [(noneMod, GLFW.Key'A)]
    , \dew -> do
      modifyIORef dew.selections (Selections . fmap Selection.turnRight . (.value))
      sels <- (.value) <$> readIORef dew.selections
      end <- Document.endOfDocument dew.document
      when (any (\s -> s.mark == end) sels) do
        len <- Document.charLengthAt dew.document end
        let c = end { column = end.column + len }
        Document.patch [(Document.Iv c c, "\n")] dew.document
      newSels <- forM sels \sel -> do
        newMark <- Document.moveCoord dew.document 1 sel.mark
        pure sel { mark = newMark }
      writeIORef dew.selections (Selections newSels)
      writeIORef dew.insertExitCallback do
        sels' <- (.value) <$> readIORef dew.selections
        newSels' <- forM sels' \sel' -> do
          if sel'.anchor < sel'.mark
            then do
              newMark <- Document.moveCoord dew.document (-1) sel'.mark
              pure sel' { mark = newMark }
            else
              pure sel'
        writeIORef dew.selections (Selections newSels')
      enterInsert dew
    )
  , ( [(shiftMod, GLFW.Key'5)]
    , movement \doc _ -> do
      end <- Document.endOfDocument doc
      pure . NE.singleton $ Selection (Coord 0 0) end
    )
  , ( [(altMod, GLFW.Key'Semicolon)]
    , movement . const $ pure . NE.singleton . Selection.alternate
    )
  , ( [(noneMod, GLFW.Key'Semicolon)]
    , movement . const $ \sel -> pure . NE.singleton $ sel { anchor = sel.mark }
    )
  , ( [(noneMod, GLFW.Key'Comma)]
    , \dew -> do
      Selections (p NE.:| _) <- readIORef dew.selections
      writeIORef dew.selections . Selections . NE.singleton $ p
    )
  , ( [(altMod, GLFW.Key'S)]
    , movement Selection.splitByLine
    )
  ]
  where
    movement m dew = do
      sels <- (.value) <$> readIORef dew.selections
      newSels <- sconcat <$> traverse (m dew.document) sels
      writeIORef dew.selections (mergeSelections (Selections newSels))
    moveDown ext dew = do
      sels <- (.value) <$> readIORef dew.selections
      newSels <- sconcat <$> traverse moveOne sels
      writeIORef dew.selections (mergeSelections (Selections newSels))
      where
        moveOne sel = do
          numLines <- Document.countLines dew.document
          newSel <- if sel.mark.line /= numLines - 1
            then do
              target <- maybe (getTarget dew sel) pure sel.target
              nextLine <- stripNewLine <$> Document.getLine (sel.mark.line + 1) dew.document
              newCol <- if nextLine == "" then pure 0 else do
                col <- flip withRefcount (\rqNext -> Raqm.positionToIndex rqNext target 0)
                  =<< flip layoutTextCached nextLine . (.face)
                  =<< readIORef config
                pure $ min col (Document.lastCharOffset nextLine)
              let newMark = Coord (sel.mark.line + 1) newCol
              pure $ SelectionWithTarget newMark newMark (Just target)
            else pure $ Selection sel.mark sel.mark
          ext (\_ _ -> pure . NE.singleton $ newSel) dew.document sel
    moveUp ext dew = do
      sels <- (.value) <$> readIORef dew.selections
      newSels <- sconcat <$> traverse moveOne sels
      writeIORef dew.selections (mergeSelections (Selections newSels))
      where
        moveOne sel = do
          newSel <- if sel.mark.line /= 0
            then do
              target <- maybe (getTarget dew sel) pure sel.target
              prevLine <- stripNewLine <$> Document.getLine (sel.mark.line - 1) dew.document
              newCol <- if prevLine == "" then pure 0 else do
                col <- flip withRefcount (\rqPrev -> Raqm.positionToIndex rqPrev target 0)
                  =<< flip layoutTextCached prevLine . (.face)
                  =<< readIORef config
                pure $ min col (Document.lastCharOffset prevLine)
              let newMark = Coord (sel.mark.line - 1) newCol
              pure $ SelectionWithTarget newMark newMark (Just target)
            else pure $ Selection sel.mark sel.mark
          ext (\_ _ -> pure . NE.singleton $ newSel) dew.document sel
    enterInsert dew = do
      writeIORef dew.eatFirstChar True
      writeIORef dew.mode InsertMode

getTarget :: DefaultEditorWindow -> Selection -> IO Int
getTarget dew sel = do
  let mark = sel.mark
  line <- Document.getLine mark.line dew.document
  face <- (.face) <$> readIORef config
  layoutTextCached face (stripNewLine line) >>= flip withRefcount \rq -> do
    after <- if mark.column == Text.lengthWord8 line - 1
      then (+ face.sizePx `div` 2) <$> if mark.column == 0 then pure 0 else Raqm.indexToXPosition rq (mark.column - 1)
      else Raqm.indexToXPosition rq mark.column
    before <- if mark.column == 0
      then pure 0
      else Raqm.indexToXPosition rq (mark.column - 1)
    pure $ (before + after) `div` 2
