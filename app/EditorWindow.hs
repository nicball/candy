module EditorWindow
  ( DefaultEditorWindow
  ) where

import Control.Monad (forM, forM_, join, when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty qualified as NE
import Data.List (partition)
import Data.Maybe (maybeToList)
import Data.Semigroup (sconcat)
import Data.Text.Foreign qualified as Text
import Data.Text qualified as Text
import Data.Text (Text)
import Graphics.UI.GLFW qualified as GLFW

import Box (drawableBox)
import Config (ConfigT(..), config)
import Document (Document, Coord(..), DocumentType(..))
import Document qualified
import GL (drawQuadColor, drawQuadTexture, quadFromBottomLeftWH, quadFromTopLeftWH, quadToViewport, viewportSlot, posterSingleton, GLSlot(..), Resolution(..), clearViewport, quadSize, quadFromYXRange, quadOverlap, Quad(..), quadPlus)
import Nexus (ListenerID)
import Nexus qualified
import Refcount (withRefcount)
import Selection qualified
import Selection (Selection(..), Selections(..), translateSelections, mergeSelections, selToIv)
import TextLayout
import Weaver
import Window (Draw(..), EditorWindow(..), Scroll(..), SendChar(..), SendKey(..), Status(..), Mode(..), GetBox(..), pattern GMKNone)

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
    modifyIORef dew.screenYPos (max 0 . (\p -> p - truncate y))

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
        Resolution screenWidth screenHeight <- readIORef dew.lastTextRes
        sel <- NE.head . (.value) <$> readIORef dew.selections
        let opts = defaultLayoutOpt{defaultFont=Just cfg.font, width=Just screenWidth}
        (_, _, lineLayout, lineQuad) <- (!! sel.mark.line) <$> getLineInfo opts dew.document
        markQuad <- (\q -> quadPlus q lineQuad.left lineQuad.top) <$> indexToQuad lineLayout sel.mark.column
        let markXPos = (markQuad.left + markQuad.right) `div` 2
        let markYPos = (markQuad.top + markQuad.bottom) `div` 2
        screenYPos <- readIORef dew.screenYPos
        screenXPos <- readIORef dew.screenXPos
        let
          (topRange, bottomRange) = cfg.cursorVerticalRangeOnScreen
          maxScreenYPos = markYPos - truncate (fromIntegral screenHeight * topRange)
          minScreenYPos = markYPos - truncate (fromIntegral screenHeight * bottomRange)
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

getLineInfo :: LayoutOpt -> Document -> IO [(Int, Text, LineLayout, Quad)]
getLineInfo opts document = do
  totalLines <- Document.countLines document
  pos <- newIORef (-1)
  forM [0 .. totalLines - 1] \idx -> do
    ln <- swapNewLine <$> Document.getLine idx document
    layout <- layoutTextCached opts ln
    res <- layoutRes layout
    p <- readIORef pos
    writeIORef pos (p + res.h)
    pure (idx, ln, layout, quadFromYXRange (p + 1) (p + res.h) 0 (res.w - 1))

instance Draw DefaultEditorWindow where
  draw res dew = do
    cfg <- readIORef config
    clearViewport cfg.background
    let margin = 20
    totalLines <- Document.countLines dew.document
    digitWidth <- (.w) <$> (layoutRes =<< layoutTextCachedDefaultFont cfg.font "0")
    let lineNumberWidth = (+ 10) . (* digitWidth) . max 1 . ceiling . logBase 10 . (+ 1) . (fromIntegral :: Int -> Double) $ totalLines
    drawQuadColor posterSingleton res cfg.lineNumbersBackground $ quadFromTopLeftWH 0 0 lineNumberWidth res.h
    let textQuad = quadFromTopLeftWH (lineNumberWidth + margin) 0 (res.w - lineNumberWidth - margin) res.h
    let textRes = quadSize textQuad
    writeIORef dew.lastTextRes textRes
    writeIORef dew.minSize (lineNumberWidth + margin + 1)
    screenYPos <- readIORef dew.screenYPos
    screenXPos <- readIORef dew.screenXPos
    let screenQuad = quadFromTopLeftWH screenXPos screenYPos textRes.w textRes.h
    let textOpt = defaultLayoutOpt{defaultFont=Just cfg.font, width=Just textRes.w}
    visibleLines <- filter (\(_, _, _, quad) -> quadOverlap quad screenQuad) <$> getLineInfo textOpt dew.document
    sels <- (\(p NE.:| xs) -> (True, p) : fmap (False, ) xs) . (.value) <$> readIORef dew.selections
    psel <- NE.head . (.value) <$> readIORef dew.selections
    oldViewport <- getSlot viewportSlot
    forM_ visibleLines \(lineIdx, lineText, lineLayout, lineQuad) -> do
      setSlot viewportSlot . flip quadToViewport oldViewport $ textQuad
      selRange <- concat <$> forM sels \(p, s) -> do
        range <- maybeToList <$> Selection.selAtLine dew.document s lineIdx
        pure . fmap (\(b, e) -> (p, b, e)) $ range
      let selColorSpec = [ (begin, end, if p then cfg.primarySelectionForeground else cfg.secondarySelectionForeground) | (p, begin, end) <- selRange ]
      let cursorRange = [ (p, s.mark.column) | (p, s) <- sels, s.mark.line == lineIdx ]
      let cursorColorSpec = [ (pos, pos, if p then cfg.primaryCursorForeground else cfg.secondaryCursorForeground) | (p, pos) <- cursorRange ]
      let quads = fmap (\(p, begin, end) -> (begin, end, if p then cfg.primarySelectionBackground else cfg.secondarySelectionBackground)) selRange
            ++ fmap (\(p, pos) -> (pos, pos, if p then cfg.primaryCursorBackground else cfg.secondaryCursorBackground)) cursorRange
      let screenLineQuad = quadPlus lineQuad (-screenQuad.left) (-screenQuad.top)
      forM_ quads \(begin, end, color) -> do
        forM_ [begin .. end] \byteOff -> do
          charQuad <- indexToQuad lineLayout byteOff
          let quad = quadPlus charQuad screenLineQuad.left screenLineQuad.top
          drawQuadColor posterSingleton textRes color quad
      drawTextCached textOpt (cursorColorSpec ++ selColorSpec ++ [(0, Text.lengthWord8 lineText - 1, cfg.foreground)]) lineText >>= flip withRefcount \(lnTex, _) -> do
        drawQuadTexture posterSingleton textRes lnTex screenLineQuad
      setSlot viewportSlot oldViewport
      let numFg = if lineIdx == psel.mark.line
            then cfg.lineNumbersCurrentForeground
            else cfg.lineNumbersForeground
      let numQuad = quadFromYXRange screenLineQuad.top screenLineQuad.bottom 0 (lineNumberWidth - 1)
      when (lineIdx == psel.mark.line) do
        drawQuadColor posterSingleton res cfg.lineNumbersCurrentBackground numQuad
      drawTextCachedDefaultFont cfg.font [(0, 100, numFg)] (Text.pack (show lineIdx)) >>= flip withRefcount \(numTex, numRes) -> do
        drawQuadTexture posterSingleton res numTex $ quadFromBottomLeftWH 5 numQuad.bottom numRes.w numRes.h

swapNewLine :: Text -> Text
swapNewLine t = if "\n" `Text.isSuffixOf` t then Text.dropEnd 1 t <> " " else t

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
              nextLine <- swapNewLine <$> Document.getLine (sel.mark.line + 1) dew.document
              newCol <- (\layout -> positionToIndex layout target 0)
                =<< flip layoutTextCachedDefaultFont nextLine . (.font)
                =<< readIORef config
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
              prevLine <- swapNewLine <$> Document.getLine (sel.mark.line - 1) dew.document
              newCol <- (\layout -> positionToIndex layout target 0)
                =<< flip layoutTextCachedDefaultFont prevLine . (.font)
                =<< readIORef config
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
  font <- (.font) <$> readIORef config
  layout <- layoutTextCachedDefaultFont font line
  quad <- indexToQuad layout mark.column
  pure $ (quad.left + quad.right) `div` 2
