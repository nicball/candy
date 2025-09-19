{-# LANGUAGE PatternSynonyms #-}

module Window
  ( WindowID
  , Scroll(..)
  , SendKey(..)
  , SendChar(..)
  , Draw(..)
  , WindowManager(..)
  , DefaultWindowManager
  , withDefaultWindowManager
  , DefaultEditorWindow
  , withDefaultEditorWindow
  , DefaultBar
  , withDefaultBar
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

import Config 
import Document 
import GL 
import Raqm qualified
import Selection 
import Weaver 

import Data.List (partition)

class Scroll a where
  scroll :: Double -> Double -> a -> IO ()

class SendKey a where
  sendKey :: GLFW.Key -> GLFW.ModifierKeys -> a -> IO ()

class SendChar a where
  sendChar :: Char -> a -> IO ()

class Draw a where
  draw :: Resolution -> a -> IO ()

newtype WindowID = WindowID Int

class (Scroll a, SendKey a, SendChar a, Draw a) => WindowManager a where
  registerEditorWindow :: EditorWindow w => w -> a -> IO WindowID
  setBar :: Bar w => w -> a -> IO ()

class (Scroll a, SendKey a, SendChar a, Draw a) => EditorWindow a where
  getContext :: a -> IO Context

class Draw a => Bar a where
  setContext :: a -> Context -> IO ()

data DefaultWindowManager = DefaultWindowManager
  { windows :: IORef (IntMap.IntMap WrapEditorWindow)
  , bar :: IORef (Maybe WrapBar)
  , poster :: Poster
  , config :: Config
  }

data WrapEditorWindow = forall w. EditorWindow w => WrapEditorWindow w
data WrapBar = forall w. Bar w => WrapBar w

withDefaultWindowManager :: Config -> (DefaultWindowManager -> IO a) -> IO a
withDefaultWindowManager config action = do
  windows <- newIORef IntMap.empty
  bar <- newIORef Nothing
  withPoster \poster -> do
    action DefaultWindowManager{..}

instance Scroll DefaultWindowManager where
  scroll x y dwm = readIORef dwm.windows >>= mapM_ (\case WrapEditorWindow w -> scroll x y w) . IntMap.elems

instance SendKey DefaultWindowManager where
  sendKey key mods dwm = readIORef dwm.windows >>= mapM_ (\case WrapEditorWindow w -> sendKey key mods w) . IntMap.elems

instance SendChar DefaultWindowManager where
  sendChar char dwm = readIORef dwm.windows >>= mapM_ (\case WrapEditorWindow w -> sendChar char w) . IntMap.elems

instance Draw DefaultWindowManager where
  draw res dwm = do
    [(0, onlyWin)] <- IntMap.assocs <$> readIORef dwm.windows
    let margin = 20
    lineHeight <- getLineHeight =<< getFaceCached dwm.config.face
    case onlyWin of
      WrapEditorWindow win -> withObject \editorTex -> do
        readIORef dwm.bar >>= \case
          Nothing -> do
            let editorRes = Resolution (res.w - margin * 2) (res.h - margin * 2)
            renderToTexture editorRes editorTex (draw editorRes win)
            drawQuadTexture dwm.poster res editorTex $ quadFromTopLeftWH margin margin editorRes.w editorRes.h
          Just (WrapBar bar) -> do
            let barRes = Resolution (res.w - margin * 2) (lineHeight + 10)
            let editorRes = Resolution (res.w - margin * 2) (res.h - margin * 2 - barRes.h - margin)
            setContext bar =<< getContext win
            withObject \barTex -> do
              renderToTexture barRes barTex (draw barRes bar)
              renderToTexture editorRes editorTex (draw editorRes win)
              drawQuadTexture dwm.poster res barTex $ quadFromTopLeftWH margin margin barRes.w barRes.h
              drawQuadTexture dwm.poster res editorTex $ quadFromBottomLeftWH margin (res.h - margin - 1) editorRes.w editorRes.h

instance WindowManager DefaultWindowManager where
  registerEditorWindow w dwm = do
    wid <- IntMap.size <$> readIORef dwm.windows
    modifyIORef dwm.windows (IntMap.insert wid (WrapEditorWindow w))
    pure (WindowID wid)
  setBar w dwm = writeIORef dwm.bar . Just . WrapBar $ w

data DefaultEditorWindow = DefaultEditorWindow
  { screenXPos :: IORef Int
  , screenYPos :: IORef Int
  , weaver :: Weaver
  , context :: Context
  , keyMatchState :: IORef KeyCandidates
  , poster :: Poster
  , lastTextRes :: IORef Resolution
  }

data Mode = NormalMode | InsertMode
  deriving (Eq, Show)

withDefaultEditorWindow :: Config -> (DefaultEditorWindow -> IO a) -> IO a
withDefaultEditorWindow config action = do
  screenXPos <- newIORef 0
  screenYPos <- newIORef 0
  document <- newIORef . Document.fromText =<< Text.readFile "./app/Window.hs"
  selection <- newIORef (Selection (Coord 0 0) (Coord 0 0))
  mode <- newIORef NormalMode
  eatFirstChar <- newIORef False
  insertExitCallback <- newIORef (pure ())
  lastTextRes <- newIORef $ Resolution 0 0
  let context = Context{..}
  keyMatchState <- newIORef normalKeymap.candidates
  withWeaver config \weaver -> do
    withPoster \poster -> do
      action DefaultEditorWindow{..}

instance Scroll DefaultEditorWindow where
  scroll _ y dew = do
    height <- getLineHeight =<< getFaceCached dew.context.config.face
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
              writeIORef dew.context.selection $ Selection (translate sel.anchor) (translate sel.mark)
          (GMKNone, GLFW.Key'Delete) -> do
            sel <- readIORef dew.context.selection
            doc <- readIORef dew.context.document
            when (sel.mark /= Document.endOfDocument doc) do
              let
                len = Document.charLengthAt doc sel.mark
                (newDoc, translate) = Document.patch (Document.Iv sel.mark sel.mark { column = sel.mark.column + len }) "" doc
              writeIORef dew.context.document newDoc
              writeIORef dew.context.selection $ Selection (translate sel.anchor) (translate sel.mark)
          (GMKNone, GLFW.Key'Enter) -> sendChar '\n' dew
          _ -> pure ()
    ensureCursorRangeOnScreen
    where
      ensureCursorRangeOnScreen = do
        sel <- readIORef dew.context.selection
        height <- getLineHeight =<< getFaceCached dew.context.config.face
        let markYPos = height * sel.mark.line
        markXPos <- getTarget dew.context
        screenYPos <- readIORef dew.screenYPos
        screenXPos <- readIORef dew.screenXPos
        Resolution screenWidth screenHeight <- readIORef dew.lastTextRes
        let
          (topRange, bottomRange) = dew.context.config.cursorVerticalRangeOnScreen
          maxScreenYPos = markYPos - truncate (fromIntegral screenHeight * topRange)
          minScreenYPos = markYPos - truncate (fromIntegral screenHeight * bottomRange) + height
          (leftRange, rightRange) = dew.context.config.cursorHorizontalRangeOnScreen
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
        writeIORef dew.context.selection $ Selection (translate sel.anchor) (translate sel.mark)
    where
      eatFirstChar action =
        readIORef dew.context.eatFirstChar >>= \case
          True -> writeIORef dew.context.eatFirstChar False
          False -> action

instance Draw DefaultEditorWindow where
  draw res dew = do
    let Color{..} = dew.context.config.background in
      glClearColor red green blue alpha
    glClear GL_COLOR_BUFFER_BIT
    document <- readIORef dew.context.document
    ftFace <- getFaceCached dew.context.config.face
    height <- getLineHeight ftFace
    let margin = 20
    digitWidth <- (`div` 64) . (.xAdvance) . (!! 0) <$> (Raqm.getGlyphs =<< layoutTextCached dew.context.config.face "0")
    let lineNumberWidth = (+ 10) . (* digitWidth) . max 1 . ceiling . logBase 10 . (+ 1) . (fromIntegral :: Int -> Double) . Document.countLines $ document
    drawQuadColor dew.poster res dew.context.config.lineNumbersBackground $ quadFromTopLeftWH 0 0 lineNumberWidth res.h
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
              , dew.context.config.primarySelectionForeground
              ))
            selRange
      let cursorRange = [ sel.mark.column | idx == sel.mark.line ]
      let cursorColorSpec = fmap
            (\pos ->
              ( pos
              , pos + Document.charLengthAt document (Coord idx pos)
              , dew.context.config.primaryCursorForeground
              ))
            cursorRange
      let quads = fmap (\(begin, end) -> (begin, end, dew.context.config.primarySelectionBackground)) selRange
            ++ fmap (\pos -> (pos, pos, dew.context.config.primaryCursorBackground)) cursorRange
      forM_ quads \(begin, end, color) -> do
        rq <- layoutTextCached dew.context.config.face ln
        xBegin <- if begin == 0 then pure 0 else Raqm.indexToXPosition rq (begin - 1)
        xEnd <- if end == Text.lengthWord8 ln
          then (+ dew.context.config.face.sizePx `div` 2) <$> if end == 0 then pure 0 else Raqm.indexToXPosition rq (end - 1)
          else Raqm.indexToXPosition rq end
        drawQuadColor dew.poster textRes color $ quadFromBottomLeftWH (xBegin - screenXPos) y (xEnd - xBegin + 1) height
      when (not . Text.null $ ln) do
        (lnTex, lnRes) <- drawTextCached dew.weaver (cursorColorSpec ++ selColorSpec ++ [(0, Text.lengthWord8 ln, dew.context.config.foreground)]) ln
        drawQuadTexture dew.poster textRes lnTex $ quadFromBottomLeftWH (-screenXPos) y lnRes.w lnRes.h
      setSlot viewportSlot $ Viewport 0 0 res.w res.h
      let numFg = if idx == sel.mark.line
            then dew.context.config.lineNumbersCurrentForeground
            else dew.context.config.lineNumbersForeground
      when (idx == sel.mark.line) do
        drawQuadColor dew.poster res dew.context.config.lineNumbersCurrentBackground $ quadFromBottomLeftWH 0 y lineNumberWidth height
      (numTex, numRes) <- drawTextCached dew.weaver [(0, 100, numFg)] (Text.pack (show idx))
      drawQuadTexture dew.poster res numTex $ quadFromBottomLeftWH 5 y numRes.w numRes.h

instance EditorWindow DefaultEditorWindow where
  getContext w = pure w.context

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

data DefaultBar = DefaultBar
  { context :: IORef (Maybe Context)
  , config :: Config
  , weaver :: Weaver
  , poster :: Poster
  }

withDefaultBar :: Config -> (DefaultBar -> IO a) -> IO a
withDefaultBar config action = do
  context <- newIORef Nothing
  withWeaver config \weaver -> do
    withPoster \poster -> do
      action DefaultBar{..}

instance Draw DefaultBar where
  draw res bar = do
    let Color{..} = bar.config.barBackground in
      glClearColor red green blue alpha
    glClear GL_COLOR_BUFFER_BIT
    readIORef bar.context >>= maybe (pure ()) \context -> do
      mode <- readIORef context.mode
      sel <- readIORef context.selection
      let text = Text.pack $ show mode <> " " <> show sel
      (ctxTex, ctxRes) <- drawTextCached bar.weaver [(0, Text.lengthWord8 text, bar.config.barForeground)] text
      drawQuadTexture bar.poster res ctxTex $ quadFromTopLeftWH 5 5 ctxRes.w ctxRes.h
      pure ()

instance Bar DefaultBar where
  setContext w ctx = writeIORef w.context (Just ctx)
