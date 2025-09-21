module WindowManager
  ( DefaultWindowManager
  , withDefaultWindowManager
  ) where

import Data.IntMap qualified as IntMap
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)

import Config (Config(..), config)
import GL (Poster, Resolution(..), withObject, renderToTexture, withPoster, quadFromTopLeftWH, quadFromBottomLeftWH, drawQuadTexture)
import Weaver (getLineHeight, getFaceCached)
import Window (Bar(..), EditorWindow(..), WindowManager(..), WindowID(..), Draw(..), SendChar(..), SendKey(..), Scroll(..))

data DefaultWindowManager = DefaultWindowManager
  { windows :: IORef (IntMap.IntMap WrapEditorWindow)
  , counter :: IORef Int
  , bar :: IORef (Maybe WrapBar)
  , poster :: Poster
  }

data WrapEditorWindow = forall w. EditorWindow w => WrapEditorWindow w
data WrapBar = forall w. Bar w => WrapBar w

withDefaultWindowManager :: (DefaultWindowManager -> IO a) -> IO a
withDefaultWindowManager action = do
  windows <- newIORef IntMap.empty
  counter <- newIORef 0
  bar <- newIORef Nothing
  withPoster \poster -> do
    action DefaultWindowManager{..}

getFocusedWindow :: DefaultWindowManager -> IO WrapEditorWindow
getFocusedWindow dwm = (IntMap.! 0) <$> readIORef dwm.windows

instance Scroll DefaultWindowManager where
  scroll x y dwm = getFocusedWindow dwm >>= \case WrapEditorWindow w -> scroll x y w

instance SendKey DefaultWindowManager where
  sendKey key mods dwm = getFocusedWindow dwm >>= \case WrapEditorWindow w -> sendKey key mods w

instance SendChar DefaultWindowManager where
  sendChar char dwm = getFocusedWindow dwm >>= \case WrapEditorWindow w -> sendChar char w

instance Draw DefaultWindowManager where
  draw res dwm = do
    [(0, left), (1, right)] <- IntMap.assocs <$> readIORef dwm.windows
    let margin = 20
    lineHeight <- getLineHeight =<< getFaceCached . (.face) =<< readIORef config
    case (left, right) of
      (WrapEditorWindow leftWin, WrapEditorWindow rightWin) -> do
        readIORef dwm.bar >>= \case
          Nothing -> do
            let editorRes = Resolution ((res.w - margin * 3) `div` 2) (res.h - margin * 2)
            withObject \leftTex -> withObject \rightTex -> do
              renderToTexture editorRes leftTex (draw editorRes leftWin)
              drawQuadTexture dwm.poster res leftTex $ quadFromTopLeftWH margin margin editorRes.w editorRes.h
              renderToTexture editorRes rightTex (draw editorRes rightWin)
              drawQuadTexture dwm.poster res rightTex $ quadFromTopLeftWH (2 * margin + editorRes.w) margin editorRes.w editorRes.h
          Just (WrapBar bar) -> do
            let barRes = Resolution (res.w - margin * 2) (lineHeight + 10)
            let editorRes = Resolution ((res.w - margin * 3) `div` 2) (res.h - margin * 2 - barRes.h - margin)
            setStatus bar =<< (\(WrapEditorWindow win) -> getStatus win) =<< getFocusedWindow dwm
            withObject \barTex -> do
              renderToTexture barRes barTex (draw barRes bar)
              drawQuadTexture dwm.poster res barTex $ quadFromTopLeftWH margin margin barRes.w barRes.h
              withObject \leftTex -> withObject \rightTex -> do
                renderToTexture editorRes leftTex (draw editorRes leftWin)
                drawQuadTexture dwm.poster res leftTex $ quadFromBottomLeftWH margin (res.h - margin - 1) editorRes.w editorRes.h
                renderToTexture editorRes rightTex (draw editorRes rightWin)
                drawQuadTexture dwm.poster res rightTex $ quadFromBottomLeftWH (2 * margin + editorRes.w) (res.h - margin - 1) editorRes.w editorRes.h

instance WindowManager DefaultWindowManager where
  registerEditorWindow w dwm = do
    wid <- readIORef dwm.counter
    modifyIORef dwm.counter (+ 1)
    modifyIORef dwm.windows (IntMap.insert wid (WrapEditorWindow w))
    pure (WindowID wid)
  setBar w dwm = writeIORef dwm.bar . Just . WrapBar $ w

data Tree = Leaf WrapEditorWindow | Node Orientation Ratio Tree Tree
data Orientation = Horizontal | Vertical
type Ratio = Double

