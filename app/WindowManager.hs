module WindowManager
  ( DefaultWindowManager
  , withDefaultWindowManager
  ) where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Graphics.UI.GLFW as GLFW

import Config (Config(..), config)
import Window (Bar(..), EditorWindow(..), WindowManager(..), Draw(..), SendChar(..), SendKey(..), Scroll(..), GetBox(..), pattern GMKCtrl)
import Box (AnyBox, drawClipping, hbox, hratioBox, hspacer, vbox, vratioBox, vspacer)

data DefaultWindowManager = DefaultWindowManager
  { layout :: IORef Tree
  , focus :: IORef Path
  , bar :: IORef (Maybe WrapBar)
  }

data WrapEditorWindow = forall w. EditorWindow w => WrapEditorWindow w
data WrapBar = forall w. Bar w => WrapBar w

withDefaultWindowManager :: EditorWindow w => w -> (DefaultWindowManager -> IO a) -> IO a
withDefaultWindowManager win action = do
  layout <- newIORef (Leaf (WrapEditorWindow win))
  focus <- newIORef []
  bar <- newIORef Nothing
  action DefaultWindowManager{..}

getFocusedWindow :: DefaultWindowManager -> IO WrapEditorWindow
getFocusedWindow dwm = do
  layout <- readIORef dwm.layout
  path <- readIORef dwm.focus
  pure . fromLeaf . getPath path $ layout

instance Scroll DefaultWindowManager where
  scroll x y dwm = getFocusedWindow dwm >>= \case WrapEditorWindow w -> scroll x y w

instance SendKey DefaultWindowManager where
  sendKey key mods dwm = do
    layout <- readIORef dwm.layout
    case (mods, key) of
      (GMKCtrl True, GLFW.Key'H) -> modifyIORef dwm.focus (movePath West layout)
      (GMKCtrl True, GLFW.Key'J) -> modifyIORef dwm.focus (movePath South layout)
      (GMKCtrl True, GLFW.Key'K) -> modifyIORef dwm.focus (movePath North layout)
      (GMKCtrl True, GLFW.Key'L) -> modifyIORef dwm.focus (movePath East layout)
      (GMKCtrl True, GLFW.Key'F) -> do
        getFocusedWindow dwm >>= \(WrapEditorWindow focused) -> do
          fork focused >>= flip addEditorWindow dwm
      _ -> getFocusedWindow dwm >>= \case WrapEditorWindow w -> sendKey key mods w

instance SendChar DefaultWindowManager where
  sendChar char dwm = getFocusedWindow dwm >>= \case WrapEditorWindow w -> sendChar char w

instance Draw DefaultWindowManager where
  draw res dwm = do
    layout <- readIORef dwm.layout
    focus <- readIORef dwm.focus
    layoutBox <- getLayoutBox layout (Just focus)
    readIORef dwm.bar >>= \case
      Nothing -> drawClipping res layoutBox
      Just (WrapBar bar) -> do
        setStatus bar =<< (\(WrapEditorWindow win) -> getStatus win) =<< getFocusedWindow dwm
        barBox <- getBox bar
        drawClipping res (vbox [barBox, layoutBox])

getLayoutBox :: Tree -> Maybe Path -> IO AnyBox
getLayoutBox (Leaf (WrapEditorWindow win)) _ = getBox win -- TODO: draw border
getLayoutBox (Branch Vertical ratio north south) focus = do
  margin <- (.tilingMargin) <$> readIORef config
  northBox <- getLayoutBox north (moveAlong North focus)
  southBox <- getLayoutBox south (moveAlong South focus)
  pure $ vratioBox ratio northBox (vbox [vspacer margin False, southBox])
getLayoutBox (Branch Horizontal ratio west east) focus = do
  margin <- (.tilingMargin) <$> readIORef config
  westBox <- getLayoutBox west (moveAlong West focus)
  eastBox <- getLayoutBox east (moveAlong East focus)
  pure $ hratioBox ratio westBox (hbox [hspacer margin False, eastBox])

moveAlong :: Direction -> Maybe Path -> Maybe Path
moveAlong dir (Just (p : ps)) | p == dir = Just ps
moveAlong _ _ = Nothing

instance WindowManager DefaultWindowManager where
  addEditorWindow w dwm = do
    layout <- readIORef dwm.layout
    focus <- readIORef dwm.focus
    let
      next North = East
      next East = South
      next South = West
      next West = North
      dir = if null focus then East else next (last focus)
    writeIORef dwm.layout (insertAt focus dir (WrapEditorWindow w)layout)
    writeIORef dwm.focus (focus ++ [ dir ])
  setBar w dwm = writeIORef dwm.bar . Just . WrapBar $ w

data Tree = Leaf WrapEditorWindow | Branch Orientation Ratio Tree Tree
data Orientation = Horizontal | Vertical
type Ratio = Double
data Direction = North | East | South | West deriving Eq
type Path = [Direction]

movePath :: Direction -> Tree -> Path -> Path
movePath dir tree path = dive . turn . dropWhile (/= opposite dir) . reverse $ path
  where
    opposite North = South
    opposite South = North
    opposite West = East
    opposite East = West
    turn (_ : xs) = reverse (dir : xs)
    turn [] = path
    dive prefix = prefix ++ most (opposite dir) (getPath prefix tree)
    most _ (Leaf _) = []
    most North (Branch Vertical   _ north _) = North : most North north
    most North (Branch Horizontal _ west _)  = West  : most North west
    most South (Branch Vertical   _ _ south) = South : most South south
    most South (Branch Horizontal _ west _)  = West  : most South west
    most West  (Branch Vertical   _ north _) = North : most West  north
    most West  (Branch Horizontal _ west _)  = West  : most West  west
    most East  (Branch Vertical   _ north _) = North : most East  north
    most East  (Branch Horizontal _ _ east)  = East  : most East  east

fromLeaf :: Tree -> WrapEditorWindow
fromLeaf (Leaf  w) = w
fromLeaf _ = undefined

getPath :: Path -> Tree -> Tree
getPath [] t = t
getPath (North : ps) (Branch Vertical   _ t _) = getPath ps t
getPath (South : ps) (Branch Vertical   _ _ t) = getPath ps t
getPath (West  : ps) (Branch Horizontal _ t _) = getPath ps t
getPath (East  : ps) (Branch Horizontal _ _ t) = getPath ps t
getPath _ _ = undefined

insertAt :: Path -> Direction -> WrapEditorWindow -> Tree -> Tree
insertAt [] dir w node = split dir
  where
    split North = Branch Vertical   0.5 (Leaf w) node
    split South = Branch Vertical   0.5 node (Leaf w)
    split West  = Branch Horizontal 0.5 (Leaf w) node
    split East  = Branch Horizontal 0.5 node (Leaf w)
insertAt (North : ps) dir win (Branch Vertical   r n s) = Branch Vertical   r (insertAt ps dir win n) s
insertAt (South : ps) dir win (Branch Vertical   r n s) = Branch Vertical   r n (insertAt ps dir win s)
insertAt (West  : ps) dir win (Branch Horizontal r w e) = Branch Horizontal r (insertAt ps dir win w) e
insertAt (East  : ps) dir win (Branch Horizontal r w e) = Branch Horizontal r w (insertAt ps dir win e)
insertAt _ _ _ _ = undefined
