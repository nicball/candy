module Box
  ( drawClipping
  , AnyBox
  , hbox
  , vbox
  , hspacer
  , vspacer
  , rigidBox
  , hratioBox
  , vratioBox
  , drawableBox
  , textureBox
  , withPadding
  , frameBox
  ) where

import Control.Monad (foldM, void, when)
import Control.Exception (assert)
import Data.List (intersperse)

import GL (drawQuadTexture, posterSingleton, quadFromTopLeftWH, Resolution(..), Texture, quadToViewport, getSlot, withSlot, viewportSlot, drawQuadFrame, posterSingleton)
import Window (AnyBox(..), Box(..), Draw(..))
import Config (Color)

drawClipping :: Resolution -> AnyBox -> IO ()
drawClipping res (AnyBox box) = do
  let
    minRes = minimumSize box
    boxRes = Resolution
      (if expandableX box then max minRes.w res.w else minRes.w)
      (if expandableY box then max minRes.h res.h else minRes.h)
    quad = quadFromTopLeftWH 0 0 boxRes.w boxRes.h
  vp <- quadToViewport quad <$> getSlot viewportSlot
  withSlot viewportSlot vp (safeDraw boxRes box)

newtype HBox = HBox [AnyBox]

instance Box HBox where
  minimumSize (HBox boxes) = Resolution (sum . fmap (.w) $ boxReses) (maximum . fmap (.h) $ boxReses)
    where boxReses = fmap minimumSize boxes
  expandableX _ = True
  expandableY (HBox boxes) = or . fmap expandableY $ boxes
  safeDraw res hBox@(HBox boxes) = void $ foldM drawSingle 0 boxes'
    where
      numExpandable = length . filter id . fmap expandableX $ boxes
      budget = res.w - (minimumSize hBox).w
      boxes' = if numExpandable > 0 then boxes else intersperse (AnyBox (HSpacer 0 True)) boxes
      numExpandable' = if numExpandable > 0 then numExpandable else length boxes - 1
      expansion = budget `div` numExpandable'
      drawSingle x box = do
        let
          minRes = minimumSize box
          boxRes = Resolution
            (if expandableX box then minRes.w + expansion else minRes.w)
            (if expandableY box then res.h else minRes.h)
          quad = quadFromTopLeftWH x ((res.h - boxRes.h) `div` 2) boxRes.w boxRes.h
        when (boxRes.w /= 0 && boxRes.h /= 0) do
          vp <- quadToViewport quad <$> getSlot viewportSlot
          withSlot viewportSlot vp (safeDraw boxRes box)
        pure $ x + boxRes.w

hbox :: [AnyBox] -> AnyBox
hbox = AnyBox . HBox

data HSpacer = HSpacer Int Bool

instance Box HSpacer where
  minimumSize (HSpacer w _) = Resolution w 0
  expandableX (HSpacer _ x) = x
  expandableY _ = False
  safeDraw _ _ = pure ()

hspacer :: Int -> Bool -> AnyBox
hspacer = fmap AnyBox . HSpacer

newtype VBox = VBox [AnyBox]

instance Box VBox where
  minimumSize (VBox boxes) = Resolution (maximum . fmap (.w) $ boxReses) (sum . fmap (.h) $ boxReses)
    where boxReses = fmap minimumSize boxes
  expandableX (VBox boxes) = or . fmap expandableX $ boxes
  expandableY _ = True
  safeDraw res vBox@(VBox boxes) = void $ foldM drawSingle 0 boxes'
    where
      numExpandable = length . filter id . fmap expandableY $ boxes
      budget = res.h - (minimumSize vBox).h
      boxes' = if numExpandable > 0 then boxes else intersperse (AnyBox (VSpacer 0 True)) boxes
      numExpandable' = if numExpandable > 0 then numExpandable else length boxes - 1
      expansion = budget `div` numExpandable'
      drawSingle y box = do
        let
          minRes = minimumSize box
          boxRes = Resolution
            (if expandableX box then res.w else minRes.w)
            (if expandableY box then minRes.h + expansion else minRes.h)
          quad = quadFromTopLeftWH ((res.w - boxRes.w) `div` 2) y boxRes.w boxRes.h
        when (boxRes.w /= 0 && boxRes.h /= 0) do
          vp <- quadToViewport quad <$> getSlot viewportSlot
          withSlot viewportSlot vp (safeDraw boxRes box)
        pure $ y + boxRes.h

vbox :: [AnyBox] -> AnyBox
vbox = AnyBox . VBox

data VSpacer = VSpacer Int Bool

instance Box VSpacer where
  minimumSize (VSpacer h _) = Resolution 0 h
  expandableX _ = False
  expandableY (VSpacer _ y) = y
  safeDraw _ _ = pure ()

vspacer :: Int -> Bool -> AnyBox
vspacer = fmap AnyBox . VSpacer

data RigidBox = RigidBox Resolution AnyBox

instance Box RigidBox where
  minimumSize (RigidBox res b) = assert (minRes.w <= res.w && minRes.h <= res.h) res
    where minRes = minimumSize b
  expandableX _ = False
  expandableY _ = False
  safeDraw _ (RigidBox res box) = safeDraw res box

rigidBox :: Resolution -> AnyBox -> AnyBox
rigidBox = fmap AnyBox . RigidBox

data HRatioBox = HRatioBox Double AnyBox AnyBox

instance Box HRatioBox where
  minimumSize (HRatioBox r a b) = Resolution
    (max (ceiling (fromIntegral aRes.w / r))
         (ceiling (fromIntegral bRes.w / (1 - r))))
    (max aRes.h bRes.h)
    where
      aRes = minimumSize a
      bRes = minimumSize b
  expandableX (HRatioBox _ a b) = assert (expandableX a && expandableX b) True
  expandableY (HRatioBox _ a b) = expandableY a || expandableY b
  safeDraw res (HRatioBox r a b) = do
    let
      aRes = Resolution (ceiling (fromIntegral res.w * r)) (if expandableY a then res.h else (minimumSize a).h)
      bRes = Resolution (res.w - aRes.w) (if expandableY b then res.h else (minimumSize b).h)
    safeDraw res (HBox [AnyBox (RigidBox aRes a), AnyBox (RigidBox bRes b)])

hratioBox :: Double -> AnyBox -> AnyBox -> AnyBox
hratioBox = fmap (fmap AnyBox) . HRatioBox

data VRatioBox = VRatioBox Double AnyBox AnyBox

instance Box VRatioBox where
  minimumSize (VRatioBox r a b) = Resolution
    (max aRes.w bRes.w)
    (max (ceiling (fromIntegral aRes.h / r))
         (ceiling (fromIntegral bRes.h / (1 - r))))
    where
      aRes = minimumSize a
      bRes = minimumSize b
  expandableX (VRatioBox _ a b) = expandableX a || expandableX b
  expandableY (VRatioBox _ a b) = assert (expandableY a && expandableY b) True
  safeDraw res (VRatioBox r a b) = do
    let
      aRes = Resolution (if expandableX a then res.w else (minimumSize a).w) (ceiling (fromIntegral res.h * r))
      bRes = Resolution (if expandableX b then res.w else (minimumSize b).w) (res.h - aRes.h)
    safeDraw res (VBox [AnyBox (RigidBox aRes a), AnyBox (RigidBox bRes b)])

vratioBox :: Double -> AnyBox -> AnyBox -> AnyBox
vratioBox = fmap (fmap AnyBox) . VRatioBox

data DrawableBox = forall a. Draw a => DrawableBox Resolution Bool Bool a

instance Box DrawableBox where
  minimumSize (DrawableBox s _ _ _) = s
  expandableX (DrawableBox _ x _ _) = x
  expandableY (DrawableBox _ _ y _) = y
  safeDraw res (DrawableBox _ _ _ d) = draw res d

drawableBox :: Draw a => Resolution -> Bool -> Bool -> a -> AnyBox
drawableBox = fmap (fmap (fmap AnyBox)) . DrawableBox

data TextureBox = TextureBox Resolution Texture

instance Box TextureBox where
  minimumSize (TextureBox s _) = s
  expandableX _ = False
  expandableY _ = False
  safeDraw _ (TextureBox res tex) = drawQuadTexture posterSingleton res tex $ quadFromTopLeftWH 0 0 res.w res.h

textureBox :: Resolution -> Texture -> AnyBox
textureBox = fmap AnyBox . TextureBox

withPadding :: Int -> Int -> Int -> Int -> AnyBox -> AnyBox
withPadding top bottom left right box =
  hbox
    [ hspacer left False
    , vbox
      [ vspacer top False
      , box
      , vspacer bottom False
      ]
    , hspacer right False
    ]

data FrameBox = FrameBox Int Color AnyBox

instance Box FrameBox where
  minimumSize (FrameBox t _ b) = Resolution (w + 2 * t) (h + 2 * t)
    where Resolution w h = minimumSize b
  expandableX (FrameBox _ _ b) = expandableX b
  expandableY (FrameBox _ _ b) = expandableY b
  safeDraw res@(Resolution w h) (FrameBox t c b) = do
    drawQuadFrame posterSingleton res c t $ quadFromTopLeftWH 0 0 w h
    let childRes = Resolution (w - 2 * t) (h - 2 * t)
    vp <- quadToViewport (quadFromTopLeftWH t t childRes.w childRes.h) <$> getSlot viewportSlot
    withSlot viewportSlot vp (safeDraw childRes b)

frameBox :: Int -> Color -> AnyBox -> AnyBox
frameBox t c b = AnyBox $ FrameBox t c b
