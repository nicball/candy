{-# LANGUAGE PatternSynonyms #-}

module Selection
  ( Selection(.., Selection)
  , alternate
  , selMin
  , selMax
  , selAtLine
  , selToIv
  , ivToSel
  , extend
  , moveLeft
  , moveRight
  , selectToWordEnd
  , selectToWordStart
  , selectToWordBegin
  , expandToLine
  , splitByLine
  , turnLeft
  , turnRight
  , Selections(..)
  , translateSelections
  , mergeSelections
  ) where

import Control.Monad (forM)
import Data.Char qualified as Char
import Data.Maybe (listToMaybe, fromJust)
import Data.Text.ICU qualified as ICU
import Data.Text.Foreign qualified as Text
import Data.Text qualified as Text
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.List (partition, sortOn)

import Document (breakAfterCoord, breakBeforeCoord, charBreaker, countBreaks, moveCoord, wordBreaker, Coord(..), Document, lastCharOffset, Iv(..), countLines, lastCharOffset, charLengthAt)
import Document qualified

data Selection = SelectionWithTarget { anchor :: Coord, mark :: Coord, target :: Maybe Int }

{-# COMPLETE Selection #-}
pattern Selection :: Coord -> Coord -> Selection
pattern Selection anchor mark <- SelectionWithTarget anchor mark _
  where Selection a m = SelectionWithTarget a m Nothing

instance Show Selection where
  show sel
    | sel.anchor == sel.mark = show sel.mark
    | otherwise = show sel.anchor <> "-" <> show sel.mark

alternate :: Selection -> Selection
alternate (Selection a b) = Selection b a

selMin :: Selection -> Coord
selMin (Selection a b) = min a b

selMax :: Selection -> Coord
selMax (Selection a b) = max a b

selToIv :: Document -> Selection -> IO Iv
selToIv doc sel = do
  let m@(Coord line _) = selMax sel
  next <- moveCoord doc 1 m
  end <- if next == m
    then Coord line . Text.lengthWord8 <$> Document.getLine line doc
    else pure next
  pure $ Iv (selMin sel) end

ivToSel :: Document -> Iv -> IO Selection
ivToSel doc iv = do
  let Coord line col = iv.end
  lineText <- Document.getLine line doc
  numLines <- countLines doc
  end <- if line == numLines - 1 && col == Text.lengthWord8 lineText
    then pure . Coord line . lastCharOffset $ lineText
    else moveCoord doc (-1) iv.end
  pure $ Selection iv.begin end

selAtLine :: Document -> Selection -> Int -> IO (Maybe (Int, Int))
selAtLine _ (Selection (Coord aln acol) (Coord mln mcol)) ln | aln == mln =
  if aln == ln
    then pure . Just $ (min acol mcol, max acol mcol)
    else pure Nothing
selAtLine doc sel ln = if
  | bln == ln -> Just . (bcol, ) . lastCharOffset <$> Document.getLine bln doc
  | bln < ln && ln < eln -> Just . (0, ) . lastCharOffset <$> Document.getLine ln doc
  | ln == eln -> pure . Just $ (0, ecol)
  | otherwise -> pure Nothing
  where
    (Coord bln bcol, Coord eln ecol) = (selMin sel, selMax sel)

type Movement = Document -> Selection -> IO (NonEmpty Selection)

extend :: Movement -> Movement
extend move doc sel = do
  sels <- move doc sel
  pure $ fmap (\s -> s { anchor = sel.anchor }) sels

moveRight :: Movement
moveRight doc sel = do
  c <- moveCoord doc 1 sel.mark
  pure . NE.singleton $ Selection c c

moveLeft :: Movement
moveLeft doc sel = do
  c <- moveCoord doc (-1) sel.mark
  pure . NE.singleton $ Selection c c

selectToWordEnd :: Movement
selectToWordEnd doc SelectionWithTarget{mark = curr} =
  findAnchor <$> breakAfterCoord wordBreaker doc curr >>= \case
    Nothing -> pure . NE.singleton $ Selection curr curr
    Just anchor -> NE.singleton . Selection anchor . breakEndCoord . findMark <$> breakAfterCoord wordBreaker doc anchor
  where
    isSpace = Text.all (\c -> Char.isSpace c && c /= '\n')
    findAnchor (x : xs) = if (<= 1) . countBreaks charBreaker . ICU.brkBreak . fst $ x
      then fmap snd . listToMaybe . dropWhile ((== "\n") . ICU.brkBreak . fst) $ xs
      else Just . snd $ x
    findAnchor [] = undefined
    findMark [x] = x
    findMark (x : y : xs)
      | isSpace (ICU.brkBreak (fst x)) = if ICU.brkBreak (fst y) == "\n" then x else findMark (y : xs)
      | ICU.brkBreak (fst x) == "\n" = findMark (y : xs)
      | otherwise = x
    findMark [] = undefined

selectToWordStart :: Movement
selectToWordStart doc SelectionWithTarget{mark = curr} =
  findAnchor <$> breakAfterCoord wordBreaker doc curr >>= \case
    Nothing -> pure . NE.singleton $ Selection curr curr
    Just anchor -> NE.singleton . Selection anchor . breakEndCoord . findMark <$> breakAfterCoord wordBreaker doc anchor
  where
    isSpace = Text.all (\c -> Char.isSpace c && c /= '\n')
    findAnchor (x : xs) = if (<= 1) . countBreaks charBreaker . ICU.brkBreak . fst $ x
      then fmap snd . listToMaybe . dropWhile ((== "\n") . ICU.brkBreak . fst) $ xs
      else Just . snd $ x
    findAnchor [] = undefined
    findMark [x] = x
    findMark (x : y : _)
      | isSpace (ICU.brkBreak (fst y)) = y
      | otherwise = x
    findMark [] = undefined

selectToWordBegin :: Movement
selectToWordBegin doc SelectionWithTarget{mark = curr} =
  findAnchor <$> (breakBeforeCoord wordBreaker doc =<< forceNext curr) >>= \case
    Nothing -> pure . NE.singleton $ Selection curr curr
    Just anchor -> findMark <$> (breakBeforeCoord wordBreaker doc =<< forceNext anchor) >>= \case
      Just (_, mark) -> pure . NE.singleton $ Selection anchor mark
      Nothing -> pure . NE.singleton $ Selection anchor anchor
  where
    forceNext c = Coord c.line . (c.column +) <$> charLengthAt doc c
    isSpace = Text.all (\c -> Char.isSpace c && c /= '\n')
    findAnchor (x : xs) = if (<= 1) . countBreaks charBreaker . ICU.brkBreak . fst $ x
      then fmap breakEndCoord . listToMaybe . dropWhile ((== "\n") . ICU.brkBreak . fst) $ xs
      else Just . breakEndCoord $ x
    findAnchor [] = Nothing
    findMark [x] = Just x
    findMark (x : y : xs)
      | isSpace (ICU.brkBreak (fst x)) = if ICU.brkBreak (fst y) == "\n" then Just x else findMark (y : xs)
      | ICU.brkBreak (fst x) == "\n" = findMark (y : xs)
      | otherwise = Just x
    findMark [] = Nothing

expandToLine :: Movement
expandToLine doc sel =
  if sel.anchor <= sel.mark
  then NE.singleton . Selection (Coord sel.anchor.line 0) . Coord sel.mark.line . lastCharOffset <$> Document.getLine sel.mark.line doc
  else NE.singleton . flip Selection (Coord sel.mark.line 0) . Coord sel.anchor.line . lastCharOffset <$> Document.getLine sel.anchor.line doc

splitByLine :: Movement
splitByLine doc sel = setPrim <$> forM (NE.fromList [(selMin sel).line .. (selMax sel).line]) \ln -> do
    (a, b) <- fromJust <$> selAtLine doc sel ln
    pure . turn $ Selection (Coord ln a) (Coord ln b)
    where
      turn = if sel.anchor <= sel.mark then id else turnLeft
      setPrim xs = if sel.anchor <= sel.mark then NE.last xs NE.:| NE.init xs else xs

turnLeft :: Selection -> Selection
turnLeft (Selection anchor mark) | anchor < mark = Selection mark anchor
turnLeft sel = sel

turnRight :: Selection -> Selection
turnRight (Selection anchor mark) | mark < anchor = Selection mark anchor
turnRight sel = sel

breakEndCoord :: (ICU.Break a, Coord) -> Coord
breakEndCoord (brk, Coord line col) = Coord line (col + lastCharOffset (ICU.brkBreak brk))

newtype Selections = Selections { value :: NonEmpty Selection }

translateSelections :: (Coord -> Coord) -> Selections -> Selections
translateSelections f (Selections s) = mergeSelections . Selections . fmap (\(Selection a m) -> Selection (f a) (f m)) $ s

mergeSelections :: Selections -> Selections
mergeSelections = fromList . merge [] . toList
  where
    toList (Selections (p NE.:| ss)) = sortOn (selMin . snd) $ (True, p) : fmap (False, ) ss
    fromList ls = case partition fst $ ls of
      ([(_, p)], ss) -> Selections $ p NE.:| fmap snd ss
      _ -> undefined
    merge [] (s : ss) = merge [s] ss
    merge ((lp, l) : ls) ((sp, s) : ss) = case mergeSel l s of
      Nothing -> merge ((sp, s) : (lp, l) : ls) ss
      Just l' -> merge ((lp || sp, l') : ls) ss
    merge ss [] = reverse ss
    mergeSel a b =
      if selMax a >= selMin b
        then Just $ turn (Selection (selMin a) (selMax b))
        else Nothing
      where turn = if a.anchor > a.mark then turnLeft else id

instance Show Selections where
  show (Selections sels@(p NE.:| xs)) = show p ++ num
    where
      num
        | null xs = ""
        | otherwise = "(" ++ show (NE.length sels) ++ ")"
