{-# LANGUAGE MultiWayIf #-}

module Selection
  ( Selection(..)
  , alternate
  , selMin
  , selMax
  , selAtLine
  , extend
  , moveLeft
  , moveRight
  , selectToWordEnd
  , selectToWordStart
  ) where

import Data.Char qualified as Char
import Data.Maybe (listToMaybe)
import Data.Text.Foreign qualified as Text
import Data.Text.ICU qualified as ICU
import Data.Text qualified as Text
import Data.Text (Text)

import Document (breakAfterCoord, charBreaker, countBreaks, moveCoord, wordBreaker, Coord(..), Document)
import Document qualified

data Selection = Selection { anchor :: Coord, mark :: Coord }
  deriving Show

alternate :: Selection -> Selection
alternate (Selection a b) = Selection b a

selMin :: Selection -> Coord
selMin (Selection a b) = min a b

selMax :: Selection -> Coord
selMax (Selection a b) = max a b

{-
selToIv :: Document -> Selection -> Iv
selToIv doc sel = Iv (selMin sel) end
  where
    m@(Coord line _) = selMax sel
    next = moveCoord doc 1 m
    end = if next == m
      then Coord line (Text.lengthWord8 (Document.getLine line doc))
      else next

ivToSel :: Document -> Iv -> Selection
ivToSel doc Iv{..} = Selection ivBegin end
  where
    Coord line col = ivEnd
    end = if line == countLines doc - 1 && col == Text.lengthWord8 (Document.getLine line doc)
      then snd . (!! 0) . breakBeforeCoord charBreaker doc $ ivEnd
      else moveCoord doc (-1) ivEnd
-}

selAtLine :: Document -> Selection -> Int -> Maybe (Int, Int)
selAtLine _ (Selection (Coord aln acol) (Coord mln mcol)) ln | aln == mln =
  if aln == ln
    then Just (min acol mcol, max acol mcol)
    else Nothing
selAtLine doc sel ln = if
  | bln == ln -> Just (bcol, (lastCharOffset . Document.getLine bln $ doc))
  | bln < ln && ln < eln -> Just (0, lastCharOffset . Document.getLine ln $ doc)
  | ln == eln -> Just (0, ecol)
  | otherwise -> Nothing
  where
    (Coord bln bcol, Coord eln ecol) = (selMin sel, selMax sel)

type Movement = Document -> Selection -> Selection

extend :: Movement -> Movement
extend move doc sel = (move doc sel) { anchor = sel.anchor }

moveRight :: Movement
moveRight doc sel = Selection c c
  where c = moveCoord doc 1 sel.mark

moveLeft :: Movement
moveLeft doc sel = Selection c c  
  where c = moveCoord doc (-1) sel.mark

selectToWordEnd :: Movement
selectToWordEnd doc Selection{mark = curr} =
  case findAnchor . breakAfterCoord wordBreaker doc $ curr of
    Nothing -> Selection curr curr
    Just anchor -> Selection anchor . endCoord . findMark . breakAfterCoord wordBreaker doc $ anchor
  where
    isSpace = Text.all (\c -> Char.isSpace c && c /= '\n')
    findAnchor (x : xs) = if (<= 1) . countBreaks charBreaker . ICU.brkBreak . fst $ x
      then fmap snd . listToMaybe . dropWhile ((== "\n") . ICU.brkBreak . fst) $ xs
      else Just . snd $ x
    findAnchor [] = undefined
    findMark (x : []) = x
    findMark (x : y : xs)
      | isSpace (ICU.brkBreak (fst x)) = if ICU.brkBreak (fst y) == "\n" then x else findMark (y : xs)
      | ICU.brkBreak (fst x) == "\n" = findMark (y : xs)
      | otherwise = x
    findMark [] = undefined
    endCoord (brk, Coord line col) = Coord line (col + lastCharOffset (ICU.brkBreak brk))

selectToWordStart :: Movement
selectToWordStart doc Selection{mark = curr} =
  case findAnchor . breakAfterCoord wordBreaker doc $ curr of
    Nothing -> Selection curr curr
    Just anchor -> Selection anchor . endCoord . findMark . breakAfterCoord wordBreaker doc $ anchor
  where
    isSpace = Text.all (\c -> Char.isSpace c && c /= '\n')
    findAnchor (x : xs) = if (<= 1) . countBreaks charBreaker . ICU.brkBreak . fst $ x
      then fmap snd . listToMaybe . dropWhile ((== "\n") . ICU.brkBreak . fst) $ xs
      else Just . snd $ x
    findAnchor [] = undefined
    findMark (x : []) = x
    findMark (x : y : _)
      | isSpace (ICU.brkBreak (fst y)) = y
      | otherwise = x
    findMark [] = undefined
    endCoord (brk, Coord line col) = Coord line (col + lastCharOffset (ICU.brkBreak brk))

lastCharOffset :: Text -> Int
lastCharOffset = Text.lengthWord8 . ICU.brkPrefix . last . ICU.breaks charBreaker
