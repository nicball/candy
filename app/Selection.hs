module Selection
  ( Selection(..)
  , alternate
  , selMin
  , selMax
  , selLines
  , extend
  , moveLeft
  , moveRight
  , selectToWordEnd
  , selectToWordStart
  ) where

import Data.Char qualified as Char
import Data.Functor ((<&>))
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

selLines :: Document -> Selection -> [(Int, Int, Int)]
selLines _ (Selection (Coord aln acol) (Coord mln mcol)) | aln == mln = [(aln, min acol mcol, max acol mcol)]
selLines doc sel = firstRange : mid ++ lastRange
  where
    (Coord bln bcol, Coord eln ecol) = (selMin sel, selMax sel)
    firstRange = (bln, bcol, (lastCharOffset . Document.getLine bln $ doc))
    lastRange = if ecol == 0 then [] else [(eln, 0, ecol)]
    mid = [bln + 1, bln + 2 .. eln - 1] <&> \ln ->
      (ln, 0, lastCharOffset . Document.getLine ln $ doc)

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
