{-# LANGUAGE MultiWayIf #-}

module Selection
  ( Selection(..)
  , alternate
  , selMin
  , selMax
  , selToIv
  , ivToSel
  , extend
  , moveLeft
  , moveRight
  , selectToWordEnd
  , selectToWordStart
  ) where

import Document
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import qualified Data.Text.ICU as ICU
import qualified Data.Char as Char
import Data.Maybe (listToMaybe)

data Selection = Selection { selAnchor :: Coord, selMark :: Coord }

alternate :: Selection -> Selection
alternate (Selection a b) = Selection b a

selMin :: Selection -> Coord
selMin (Selection a b) = min a b

selMax :: Selection -> Coord
selMax (Selection a b) = max a b

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
      then snd . last . breakBeforeCoord charBreaker doc $ ivEnd
      else moveCoord doc (-1) ivEnd

type Movement = Document -> Selection -> Selection

extend :: Movement -> Movement
extend move doc sel = (move doc sel) { selAnchor = selAnchor sel }

moveRight :: Movement
moveRight doc sel = Selection c c
  where c = moveCoord doc 1 (selMark sel)

moveLeft :: Movement
moveLeft doc sel = Selection c c  
  where c = moveCoord doc (-1) (selMark sel)

{-
aaa---n---aaa--e
^ %
 ^%
   ^ %
   ^ %
    ^%
       ^    %
       ^    %
       ^    %
       ^    %
        ^   %
          ^ %
          ^ %
           ^%
             ^%
             ^%
              %
-}

selectToWordEnd :: Movement
selectToWordEnd doc Selection{selMark = curr} =
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
    endCoord (brk, Coord line col) = Coord line (col + offset)
      where
        offset = Text.lengthWord8 . ICU.brkPrefix . last . ICU.breaks charBreaker . ICU.brkBreak $ brk

selectToWordStart :: Movement
selectToWordStart doc Selection{selMark = curr} =
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
    endCoord (brk, Coord line col) = Coord line (col + offset)
      where
        offset = Text.lengthWord8 . ICU.brkPrefix . last . ICU.breaks charBreaker . ICU.brkBreak $ brk
