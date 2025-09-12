module Document
  ( Document
  , Coord(..)
  , Iv(..)
  , empty
  , patch
  , extract
  , Document.getLine
  , Document.length
  , countLines
  , charLengthAt
  , endOfDocument
  , moveCoord
  , fromText
  , toText
  , breakAfterCoord
  , breakBeforeCoord
  , charBreaker
  , wordBreaker
  , countBreaks
  , lastCharOffset
  ) where

import Data.Foldable (fold)
import Data.Maybe (fromJust)
import Data.Monoid (Sum(..))
import Data.Sequence qualified as Seq
import Data.Sequence (Seq)
import Data.Text.Foreign qualified as Text
import Data.Text.ICU qualified as ICU
import Data.Text qualified as Text
import Data.Text (Text)

data Document = Document
  { lines :: Seq Text
  }
  deriving Show

data Coord = Coord { line :: Int, column :: Int }
  deriving (Show, Eq, Ord)

data Iv = Iv { begin :: Coord, end :: Coord }
  deriving (Show, Eq)
-- data EndOfLine = Cr | Lf | CrLf

{-
subIv :: Iv -> Iv -> Bool
subIv (Iv a b) (Iv c d) = c <= a && b <= d
-}

inIv :: Coord -> Iv -> Bool
inIv c (Iv b e) = b <= c && c < e

empty :: Document
empty = Document . Seq.singleton $ "\n"

sanitizeCoord :: Document -> Coord -> Coord
sanitizeCoord doc c =
  if c.line == numLines
  then Coord (c.line - 1) (lastCharOffset (getLastLine doc.lines))
  else
    if c.column < Text.lengthWord8 lineText
    then c
    else
      if c.line == numLines - 1
      then c { column = lastCharOffset lineText }
      else Coord (c.line + 1) 0
  where
    lineText = Document.getLine c.line doc
    numLines = countLines doc

sanitizeLines :: Seq Text -> Seq Text
sanitizeLines lns = case Seq.viewr lns of
  as Seq.:> a
    | a == "" -> sanitizeLines as
    | otherwise -> lns
  Seq.EmptyR -> Seq.singleton "\n"

patch :: Iv -> Text -> Document -> (Document, Coord -> Coord)
patch iv@(Iv (Coord bline bcol) (Coord eline ecol)) text doc
  | bline > eline = undefined
  | otherwise = (newDoc, patchCoord)
  where
    newDoc = Document . sanitizeLines . combineLines before . combineLines textLines $ after
    before = Seq.take bline doc.lines <> Seq.singleton prefix
    after = (if Text.null suffix then Seq.empty else Seq.singleton suffix) <> Seq.drop (eline + 1) doc.lines
    prefix = Text.takeWord8 (fromIntegral bcol) . fromJust . Seq.lookup bline $ doc.lines
    suffix = Text.dropWord8 (fromIntegral ecol) . fromJust . Seq.lookup eline $ doc.lines
    textLines = toLines text
    numLines = Seq.length textLines
    patchEndCoord = Coord (bline + numLines - 1) (Text.lengthWord8 (getLastLine textLines) + if numLines == 1 then bcol else 0)
    patchCoord c
      | c < iv.begin = c
      | c `inIv` iv = sanitizeCoord newDoc patchEndCoord
      | iv.end <= c =
        if c.line == iv.end.line
        then Coord patchEndCoord.line (c.column - ecol + patchEndCoord.column)
        else c { line = c.line - iv.end.line + patchEndCoord.line }
      | otherwise = undefined

extract :: Iv -> Document -> Text
extract (Iv (Coord bline bcol) (Coord eline ecol)) doc
  | bline == eline = Text.dropWord8 (fromIntegral bcol) . Text.takeWord8 (fromIntegral ecol) . fromJust . Seq.lookup bline $ doc.lines
  | bline > eline = undefined
  | otherwise = firstLine <> wholeLines <> lastLine
  where
    firstLine = Text.dropWord8 (fromIntegral bcol) . fromJust . Seq.lookup bline $ doc.lines
    wholeLines = foldMap (fromJust . flip Seq.lookup doc.lines) [ bline, bline + 1 .. eline - 1 ]
    lastLine = Text.takeWord8 (fromIntegral ecol) . fromJust . Seq.lookup eline $ doc.lines

getLine :: Int -> Document -> Text
getLine n doc = fromJust . Seq.lookup n $ doc.lines

length :: Document -> Int
length doc = getSum . foldMap (Sum . Text.length) $ doc.lines

countLines :: Document -> Int
countLines doc = Seq.length doc.lines

charLengthAt :: Document -> Coord -> Int
charLengthAt = fmap (Text.lengthWord8 . ICU.brkBreak . fst . (!! 0)) . breakAfterCoord charBreaker

endOfDocument :: Document -> Coord
endOfDocument doc = Coord line col
  where
    line = countLines doc - 1
    col = Text.lengthWord8 . ICU.brkPrefix . last . ICU.breaks charBreaker . fromJust . Seq.lookup line $ doc.lines

moveCoord :: Document -> Int -> Coord -> Coord
moveCoord _ 0 c = c
moveCoord document offset coord =
  if offset > 0
    then forward offset
    else backward (-offset)
  where
    forward n = indexSatDef coord (snd <$> breakAfterCoord charBreaker document coord) n
    backward n = indexSatDef coord (snd <$> breakBeforeCoord charBreaker document coord) (n - 1)

fromText :: Text -> Document
fromText = Document <$> toLines

toText :: Document -> Text
toText doc = fold doc.lines

toLines :: Text -> Seq Text
toLines text = fmap (<> "\n") initLines <> lastLines
  where
    lastLines = maybe (Seq.singleton "") (\(_, end) -> if end == '\n' then Seq.empty else Seq.singleton lastLine) . Text.unsnoc $ text
    (initLines, lastLine) = case Seq.viewr . Seq.fromList . Text.splitOn "\n" $ text of
      s Seq.:> a -> (s, a)
      Seq.EmptyR -> undefined

getLastLine :: Seq Text -> Text
getLastLine lns= case Seq.viewr lns of
  Seq.EmptyR -> undefined
  _ Seq.:> l -> l

combineLines :: Seq Text -> Seq Text -> Seq Text
combineLines Seq.Empty a = a
combineLines a Seq.Empty = a
combineLines x@(as Seq.:|> a) y@(b Seq.:<| bs)
  | "\n" `Text.isSuffixOf` a = x <> y
  | otherwise = as <> Seq.singleton (a <> b) <> bs

breakAfterCoord :: ICU.Breaker a -> Document -> Coord -> [(ICU.Break a, Coord)]
breakAfterCoord breaker doc (Coord line col) =
  restLine ++ restDoc
  where
    restLine =
      fmap (\brk -> (brk, Coord line (col + Text.lengthWord8 (ICU.brkPrefix brk))))
      . ICU.breaks breaker
      . Text.dropWord8 (fromIntegral col)
      . fromJust
      . Seq.lookup line
      $ doc.lines
    restDoc = fold . Seq.mapWithIndex (\idx text -> breakLine (idx + line + 1) text). Seq.drop (line + 1) $ doc.lines
    breakLine idx = fmap (\brk -> (brk, Coord idx (Text.lengthWord8 (ICU.brkPrefix brk)))) . ICU.breaks breaker

breakBeforeCoord :: ICU.Breaker a -> Document -> Coord -> [(ICU.Break a, Coord)]
breakBeforeCoord breaker doc (Coord line col) =
  restLine ++ restDoc
  where
    restLine =
      reverse
      . fmap (\brk -> (brk, Coord line (Text.lengthWord8 (ICU.brkPrefix brk))))
      . ICU.breaks breaker
      . Text.takeWord8 (fromIntegral col)
      . fromJust
      . Seq.lookup line
      $ doc.lines
    restDoc = fold . Seq.reverse . Seq.mapWithIndex (\idx text -> breakLine idx text). Seq.take line $ doc.lines
    breakLine idx = reverse . fmap (\brk -> (brk, Coord idx (Text.lengthWord8 (ICU.brkPrefix brk)))) . ICU.breaks breaker

indexSatDef :: a -> [a] -> Int -> a
indexSatDef dflt [] _ = dflt
indexSatDef _ [x] _ = x
indexSatDef _ (x : _) 0 = x
indexSatDef _ (_ : xs) n = indexSatDef undefined xs (n - 1)

charBreaker :: ICU.Breaker ()
charBreaker = ICU.breakCharacter ICU.Current

wordBreaker :: ICU.Breaker ICU.Word
wordBreaker = ICU.breakWord ICU.Current

countBreaks :: ICU.Breaker a -> Text -> Int
countBreaks breaker = Prelude.length . ICU.breaks breaker

lastCharOffset :: Text -> Int
lastCharOffset = Text.lengthWord8 . ICU.brkPrefix . last . ICU.breaks charBreaker
