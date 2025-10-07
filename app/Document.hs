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
import Data.List (foldl')
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
  deriving (Eq, Ord)

data Iv = Iv { begin :: Coord, end :: Coord }
  deriving (Show, Eq)
-- data EndOfLine = Cr | Lf | CrLf

{-
subIv :: Iv -> Iv -> Bool
subIv (Iv a b) (Iv c d) = c <= a && b <= d
-}

instance Show Coord where
  show c = show c.line <> ":" <> show c.column

inIv :: Coord -> Iv -> Bool
inIv c (Iv b e) = b <= c && c < e

mapIv :: (Coord -> Coord) -> Iv -> Iv
mapIv f iv = Iv (f iv.begin) (f iv.end)

empty :: Document
empty = Document . Seq.singleton $ "\n"

clampCoord :: Document -> Coord -> Coord
clampCoord doc c
  | c.line >= numLines = Coord (c.line - 1) (lastCharOffset (getLastLine doc.lines))
  | c.column < Text.lengthWord8 lineText = c
  | c.line == numLines - 1 =  c { column = lastCharOffset lineText }
  | otherwise = Coord (c.line + 1) 0
  where
    lineText = Document.getLine c.line doc
    numLines = countLines doc

patch :: [(Iv, Text)] -> Document -> (Document, Coord -> Coord)
patch mods doc = (newDoc, clampCoord newDoc . genTranslate id)
  where
    newDoc = Document . closeLines . combineLines firstHalf $ secondHalf
    (firstHalf, secondHalf, _, genTranslate) = foldl' go (Seq.singleton "", doc.lines, id, id) mods
    go (done, rest, offsetIv, gt) (iv, text) = (combineLines done before, after, offsetIv', gt')
      where
        (before, len, after, subgt) = patch1 (offsetIv iv) text rest
        offsetIv' = mapIv (`subtractCoord` len) . offsetIv
        gt' = gt . subgt
    patch1 :: Iv -> Text -> Seq Text -> (Seq Text, Coord, Seq Text, (Coord -> Coord) -> Coord -> Coord)
    patch1 iv text lns = (patched, afterBegin, after, translate)
      where
        textLines = toOpenLines text
        (before, _) = splitLines iv.begin lns
        (_, after) = splitLines iv.end lns
        patched = combineLines before textLines
        afterBegin = lengthLines patched
        translate f c
          | c < iv.begin = c
          | c `inIv` iv = afterBegin
          | iv.end <= c = f (c `subtractCoord` iv.end) `addCoord` afterBegin
          | otherwise = undefined

subtractCoord :: Coord -> Coord -> Coord
subtractCoord a b = Coord (a.line - b.line) (if a.line == b.line then a.column - b.column else a.column)

addCoord :: Coord -> Coord -> Coord
addCoord a b = Coord (a.line + b.line) (if a.line == 0 then a.column + b.column else a.column)

extract :: Iv -> Document -> Text
extract iv doc
  = fold
  . fst . splitLines (iv.end `subtractCoord` iv.begin)
  . snd . splitLines iv.begin
  $ doc.lines

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
    forward = indexSatDef coord (snd <$> breakAfterCoord charBreaker document coord)
    backward n = indexSatDef coord (snd <$> breakBeforeCoord charBreaker document coord) (n - 1)

fromText :: Text -> Document
fromText = Document <$> closeLines . toOpenLines

toText :: Document -> Text
toText doc = fold doc.lines

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
    restDoc = fold . Seq.reverse . Seq.mapWithIndex breakLine . Seq.take line $ doc.lines
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

------------------------ lines manipulation ------------------------

toOpenLines :: Text -> Seq Text
toOpenLines text = fmap (<> "\n") initLines Seq.|> lastLine
  where
    (initLines, lastLine) = unsnocLines . Seq.fromList . Text.splitOn "\n" $ text

getLastLine :: Seq Text -> Text
getLastLine = snd . unsnocLines

combineLines :: Seq Text -> Seq Text -> Seq Text
combineLines Seq.Empty a = a
combineLines a Seq.Empty = a
combineLines x@(as Seq.:|> a) y@(b Seq.:<| bs)
  | "\n" `Text.isSuffixOf` a = x <> y
  | otherwise = as <> Seq.singleton (a <> b) <> bs

closeLines :: Seq Text -> Seq Text
closeLines lns = case Seq.viewr lns of
  as Seq.:> a
    | Text.null a -> closeLines as
    | otherwise -> lns
  Seq.EmptyR -> Seq.singleton "\n"

splitLines :: Coord -> Seq Text -> (Seq Text, Seq Text)
splitLines c lns = (before Seq.|> prefix, if Text.null suffix then after else suffix Seq.<| after)
  where
    (before, notBefore) = Seq.splitAt c.line lns
    (ln, after) = unconsLines notBefore
    (prefix, suffix) = (Text.takeWord8 (fromIntegral c.column) ln, Text.dropWord8 (fromIntegral c.column) ln)

lengthLines :: Seq Text -> Coord
lengthLines lns = Coord (Seq.length wholeLns) (Text.lengthWord8 ln)
  where
    (wholeLns, ln) = unsnocLines lns

unsnocLines :: Seq Text -> (Seq Text, Text)
unsnocLines lns = case Seq.viewr lns of
  xs Seq.:> x -> (xs, x)
  Seq.EmptyR -> undefined

unconsLines :: Seq Text -> (Text, Seq Text)
unconsLines lns = case Seq.viewl lns of
  x Seq.:< xs -> (x, xs)
  Seq.EmptyL -> undefined
