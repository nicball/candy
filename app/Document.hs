module Document
  ( Document(..)
  , DocumentType(..)
  , Coord(..)
  , Iv(..)
  , fromFile
  , patch
  , extract
  , Document.getLine
  , Document.length
  , countLines
  , charLengthAt
  , endOfDocument
  , moveCoord
  , toText
  , breakAfterCoord
  , breakBeforeCoord
  , charBreaker
  , wordBreaker
  , countBreaks
  , lastCharOffset
  ) where

import Data.Foldable (fold)
import Data.List (foldl', sortOn)
import Data.Maybe (fromJust)
import Data.Monoid (Sum(..))
import Data.Sequence qualified as Seq
import Data.Sequence (Seq)
import Data.Text.Foreign qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.ICU qualified as ICU
import Data.Text qualified as Text
import Data.Text (Text)
import Data.IORef

import Nexus

data Document = Document
  { lines :: IORef (Seq Text)
  , type_ :: DocumentType
  , onPatch :: Nexus (Coord -> Coord)
  }

data DocumentType
  = Scratch
  | FileBacked FilePath

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

mapIv :: (Coord -> Coord) -> Iv -> Iv
mapIv f iv = Iv (f iv.begin) (f iv.end)

fromFile :: FilePath -> IO Document
fromFile path = do
  lns <- newIORef . closeLines . toOpenLines =<< Text.readFile path
  let type_ = FileBacked path
  onPatch <- newNexus
  pure Document{ lines = lns, ..}

toText :: Document -> IO Text
toText doc = fold <$> readIORef doc.lines

clampCoord :: Seq Text -> Coord -> Coord
clampCoord lns c
  | c.line >= numLines = Coord (c.line - 1) (lastCharOffset (getLastLine lns))
  | c.column < Text.lengthWord8 lineText = c
  | c.line == numLines - 1 =  c { column = lastCharOffset lineText }
  | otherwise = Coord (c.line + 1) 0
  where
    lineText = fromJust . Seq.lookup c.line $ lns
    numLines = Seq.length lns

patch :: [(Iv, Text)] -> Document -> IO ()
patch mods doc = do
  lns <- readIORef doc.lines
  let
    translate = clampCoord newLines . genTranslate id
    newLines = closeLines . combineLines firstHalf $ secondHalf
    (firstHalf, secondHalf, _, genTranslate) = foldl' go (Seq.singleton "", lns, id, id) . sortOn ((.begin) . fst) $ mods
    go (done, rest, offsetIv, gt) (iv, text) = (combineLines done before, after, offsetIv', gt')
      where
        oiv = offsetIv iv
        (before, after, subgt) = patch1 oiv text rest
        offsetIv' = mapIv (`subtractCoord` oiv.end) . offsetIv
        gt' = gt . subgt
    patch1 :: Iv -> Text -> Seq Text -> (Seq Text, Seq Text, (Coord -> Coord) -> Coord -> Coord)
    patch1 iv text ls = (patched, after, trans)
      where
        textLines = toOpenLines text
        (before, _) = splitLines iv.begin ls
        (_, after) = splitLines iv.end ls
        patched = combineLines before textLines
        afterBegin = lengthLines patched
        trans f c
          | c < iv.begin = c
          | c < iv.end = afterBegin
          | iv.end <= c = f (c `subtractCoord` iv.end) `addCoord` afterBegin
          | otherwise = undefined
  writeIORef doc.lines newLines
  notify translate doc.onPatch

subtractCoord :: Coord -> Coord -> Coord
subtractCoord a b = Coord (a.line - b.line) (if a.line == b.line then a.column - b.column else a.column)

addCoord :: Coord -> Coord -> Coord
addCoord a b = Coord (a.line + b.line) (if a.line == 0 then a.column + b.column else a.column)

extract :: Iv -> Document -> IO Text
extract iv doc
  = fold
  . snd . splitLines iv.begin
  . fst . splitLines iv.end
  <$> readIORef doc.lines

getLine :: Int -> Document -> IO Text
getLine n doc = fromJust . Seq.lookup n <$> readIORef doc.lines

length :: Document -> IO Int
length doc = getSum . foldMap (Sum . Text.length) <$> readIORef doc.lines

countLines :: Document -> IO Int
countLines doc = Seq.length <$> readIORef doc.lines

charLengthAt :: Document -> Coord -> IO Int
charLengthAt = fmap (fmap (Text.lengthWord8 . ICU.brkBreak . fst . (!! 0))) . breakAfterCoord charBreaker

endOfDocument :: Document -> IO Coord
endOfDocument doc = endOfLines <$> readIORef doc.lines

moveCoord :: Document -> Int -> Coord -> IO Coord
moveCoord _ 0 c = pure c
moveCoord document offset coord =
  if offset > 0
    then forward offset
    else backward (-offset)
  where
    forward n = flip (indexSatDef coord) n . fmap snd <$> breakAfterCoord charBreaker document coord
    backward n = flip (indexSatDef coord) (n - 1) . fmap snd <$> breakBeforeCoord charBreaker document coord

breakAfterCoord :: ICU.Breaker a -> Document -> Coord -> IO [(ICU.Break a, Coord)]
breakAfterCoord breaker doc (Coord line col) = do
  lns <- readIORef doc.lines
  let
    restLine =
      fmap (\brk -> (brk, Coord line (col + Text.lengthWord8 (ICU.brkPrefix brk))))
      . ICU.breaks breaker
      . Text.dropWord8 (fromIntegral col)
      . fromJust
      . Seq.lookup line
      $ lns
    restDoc = fold . Seq.mapWithIndex (\idx text -> breakLine (idx + line + 1) text). Seq.drop (line + 1) $ lns
    breakLine idx = fmap (\brk -> (brk, Coord idx (Text.lengthWord8 (ICU.brkPrefix brk)))) . ICU.breaks breaker
  pure $ restLine ++ restDoc

breakBeforeCoord :: ICU.Breaker a -> Document -> Coord -> IO [(ICU.Break a, Coord)]
breakBeforeCoord breaker doc (Coord line col) = do
  lns <- readIORef doc.lines
  let
    restLine =
      reverse
      . fmap (\brk -> (brk, Coord line (Text.lengthWord8 (ICU.brkPrefix brk))))
      . ICU.breaks breaker
      . Text.takeWord8 (fromIntegral col)
      . fromJust
      . Seq.lookup line
      $ lns
    restDoc = fold . Seq.reverse . Seq.mapWithIndex breakLine . Seq.take line $ lns
    breakLine idx = reverse . fmap (\brk -> (brk, Coord idx (Text.lengthWord8 (ICU.brkPrefix brk)))) . ICU.breaks breaker
  pure $ restLine ++ restDoc

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

endOfLines :: Seq Text -> Coord
endOfLines lns = Coord line col
  where
    closed = closeLines lns
    line = Seq.length closed - 1
    col = Text.lengthWord8 . ICU.brkPrefix . last . ICU.breaks charBreaker . fromJust . Seq.lookup line $ closed
