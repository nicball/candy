module Document
  ( Document(..)
  , DocumentType(..)
  , Coord(..)
  , Iv(..)
  , Patch(..)
  , fromFile
  , patch
  , extract
  , Document.getLine
  , countLines
  , charLengthAt
  , lastCharCoord
  , lastCharOffset
  , moveCoord
  , breakAfterCoord
  , breakBeforeCoord
  , charBreaker
  , wordBreaker
  , countBreaks
  ) where

import Data.Foldable (fold)
import Data.List (sortOn)
import Data.Sequence qualified as Seq
import Data.Text.Foreign qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.ICU qualified as ICU
import Data.Text (Text)
import Data.IORef

import Nexus
import Lines hiding (lastCharCoord)
import Lines qualified

data Document = Document
  { lines :: IORef Lines
  , type_ :: DocumentType
  , onPatch :: Nexus (Coord -> Patch)
  }

data Patch
  = Translation Coord
  | Modification Iv Iv

data DocumentType
  = Scratch
  | FileBacked FilePath

data Iv = Iv { begin :: Coord, end :: Coord }
  deriving (Show, Eq)
-- data EndOfLine = Cr | Lf | CrLf

{-
subIv :: Iv -> Iv -> Bool
subIv (Iv a b) (Iv c d) = c <= a && b <= d
-}

mapIv :: (Coord -> Coord) -> Iv -> Iv
mapIv f iv = Iv (f iv.begin) (f iv.end)

mapPatch :: (Coord -> Coord) -> Patch -> Patch
mapPatch f (Translation c) = Translation (f c)
mapPatch f (Modification d a) = Modification (mapIv f d) (mapIv f a)

fromFile :: FilePath -> IO (Document)
fromFile path = do
  lns <- newIORef . close . fromText =<< Text.readFile path
  let type_ = FileBacked path
  onPatch <- newNexus
  pure Document{ lines = lns, ..}

patch :: [(Iv, Text)] -> Document -> IO ()
patch mods doc = do
  lns <- readIORef doc.lines
  let
    translate = mapPatch (clampCoord newLines) . genTranslate Translation
    newLines = close (firstHalf <> secondHalf)
    (firstHalf, secondHalf, _, genTranslate) = foldl' go (mempty, lns, id, id) . sortOn ((.begin) . fst) $ mods
    go (done, rest, offsetIv, gt) (iv, text) = (done <> before, after, offsetIv', gt')
      where
        oiv = offsetIv iv
        (before, after, subgt) = patch1 oiv text rest
        offsetIv' = mapIv (`subtractCoord` oiv.end) . offsetIv
        gt' = gt . subgt
    patch1 :: Iv -> Text -> Lines -> (Lines, Lines, (Coord -> Patch) -> Coord -> Patch)
    patch1 iv text ls = (patched, after, trans)
      where
        textLines = fromText text
        (before, _) = split iv.begin ls
        (_, after) = split iv.end ls
        patched = before <> textLines
        afterBegin = endCoord patched
        trans f c
          | c < iv.begin = Translation c
          | c < iv.end = Modification iv iv { end = afterBegin }
          | iv.end <= c = mapPatch (`addCoord` afterBegin) . f . (`subtractCoord` iv.end) $ c
          | otherwise = undefined
  writeIORef doc.lines newLines
  notify translate doc.onPatch

extract :: Iv -> Document -> IO Text
extract iv doc
  = fold
  . (.seq)
  . snd . split iv.begin
  . fst . split iv.end
  <$> readIORef doc.lines

getLine :: Int -> Document -> IO Text
getLine n doc = (`at` n) <$> readIORef doc.lines

countLines :: Document -> IO Int
countLines doc = Seq.length . (.seq) <$> readIORef doc.lines

charLengthAt :: Document -> Coord -> IO Int
charLengthAt = fmap (fmap (Text.lengthWord8 . ICU.brkBreak . fst . (!! 0))) . breakAfterCoord charBreaker

lastCharCoord :: Document -> IO Coord
lastCharCoord doc = Lines.lastCharCoord <$> readIORef doc.lines

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
      . (`at` line)
      $ lns
    restDoc = fold . Seq.mapWithIndex (\idx ln -> breakLine (idx + line + 1) ln). Seq.drop (line + 1) $ lns.seq
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
      . (`at` line)
      $ lns
    restDoc = fold . Seq.reverse . Seq.mapWithIndex breakLine . Seq.take line $ lns.seq
    breakLine idx = reverse . fmap (\brk -> (brk, Coord idx (Text.lengthWord8 (ICU.brkPrefix brk)))) . ICU.breaks breaker
  pure $ restLine ++ restDoc

indexSatDef :: a -> [a] -> Int -> a
indexSatDef dflt [] _ = dflt
indexSatDef _ [x] _ = x
indexSatDef _ (x : _) 0 = x
indexSatDef _ (_ : xs) n = indexSatDef undefined xs (n - 1)

wordBreaker :: ICU.Breaker ICU.Word
wordBreaker = ICU.breakWord ICU.Current

countBreaks :: ICU.Breaker a -> Text -> Int
countBreaks breaker = Prelude.length . ICU.breaks breaker
