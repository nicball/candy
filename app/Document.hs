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
  , moveCoord
  , fromText
  , toText
  , breakWithCoord
  , breakBackwardWithCoord
  ) where

import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import Data.Text (Text)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Maybe (fromJust)
import Data.Foldable (fold)
import Data.Monoid (Sum(..))
import qualified Data.Text.ICU as ICU

data Document = Document
  { docLines :: Seq Text
  }
data Coord = Coord { coordLine :: Int, coordColumn :: Int }
  deriving Show
data Iv = Iv { ivBegin :: Coord, ivEnd :: Coord }
  deriving Show
-- data EndOfLine = Cr | Lf | CrLf

empty :: Document
empty = Document . Seq.singleton $ ""

patch :: Iv -> Text -> Document -> Document
patch (Iv (Coord bline bcol) (Coord eline ecol)) text doc@Document{docLines}
  | bline > eline = doc
  | otherwise = Document $ combineLines before (combineLines (toLines text) after)
  where
    before = Seq.take bline docLines <> Seq.singleton prefix
    after = (if Text.null suffix then Seq.empty else Seq.singleton suffix) <> Seq.drop (eline + 1) docLines
    prefix = Text.takeWord8 (fromIntegral bcol) . fromJust . Seq.lookup bline $ docLines
    suffix = Text.dropWord8 (fromIntegral ecol) . fromJust . Seq.lookup eline $ docLines

extract :: Iv -> Document -> Text
extract (Iv (Coord bline bcol) (Coord eline ecol)) Document{docLines}
  | bline == eline = Text.dropWord8 (fromIntegral bcol) . Text.takeWord8 (fromIntegral ecol) . fromJust . Seq.lookup bline $ docLines
  | bline > eline = ""
  | otherwise = firstLine <> wholeLines <> lastLine
  where
    firstLine = Text.dropWord8 (fromIntegral bcol) . fromJust . Seq.lookup bline $ docLines
    wholeLines = foldMap (fromJust . flip Seq.lookup docLines) [ bline, bline + 1 .. eline - 1 ]
    lastLine = Text.takeWord8 (fromIntegral ecol) . fromJust . Seq.lookup eline $ docLines

getLine :: Int -> Document -> Text
getLine n Document{docLines} = fromJust . Seq.lookup n $ docLines

length :: Document -> Int
length = getSum . foldMap (Sum . Text.length) . docLines

countLines :: Document -> Int
countLines = Seq.length . docLines

moveCoord :: Document -> Int -> Coord -> Coord
moveCoord _ 0 c = c
moveCoord document offset coord =
  if offset > 0
    then forward offset
    else backward (-offset)
  where
    forward n = indexSatDef coord (snd <$> breakWithCoord (ICU.breakCharacter ICU.Current) coord document) n
    backward n = indexSatDef coord (snd <$> breakBackwardWithCoord (ICU.breakCharacter ICU.Current) coord document) (n - 1)

fromText :: Text -> Document
fromText = Document <$> toLines

toText :: Document -> Text
toText = fold . docLines

toLines :: Text -> Seq Text
toLines text = fmap (<> "\n") initLines <> lastLines
  where
    lastLines = maybe Seq.empty (\(_, end) -> if end == '\n' then Seq.empty else Seq.singleton lastLine) . Text.unsnoc $ text
    (initLines, lastLine) = case Seq.viewr . Seq.fromList . Text.splitOn "\n" $ text of
      s Seq.:> a -> (s, a)
      Seq.EmptyR -> undefined

combineLines :: Seq Text -> Seq Text -> Seq Text
combineLines Seq.Empty a = a
combineLines a Seq.Empty = a
combineLines x@(as Seq.:|> a) y@(b Seq.:<| bs)
  | "\n" `Text.isSuffixOf` a = x <> y
  | otherwise = as <> Seq.singleton (a <> b) <> bs

breakWithCoord :: ICU.Breaker a -> Coord -> Document -> [(ICU.Break a, Coord)]
breakWithCoord breaker (Coord line col) Document{docLines} =
  restLine ++ restDoc
  where
    restLine =
      fmap (\brk -> (brk, Coord line (col + Text.lengthWord8 (ICU.brkPrefix brk))))
      . ICU.breaks breaker
      . Text.dropWord8 (fromIntegral col)
      . fromJust
      . Seq.lookup line
      $ docLines
    restDoc = fold . Seq.mapWithIndex (\idx text -> breakLine (idx + line + 1) text). Seq.drop (line + 1) $ docLines
    breakLine idx = fmap (\brk -> (brk, Coord idx (Text.lengthWord8 (ICU.brkPrefix brk)))) . ICU.breaks breaker

breakBackwardWithCoord :: ICU.Breaker a -> Coord -> Document -> [(ICU.Break a, Coord)]
breakBackwardWithCoord breaker (Coord line col) Document{docLines} =
  restLine ++ restDoc
  where
    restLine =
      reverse
      . fmap (\brk -> (brk, Coord line (Text.lengthWord8 (ICU.brkPrefix brk))))
      . ICU.breaks breaker
      . Text.takeWord8 (fromIntegral col)
      . fromJust
      . Seq.lookup line
      $ docLines
    restDoc = fold . Seq.reverse . Seq.mapWithIndex (\idx text -> breakLine idx text). Seq.take line $ docLines
    breakLine idx = reverse . fmap (\brk -> (brk, Coord idx (Text.lengthWord8 (ICU.brkPrefix brk)))) . ICU.breaks breaker

indexSatDef :: a -> [a] -> Int -> a
indexSatDef dflt [] _ = dflt
indexSatDef _ [x] _ = x
indexSatDef _ (x : _) 0 = x
indexSatDef _ (_ : xs) n = indexSatDef undefined xs (n - 1)
