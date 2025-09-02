module Document
  ( Document
  , Coord(..)
  , Iv(..)
  , empty
  , patch
  , extract
  , docGetLine
  , Document.length
  , moveCoord
  , fromText
  , toText
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
data Iv = Iv { ivBegin :: Coord, ivEnd :: Coord }
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

docGetLine :: Int -> Document -> Text
docGetLine n Document{docLines} = fromJust . Seq.lookup n $ docLines

length :: Document -> Int
length = getSum . foldMap (Sum . Text.length) . docLines

moveCoord :: Document -> Int -> Coord -> Coord
moveCoord _ 0 c = c
moveCoord Document{docLines} offset (Coord line col) =
  if offset > 0
    then forward line col offset
    else backward line col (-offset)
  where
    nLines = Seq.length docLines
    forward line col 0 = Coord line col
    forward line col n =
      if n < nRest
        then Coord line . (col +) . Text.lengthWord8 . ICU.brkPrefix . (!! n) $ rest
        else if line + 1 < nLines
          then forward (line + 1) 0 (n - nRest)
          else Coord line . Text.lengthWord8 . fromJust . Seq.lookup line $ docLines
      where
        rest = ICU.breaks (ICU.breakCharacter ICU.Current) . Text.dropWord8 (fromIntegral col) . fromJust . Seq.lookup line $ docLines
        nRest = Prelude.length rest
    backward line col 0 = Coord line col
    backward line col n =
      if n <= nRest
        then Coord line . (col -) . suffixLength . (!! (n - 1)) $ rest
        else if line - 1 >= 0
          then backward (line - 1) lastCol (n - nRest - 1)
          else Coord 0 0
      where
        lastCol = Text.lengthWord8 . fst . fromJust . Text.unsnoc . fromJust . Seq.lookup (line - 1) $ docLines
        suffixLength b = Text.lengthWord8 (ICU.brkBreak b) + Text.lengthWord8 (ICU.brkSuffix b)
        rest = reverse . ICU.breaks (ICU.breakCharacter ICU.Current) . Text.takeWord8 (fromIntegral col) . fromJust . Seq.lookup line $ docLines
        nRest = Prelude.length rest

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
