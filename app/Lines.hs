module Lines
  ( Lines(..)
  , Coord(..)
  , clampCoord
  , subtractCoord
  , addCoord
  , fromText
  , close
  , last
  , at
  , length
  , split
  , lastCharCoord
  , endCoord
  , lastCharOffset
  , charBreaker
  ) where

import Data.Sequence qualified as Seq
import Data.Sequence (Seq)
import Data.Text.Foreign qualified as Text
import Data.Text.ICU qualified as ICU
import Data.Text qualified as Text
import Data.Text (Text)
import Prelude hiding (last, length)
import Prelude qualified

newtype Lines = Lines { seq :: Seq Line }

type Line = Text

data Coord = Coord { line :: Int, column :: Int }
  deriving (Eq, Ord)

instance Show Coord where
  show c = show c.line <> ":" <> show c.column

subtractCoord :: Coord -> Coord -> Coord
subtractCoord a b = Coord (a.line - b.line) (if a.line == b.line then a.column - b.column else a.column)

addCoord :: Coord -> Coord -> Coord
addCoord a b = Coord (a.line + b.line) (if a.line == 0 then a.column + b.column else a.column)

fromText :: Text -> Lines
fromText text = Lines . (fmap (<> "\n") initl Seq.|>) $ lastl
  where
    (initl, lastl) = unsnocSeq . Seq.fromList . Text.splitOn "\n" $ text

instance Semigroup Lines where
  (Lines Seq.Empty) <> a = a
  a <> (Lines Seq.Empty) = a
  (Lines x@(as Seq.:|> a)) <> (Lines y@(b Seq.:<| bs))
    | "\n" `Text.isSuffixOf` a = Lines (x <> y)
    | otherwise = Lines (as <> Seq.singleton (a <> b) <> bs)

instance Monoid Lines where
  mempty = Lines Seq.empty

close :: Lines -> Lines
close (Lines lns) = case Seq.viewr lns of
  as Seq.:> a
    | Text.null a -> close (Lines as)
    | otherwise -> Lines lns
  Seq.EmptyR -> Lines (Seq.singleton "\n")

last :: Lines -> Line
last (Lines lns) = snd . unsnocSeq $ lns

at :: Lines -> Int -> Line
(Lines lns) `at` n = Seq.index lns n

length :: Lines -> Int
length (Lines lns) = Seq.length lns

split ::  Coord -> Lines -> (Lines, Lines)
split c (Lines lns) =
  ( Lines (before Seq.|> prefix)
  , Lines if Text.null suffix
    then after
    else suffix Seq.<| after
  )
  where
    (before, notBefore) = Seq.splitAt c.line lns
    (ln, after) = unconsSeq notBefore
    (prefix, suffix) = (Text.takeWord8 (fromIntegral c.column) ln, Text.dropWord8 (fromIntegral c.column) ln)

clampCoord :: Lines -> Coord -> Coord
clampCoord lns c
  | c.line >= numLines = Coord (c.line - 1) (lastCharOffset (last lns))
  | c.column < Text.lengthWord8 lineText = c
  | c.line == numLines - 1 =  c { column = lastCharOffset lineText }
  | otherwise = Coord (c.line + 1) 0
  where
    lineText = lns `at` c.line
    numLines = length lns

lastCharCoord :: Lines -> Coord
lastCharCoord (Lines lns) = Coord (Seq.length wholeLns) col
  where
    (wholeLns, ln) = unsnocSeq lns
    col = Text.lengthWord8 . ICU.brkPrefix . Prelude.last . ICU.breaks charBreaker $ ln

endCoord :: Lines -> Coord
endCoord (Lines lns) = Coord (Seq.length wholeLns) (Text.lengthWord8 ln)
  where
    (wholeLns, ln) = unsnocSeq lns

unconsSeq :: Seq a -> (a, Seq a)
unconsSeq lns = case Seq.viewl lns of
  x Seq.:< xs -> (x, xs)
  Seq.EmptyL -> undefined

unsnocSeq :: Seq a -> (Seq a, a)
unsnocSeq lns = case Seq.viewr lns of
  xs Seq.:> x -> (xs, x)
  Seq.EmptyR -> undefined

lastCharOffset :: Text -> Int
lastCharOffset = Text.lengthWord8 . ICU.brkPrefix . Prelude.last . ICU.breaks charBreaker

charBreaker :: ICU.Breaker ()
charBreaker = ICU.breakCharacter ICU.Current
