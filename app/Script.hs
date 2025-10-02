{-# LANGUAGE QuasiQuotes, UndecidableInstances #-}

module Script
  ( test
  ) where

import Control.Monad (when, void)
import Data.Map qualified as Map
import GHC.Generics (Generic(..), Selector(selName), K1(K1), M1(M1, unM1), (:*:)(..), D1, C1, S1)
import Data.Kind (Type)

import HsLua hiding (Field, deftype, Type)
import HsLua.Core.Utf8 qualified as Utf8

import Data.String.Interpolate (i)
import Data.Text.IO qualified as Text
import Data.Text (Text)
import Data.IORef

data Field a = forall b. (Pushable b, Peekable b) => Field
  { name :: Name
  , get :: a -> IO b
  , set :: a -> b -> IO ()
  }

data Method = Method
  { name :: Name
  , action :: Lua NumResults
  }

defType :: Name -> [Field a] -> [Method] -> Lua ()
defType name fields methods = do
  newudmetatable name >>= flip when do
    pushHaskellFunction indexFn
    setfield (nth 2) "__index"
    pushHaskellFunction newIndexFn
    setfield (nth 2) "__newindex"
  where
    indexFn = do
      hsValue <- fromuserdata (nthBottom 1) name
        >>= maybe (throwTypeMismatchError (fromName name) (nthBottom 1)) pure
      indexName <- forcePeek . peekName . nthBottom $ 2
      case Map.lookup indexName fieldsMap of
        Just Field{get} -> do
          value <- liftIO . get $ hsValue
          push value
          pure (NumResults 1)
        Nothing -> case Map.lookup indexName methodsMap of
          Just action -> pushHaskellFunction action >> pure (NumResults 1)
          Nothing -> failLua $ "unknown field '" <> (Utf8.toString . fromName $ indexName) <> "'"
    newIndexFn = do
      hsValue <- fromuserdata (nthBottom 1) name
        >>= maybe (throwTypeMismatchError (fromName name) (nthBottom 1)) pure
      indexName <- forcePeek . peekName . nthBottom $ 2
      case Map.lookup indexName fieldsMap of
        Just Field{set} -> do
          newValue <- peek (nthBottom 3)
          liftIO $ set hsValue newValue
          pure (NumResults 0)
        Nothing -> failLua $ "unknown or readonly field '" <> (Utf8.toString . fromName $ indexName) <> "'"
    fieldsMap = Map.fromList . fmap (\f -> (f.name, f)) $ fields
    methodsMap = Map.fromList . fmap (\m -> (m.name, m.action)) $ methods

data Record1 = MkRecord1
  { field1 :: Record2
  }
  deriving Generic

data Record2 = MkRecord2
  { field2 :: LuaPrim Int
  }
  deriving Generic

test :: IO ()
test = run do
  defGenericRecordType
  pop 1
  pushGenericRecord =<< liftIO (newIORef (MkRecord1 (MkRecord2 (LuaPrim 42))))
  setglobal "hsvalue"
  openio
  setglobal "io"
  status <- dostringTrace [i|
    io.write(hsvalue.field1.field2)
    hsvalue.field1 = { field2 = 24 }
    io.write(hsvalue.field1.field2)
    io.flush()
  |]
  case status of
    OK -> pure ()
    _ -> liftIO . print =<< popException

defGenericRecordType :: Lua ()
defGenericRecordType = do
  newudmetatable name >>= flip when do
    pushHaskellFunction indexFn
    setfield (nth 2) "__index"
    pushHaskellFunction newIndexFn
    setfield (nth 2) "__newindex"
  where
    name = "Candy.GenericRecord"
    indexFn = do
      readField (nthBottom 1)
      pure (NumResults 1)
    newIndexFn = do
      writeField (nthBottom 1)
      pure (NumResults 0)

data GenericRecordUD = forall root a. GenericRecordUD
  { root :: IORef root
  , getFocus :: root -> a
  , modifyFocus :: (a -> a) -> root -> root
  , subfocuses :: Map.Map Name (Focus a)
  }

pushGenericRecord :: (Generic a, FocusesOf' (Rep a)) => IORef a -> Lua ()
pushGenericRecord root
  = pushGenericRecordUD
  . GenericRecordUD root id id
  . Map.fromList
  . fmap (\f -> (f.name, f))
  $ focusesOf

pushGenericRecordUD :: GenericRecordUD -> Lua ()
pushGenericRecordUD ud = do
  newhsuserdatauv ud 0
  TypeTable <- getfield registryindex "Candy.GenericRecord"
  setmetatable (nth 2)

readField :: StackIndex -> Lua ()
readField udIdx = do
  fname <- forcePeek . peekName . nth $ 1
  GenericRecordUD root getFocus modifyFocus subfocuses <- fromuserdata udIdx "Candy.GenericRecord"
    >>= maybe (throwTypeMismatchError "Candy.GenericRecord" udIdx) pure
  pop 1
  case Map.lookup fname $ subfocuses of
    Just (Focus _ get set (SubTable sf)) -> do
      let modify f a = set a (f (get a))
      pushGenericRecordUD GenericRecordUD
        { root = root
        , getFocus = get . getFocus
        , modifyFocus = modifyFocus . modify
        , subfocuses = Map.fromList . fmap (\f -> (f.name, f)) $ sf
        }
    Just (Focus _ get _ PrimField) -> do
      push =<< get . getFocus <$> liftIO (readIORef root)
    Nothing -> do
      failLua $ "unknown field '" <> (Utf8.toString . fromName $ fname) <> "'"

writeField :: StackIndex -> Lua ()
writeField udIdx = do
  fname <- forcePeek . peekName . nth $ 2
  rotate (nth 2) (-1)
  pop 1
  GenericRecordUD root _ modifyFocus subfocuses <- fromuserdata udIdx "Candy.GenericRecord"
    >>= maybe (throwTypeMismatchError "Candy.GenericRecord" udIdx) pure
  case Map.lookup fname $ subfocuses of
    Just (Focus _ _ set (SubTable _)) -> do
      v <- parseTable
      liftIO . modifyIORef root . modifyFocus . flip set $ v
      pop 1
    Just (Focus _ _ set PrimField) -> do
      v <- peek top
      liftIO . modifyIORef root . modifyFocus . flip set $ v
      pop 1
    Nothing -> do
      pop 1
      failLua $ "unknown field '" <> (Utf8.toString . fromName $ fname) <> "'"

parseTable :: (Generic a, ParseTable' (Rep a)) => Lua a
parseTable = to <$> parseTable'

class ParseTable' (a :: Type -> Type) where
  parseTable' :: Lua (a p)

instance {-# OVERLAPPING #-} (Peekable a, Selector m) => ParseTable' (S1 m (K1 i (LuaPrim a))) where
  parseTable' = do
    let name = Name . Utf8.fromString . selName $ (undefined :: S1 m (K1 i (LuaPrim a)) p)
    void $ getfield (nth 1) name
    v <- peek (nth 1)
    pop 1
    pure (M1 (K1 (LuaPrim v)))

instance {-# OVERLAPPABLE #-} (Generic a, ParseTable' (Rep a), Selector m) => ParseTable' (S1 m (K1 i a)) where
  parseTable' = do
    let name = Name . Utf8.fromString . selName $ (undefined :: S1 m (K1 i a) p)
    void $ getfield (nth 1) name
    v <- to <$> parseTable'
    pop 1
    pure (M1 (K1 v))

instance (ParseTable' a, ParseTable' b) => ParseTable' (a :*: b) where
  parseTable' = (:*:) <$> parseTable' <*> parseTable'

instance ParseTable' a => ParseTable' (C1 m a) where
  parseTable' = M1 <$> parseTable'

instance ParseTable' a => ParseTable' (D1 m a) where
  parseTable' = M1 <$> parseTable'

data Focus a = forall f. Focus
  { name :: Name
  , get :: a -> f
  , set :: a -> f -> a
  , fieldType :: FieldType f
  }

data FieldType f
  = (Pushable f, Peekable f) => PrimField
  | (Generic f, FocusesOf' (Rep f), ParseTable' (Rep f)) => SubTable [Focus f]

focusesOf :: (Generic a, FocusesOf' (Rep a)) => [Focus a]
focusesOf = fmap (\(Focus n get set ft) -> Focus n (get . from) (\a v -> to (set (from a) v)) ft) focusesOf'

class FocusesOf' (f :: Type -> Type) where
  focusesOf' :: [Focus (f a)]

instance (GetFieldType c, Selector m) => FocusesOf' (S1 m (K1 i c)) where
  focusesOf' = [ Focus name getter setter getFieldType ]
    where
      name = Name . Utf8.fromString . selName $ (undefined :: S1 m (K1 i c) p)
      getter (M1 (K1 c)) = c
      setter _ v = M1 (K1 v)

instance (FocusesOf' a, FocusesOf' b) => FocusesOf' (a :*: b) where
  focusesOf' =
    fmap (\(Focus n get set ft) -> Focus n (get . getFst) (\(a :*: b) v -> set a v :*: b) ft) fstFocuses
    ++
    fmap (\(Focus n get set ft) -> Focus n (get . getSnd) (\(a :*: b) v -> a :*: set b v) ft) sndFocuses
    where
      fstFocuses = focusesOf' @a
      getFst (a :*: _) = a
      sndFocuses = focusesOf' @b
      getSnd (_ :*: b) = b

instance FocusesOf' a => FocusesOf' (C1 m a) where
  focusesOf' = fmap (\(Focus n get set ft) -> Focus n (get . unM1) (\(M1 a) v -> M1 (set a v)) ft) $ focusesOf' @a

instance FocusesOf' a => FocusesOf' (D1 m a) where
  focusesOf' = fmap (\(Focus n get set ft) -> Focus n (get . unM1) (\(M1 a) v -> M1 (set a v)) ft) $ focusesOf' @a

class GetFieldType a where
  getFieldType :: FieldType a

instance {-# OVERLAPPABLE #-} (Generic a, FocusesOf' (Rep a), ParseTable' (Rep a)) => GetFieldType a where
  getFieldType = SubTable focusesOf

instance {-# OVERLAPPING #-} (Pushable a, Peekable a) => GetFieldType (LuaPrim a) where
  getFieldType = PrimField

newtype LuaPrim a = LuaPrim { value :: a }

instance Pushable a => Pushable (LuaPrim a) where
  push = push . (.value)

instance Peekable a => Peekable (LuaPrim a) where
  safepeek = fmap LuaPrim <$> safepeek
