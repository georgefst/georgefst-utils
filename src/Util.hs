module Util where

import Control.Applicative (liftA2)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Extra (second, (&&&))
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Type.Reflection (Typeable, typeRep)

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen = flip $ bool id

clamp :: Ord c => (c, c) -> c -> c
clamp (l, u) = max l . min u

symbolValT :: forall a. KnownSymbol a => Text
symbolValT = T.pack $ symbolVal $ Proxy @a

showT :: Show a => a -> Text
showT = T.pack . show

typeRepT :: forall a. Typeable a => Text
typeRepT = showT $ typeRep @a

infixl 4 <<$>>
(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

infixl 4 <<*>>
(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

biVoid :: Bifunctor p => p a b -> p () ()
biVoid = bimap (const ()) (const ())

untilLeft :: Monad m => m (Either e a) -> m e
untilLeft x = x >>= either pure (const $ untilLeft x)

mapRightM :: Monad m => (a -> m b) -> Either e a -> m (Either e b)
mapRightM f = either (return . Left) (fmap Right . f)

-- | Like 'groupOn', but with non-adjacent elements grouped, and the witness to equality returned.
classifyOn :: Ord b => (a -> b) -> [a] -> [(b, NonEmpty a)]
classifyOn f = Map.toList . Map.fromListWith (<>) . map (f &&& pure)

-- | Special case of 'classifyOn'.
classifyOnFst :: Ord a => [(a, b)] -> [(a, NonEmpty b)]
classifyOnFst = second (fmap snd) <<$>> classifyOn fst

-- | See 'classifyOnFst'.
classifyOnSnd :: Ord b => [(a, b)] -> [(b, NonEmpty a)]
classifyOnSnd = second (fmap fst) <<$>> classifyOn snd

-- | Like 'listDirectory', but returns paths relative to the input.
listDirectory' :: FilePath -> IO [FilePath]
listDirectory' d = map (d </>) <$> listDirectory d

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x
