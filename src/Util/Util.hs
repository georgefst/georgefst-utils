module Util.Util where

import Control.Concurrent (threadDelay)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bool (bool)
import Data.Foldable (find)
import Data.List.Extra (enumerate)
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Tuple.Extra (second, (&&&))
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Type.Reflection (Typeable, typeRep)

-- | Invert a function. 'a' must be a finite type (and for efficiency, should be very small).
invert :: (Enum a, Bounded a, Eq b) => (a -> b) -> b -> Maybe a
invert = invert' enumerate

{- | A generalisation of 'invert'. The first argument is the sub-domain of the function on which to invert.
As with 'invert', this list must be finite, and should be small.
-}
invert' :: (Eq b) => [a] -> (a -> b) -> b -> Maybe a
invert' xs f y = fst <$> find ((== y) . snd) [(x, f x) | x <- xs]

tailSafe :: [a] -> [a]
tailSafe = \case
    [] -> []
    _ : xs -> xs

mwhen :: (Monoid p) => Bool -> p -> p
mwhen b x = if b then x else mempty

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen = flip $ bool id

clamp :: (Ord c) => (c, c) -> c -> c
clamp (l, u) = max l . min u

symbolValT :: forall a. (KnownSymbol a) => Text
symbolValT = T.pack $ symbolVal $ Proxy @a

showT :: (Show a) => a -> Text
showT = T.pack . show

typeRepT :: forall a. (Typeable a) => Text
typeRepT = showT $ typeRep @a

infixl 4 <<$>>
(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

infixl 4 <<*>>
(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

infixl 4 <<<*>>>
(<<<*>>>) :: (Applicative f, Applicative g, Applicative h) => f (g (h (a -> b))) -> f (g (h a)) -> f (g (h b))
(<<<*>>>) = liftA2 (<<*>>)

biVoid :: (Bifunctor p) => p a b -> p () ()
biVoid = bimap (const ()) (const ())

-- | Perform an action repeatedly until it errors. And run a callback on each success.
untilLeft :: (Monad m) => (a -> m ()) -> m (Either e a) -> m e
untilLeft f x = x >>= either pure (\a -> f a >> untilLeft f x)

mapRightM :: (Monad m) => (a -> m b) -> Either e a -> m (Either e b)
mapRightM f = either (return . Left) (fmap Right . f)

-- | Like 'groupOn', but with non-adjacent elements grouped, and the witness to equality returned.
classifyOn :: (Ord b) => (a -> b) -> [a] -> [(b, NonEmpty a)]
classifyOn f = Map.toList . Map.fromListWith (<>) . map (f &&& pure)

-- | Special case of 'classifyOn'.
classifyOnFst :: (Ord a) => [(a, b)] -> [(a, NonEmpty b)]
classifyOnFst = second (fmap snd) <<$>> classifyOn fst

-- | See 'classifyOnFst'.
classifyOnSnd :: (Ord b) => [(a, b)] -> [(b, NonEmpty a)]
classifyOnSnd = second (fmap fst) <<$>> classifyOn snd

-- | Like 'listDirectory', but returns paths relative to the input.
listDirectory' :: FilePath -> IO [FilePath]
listDirectory' d = map (d </>) <$> listDirectory d

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

-- | A more strongly-typed version of `threadDelay`.
threadDelay' :: NominalDiffTime -> IO ()
threadDelay' = threadDelay . round . (* 1_000_000) . nominalDiffTimeToSeconds

infixr 8 .:, .:., .::
(.:) :: (c -> c') -> (a -> b -> c) -> a -> b -> c'
(.:) = (.) . (.)
(.:.) :: (d -> d') -> (a -> b -> c -> d) -> a -> b -> c -> d'
(.:.) = (.:) . (.)
(.::) :: (e -> e') -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e'
(.::) = (.:.) . (.)
