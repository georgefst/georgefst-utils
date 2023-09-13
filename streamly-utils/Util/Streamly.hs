module Util.Streamly where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromMaybe)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Tuple.Extra (thd3)
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Data.StreamK qualified as SK
import Streamly.Internal.Data.Stream qualified as S
import Streamly.Internal.Data.Stream.StreamK qualified as SK
import Util.Util (threadDelay')

-- TODO look at `S.bracket` implementation for better way to do this?
withInit :: (Monad m) => m t -> (t -> S.Stream m a) -> S.Stream m a
withInit init_ stream = SK.toStream $ SK.unCross do
    m <- SK.mkCross $ SK.fromStream $ S.fromEffect init_
    SK.mkCross . SK.fromStream $ stream m

-- TODO ask upstream about easier approaches?
fromEmitter :: (S.MonadAsync m) => ((a -> m ()) -> m ()) -> S.Stream m a
fromEmitter f = withInit (liftIO newEmptyMVar) \m ->
    (S.catMaybes . S.parList id)
        [ S.fromEffect $ (\() -> Nothing) <$> f (liftIO . putMVar m)
        , S.repeatM $ Just <$> liftIO (takeMVar m)
        ]

-- TODO can this be implemented more efficiently in a single pass?
partitionEithers :: (Monad m) => S.Stream m (Either a b) -> (S.Stream m a, S.Stream m b)
partitionEithers s = (S.catLefts s, S.catRights s)

-- TODO use `StateT` internally for more readable implementation
-- TODO use `DList` internally for performance
-- TODO avoid internal uses of `error`
-- group events which are less then the given interval
groupByTime :: (S.MonadAsync m) => NominalDiffTime -> S.Stream m a -> S.Stream m (NonEmpty a)
groupByTime interval = fmap (fromMaybe (error "groupByTime") . nonEmpty) . f2 . f1
  where
    -- delay everything by `interval`, and insert 'Nothing' markers where the value first came in
    f1 =
        S.parConcat id . fmap \x ->
            S.cons Nothing $ S.fromEffect (liftIO (threadDelay' interval) >> pure (Just x))
    -- ignore any event which appears within `interval` of a 'Nothing'
    f2 =
        S.mapMaybe id . fmap thd3 . flip S.postscanlM' (pure (const False, [], error "groupByTime")) \(tooSoon, this, _) -> \case
            Just x -> do
                t <- liftIO getCurrentTime
                pure
                    if tooSoon t
                        then (tooSoon, new, Nothing)
                        else (tooSoon, [], Just new)
              where
                new = this ++ [x]
            Nothing -> do
                t <- liftIO getCurrentTime
                pure (\t' -> diffUTCTime t' t < interval, this, Nothing)
