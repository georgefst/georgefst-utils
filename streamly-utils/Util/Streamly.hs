module Util.Streamly where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromMaybe)
import Data.Time (NominalDiffTime)
import Streamly.Data.Fold.Prelude qualified as SF
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Data.StreamK qualified as SK
import Streamly.Internal.Data.StreamK qualified as SK
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

-- | Group events which occur within the given interval of each other.
groupByTime :: (S.MonadAsync m) => NominalDiffTime -> S.Stream m a -> S.Stream m (NonEmpty a)
groupByTime interval =
    fmap (fromMaybe (error "groupByTime 1") . nonEmpty . fmap (snd . fst))
        -- each group ends with the event where `i == lastEvent`
        -- i.e. where no other event has been seen after `interval`
        . S.foldMany (SF.takeEndBy (\((i, _), lastEvent) -> i == lastEvent) SF.toList)
        . S.mapMaybe (\(lastEvent, x) -> (,lastEvent) <$> x)
        . S.postscan
            ( flip SF.foldl' (error "groupByTime 2", Nothing) \(lastEvent, _) -> \case
                Left i -> (i, Nothing)
                Right x -> (lastEvent, Just x)
            )
        . S.parList id
        . sequence
            -- the original stream - indicates where each event actually occurred
            [ fmap (Left . fst)
            , -- each event delayed by `interval`
              S.mapM (\x -> liftIO (threadDelay' interval) >> pure (Right x))
            ]
        . S.zipWith (,) (S.enumerateFrom @Int 0)
