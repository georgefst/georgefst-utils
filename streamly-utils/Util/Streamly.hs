module Util.Streamly where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Data.StreamK qualified as SK
import Streamly.Internal.Data.Stream.StreamK qualified as SK

withInit :: (Monad m) => m t -> (t -> S.Stream m a) -> S.Stream m a
withInit init_ stream = SK.toStream $ SK.unCross do
    m <- SK.mkCross $ SK.fromStream $ S.fromEffect init_
    SK.mkCross . SK.fromStream $ stream m

fromEmitter :: (S.MonadAsync m) => ((a -> m ()) -> m ()) -> S.Stream m a
fromEmitter f = withInit (liftIO newEmptyMVar) \m ->
    (S.catMaybes . S.parList id)
        [ S.fromEffect $ (\() -> Nothing) <$> f (liftIO . putMVar m)
        , S.repeatM $ Just <$> liftIO (takeMVar m)
        ]

-- TODO can this be implemented more efficiently in a single pass?
partitionEithers :: (Monad m) => S.Stream m (Either a b) -> (S.Stream m a, S.Stream m b)
partitionEithers s = (S.catLefts s, S.catRights s)
