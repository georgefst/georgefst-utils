-- TODO update to new Okapi as soon as it's released: https://github.com/monadicsystems/okapi/issues/30
module Util.Streamly.Okapi where

import Control.Monad.State.Strict (MonadIO (..))
import Data.Foldable (asum)
import Data.Tuple.Extra (curry3, uncurry3)
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import Network.Wai.Handler.Warp qualified as Warp
import Okapi (OkapiT, Result, makeOkapiApp)
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly qualified as S

data Opts a = Opts
    { warpSettings :: Warp.Settings -- TODO what if the settings passed in override the logger? just say not to in Haddocks?
    , routes :: [OkapiT IO (a, OkapiT IO Result)]
    }

data Item a
    = Event a
    | WarpLog Request Status (Maybe Integer)

stream ::
    (MonadIO m) =>
    Opts a ->
    S.Stream m (Item a)
stream Opts{..} = S.morphInner liftIO $ S.fromEmitter \f ->
    Warp.runSettings (Warp.setLogger (curry3 $ f . uncurry3 WarpLog) warpSettings)
        . makeOkapiApp id
        . asum
        $ map
            ( (=<<) \(a, r) -> do
                liftIO $ f $ Event a -- put action in the stream
                r -- return result to client
            )
            routes
