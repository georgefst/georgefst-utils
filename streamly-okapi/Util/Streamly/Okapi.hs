module Util.Streamly.Okapi where

import Control.Monad.State.Strict (MonadIO (..))
import Network.HTTP.Types (Status, status404)
import Network.Wai (Request, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.App (Node, choice, withDefault)
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly qualified as S
import Util.Util ((.:.))

data Opts a = Opts
    { warpSettings :: Warp.Settings -- TODO what if the settings passed in override the logger? just say not to in Haddocks?
    -- TODO I'm not really happy about the callbacky-ness of this
    -- ask Okapi maintainer to consider such use cases while developing the new API?
    , routes :: (a -> IO ()) -> [Node '[]]
    }

data Item a
    = Event a
    | WarpLog Request Status (Maybe Integer)

stream ::
    (MonadIO m) =>
    Opts a ->
    S.Stream m (Item a)
stream Opts{..} = S.morphInner liftIO $ S.fromEmitter \f ->
    Warp.runSettings (Warp.setLogger (f .:. WarpLog) warpSettings)
        . withDefault (choice . routes $ f . Event)
        $ \_ resp -> resp $ responseLBS status404 [] "Not Found..."
