module Util.Streamly.GPIO where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Streamly.Data.Stream.Prelude qualified as S
import Util.GPIO qualified as GPIO
import Util.Streamly qualified as S

data Opts = Opts
    { chip :: ByteString
    , pin :: Int
    , debounce :: NominalDiffTime
    }

data Item
    = Event
    | OutLine {line :: Text}

stream :: (S.MonadAsync m) => Opts -> S.Stream m Item
stream Opts{..} = S.fromEmitter \f ->
    GPIO.mon chip (\line -> f OutLine{..} >> f Event) debounce pin
