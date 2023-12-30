module Util.Streamly.GPIO where

import Data.ByteString (ByteString)
import Data.Time (NominalDiffTime)
import Streamly.Data.Stream.Prelude qualified as S
import Util.GPIO qualified as GPIO
import Util.Streamly qualified as S

data Opts = Opts
    { chip :: ByteString
    , pin :: Int
    , debounce :: NominalDiffTime
    }

stream :: (S.MonadAsync m) => Opts -> S.Stream m ()
stream Opts{..} = S.fromEmitter \f ->
    GPIO.mon chip (f ()) debounce pin
