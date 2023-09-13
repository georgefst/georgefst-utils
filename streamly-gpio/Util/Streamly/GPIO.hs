module Util.Streamly.GPIO where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Streamly.Data.Stream.Prelude qualified as S
import Util.GPIO qualified as GPIO
import Util.Streamly qualified as S

data Opts = Opts
    { chip :: ByteString
    , pin :: Int
    , debounce :: Double
    }

data Item
    = Event
    | OutLine {ignoring :: Bool, line :: Text}

stream :: (S.MonadAsync m) => Opts -> S.Stream m Item
stream Opts{..} = S.fromEmitter \f ->
    GPIO.mon chip (\(not -> ignoring) line -> f OutLine{..} >> unless ignoring (f Event)) debounce pin
