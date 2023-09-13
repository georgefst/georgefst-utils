module Util.Streamly.GPIO where

import Control.Monad (unless)
import Control.Monad.State (evalStateT, get, modify, put)
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Numeric.Natural
import Streamly.Data.Stream.Prelude qualified as S
import Util.GPIO qualified as GPIO
import Util.Streamly qualified as S

data Opts = Opts
    { chip :: ByteString
    , pin :: Int
    , debounce :: NominalDiffTime
    , window :: NominalDiffTime
    }

data Item
    = Event {repeats :: Natural} -- TODO always 1 when window is 0 - model this with type families?
    | OutLine {ignoring :: Bool, line :: Text}

stream :: (S.MonadAsync m) => Opts -> S.Stream m Item
stream Opts{..} = S.fromEmitter \((lift .) -> f) ->
    flip evalStateT (0 :: Natural) $
        GPIO.mon
            chip
            ( \(not -> ignoring) line interval -> do
                f OutLine{..}
                unless ignoring $
                    if interval < window
                        then modify succ
                        else do
                            repeats <- get
                            put 0
                            f $ Event repeats
            )
            debounce
            pin
