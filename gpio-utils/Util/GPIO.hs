-- TODO wrap up a state-with-map monad to encapsulate this better
-- TODO get a proper Haskell GPIO library (hpio?) working with the modern `libgpiod` interface
module Util.GPIO (Handle, reset, set, mon) where

import Control.Monad.Except (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import RawFilePath (Inherit, Process, proc, startProcess, terminateProcess)
import Util.GPIO.Mon (mon)

newtype Handle = Handle {unwrap :: Process Inherit Inherit Inherit}

reset :: (MonadIO m) => Handle -> m ()
reset h = liftIO $ terminateProcess h.unwrap

set :: (MonadIO m) => ByteString -> [Int] -> m Handle
set gpioChip xs =
    liftIO
        . fmap Handle
        . startProcess
        . proc "gpioset"
        $ "--mode=signal" : gpioChip : map ((<> "=1") . encodeUtf8 . pack . show) xs
