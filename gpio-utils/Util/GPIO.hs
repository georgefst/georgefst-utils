-- TODO wrap up a state-with-map monad to encapsulate this better
-- TODO get a proper Haskell GPIO library (hpio?) working with the modern `libgpiod` interface
module Util.GPIO (Handle, reset, set, mon) where

import Control.Monad.IO.Class (MonadIO (..))
import System.OsString.Posix (PosixString, unsafeEncodeUtf)
import System.Posix.Types (ProcessID)
import Util.GPIO.Mon (mon)
import Util.Posix.Process (spawn, terminateAndWait)

newtype Handle = Handle {unwrap :: ProcessID}

reset :: (MonadIO m) => Handle -> m ()
reset h = liftIO $ terminateAndWait h.unwrap

set :: (MonadIO m) => PosixString -> [Int] -> m Handle
set gpioChip xs =
    liftIO
        . fmap Handle
        $ spawn
            (unsafeEncodeUtf "gpioset")
            ( [unsafeEncodeUtf "--chip", gpioChip]
                <> map ((<> unsafeEncodeUtf "=1") . unsafeEncodeUtf . show @Int) xs
            )
