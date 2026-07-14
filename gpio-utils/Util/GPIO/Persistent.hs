-- TODO use backpack?

{- | a drop-in replacement for `Util.GPIO` for systems where GPIO pins prefer to be set persistently
investigations continue, but it seems that this approach is more suited to the Raspberry Pi 5's chip, but not the Pi 3
-}
module Util.GPIO.Persistent (Handle, reset, set, mon) where

import Control.Monad.IO.Class (MonadIO (..))
import System.OsString.Posix (PosixString, unsafeEncodeUtf)
import Util.GPIO.Mon (mon)
import Util.Posix.Process (runAndWait)

data Handle = Handle PosixString [Int]

reset :: (MonadIO m) => Handle -> m ()
reset (Handle gpioChip xs) = gpioset gpioChip (unsafeEncodeUtf "0") xs

set :: (MonadIO m) => PosixString -> [Int] -> m Handle
set gpioChip xs = do
    gpioset gpioChip (unsafeEncodeUtf "1") xs
    pure $ Handle gpioChip xs

gpioset :: (MonadIO m) => PosixString -> PosixString -> [Int] -> m ()
gpioset gpioChip value xs =
    liftIO $
        runAndWait
            (unsafeEncodeUtf "gpioset")
            ( [unsafeEncodeUtf "--chip", gpioChip, unsafeEncodeUtf "--toggle", unsafeEncodeUtf "0"]
                <> map ((<> unsafeEncodeUtf "=" <> value) . unsafeEncodeUtf . show @Int) xs
            )
