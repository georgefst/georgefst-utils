-- TODO use backpack?

{- | a drop-in replacement for `Util.GPIO` for systems (RPiOS) where `gpioset` commands are weirdly persistent
from documentation I've seen, the other behaviour seems to be what is supposed to happen
-}
module Util.GPIO.Persistent (Handle, reset, set, mon) where

import Control.Monad (void)
import Control.Monad.Except (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import RawFilePath (proc, readProcessWithExitCode)
import Util.GPIO.Mon (mon)

data Handle = Handle ByteString [Int]

reset :: (MonadIO m) => Handle -> m ()
reset (Handle gpioChip xs) =
    void . liftIO $
        readProcessWithExitCode . proc "gpioset" $
            gpioChip : map ((<> "=0") . encodeUtf8 . pack . show) xs

set :: (MonadIO m) => ByteString -> [Int] -> m Handle
set gpioChip xs = do
    void . liftIO $
        readProcessWithExitCode . proc "gpioset" $
            gpioChip : map ((<> "=1") . encodeUtf8 . pack . show) xs
    pure $ Handle gpioChip xs
