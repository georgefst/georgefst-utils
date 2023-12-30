module Util.GPIO.Mon (mon) where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import RawFilePath (CreatePipe (CreatePipe), proc, processStdout, setStdout, startProcess)
import System.IO (hGetLine)

-- TODO "debounced" events are very often just the button being released
-- if we had a way to distinguish (which I think we would if not for dodgy wires), we could set the threshold much lower
mon :: (MonadIO m) => ByteString -> (Text -> m ()) -> NominalDiffTime -> Int -> m ()
mon gpioChip putLine debounce pin = do
    p <-
        liftIO . startProcess $
            proc "gpiomon" ["--edges", "falling", "--chip", gpioChip, "--debounce-period", debounceString, showBS pin]
                `setStdout` CreatePipe
    forever do
        line <- liftIO . hGetLine $ processStdout p
        putLine (pack line)
  where
    debounceString = (<> "us") . showBS @Integer . round . (* 1_000_000) $ nominalDiffTimeToSeconds debounce
    showBS :: (Show a) => a -> ByteString
    showBS = encodeUtf8 . pack . show
