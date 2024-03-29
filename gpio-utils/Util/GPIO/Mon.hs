module Util.GPIO.Mon (mon) where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import RawFilePath (CreatePipe (CreatePipe), proc, processStdout, setStdout, startProcess)
import System.IO (hGetLine)

mon :: (MonadIO m) => ByteString -> m () -> NominalDiffTime -> Int -> m ()
mon gpioChip action debounce pin = do
    p <-
        liftIO . startProcess $
            proc "gpiomon" ["--edges", "falling", "--chip", gpioChip, "--debounce-period", debounceString, showBS pin]
                `setStdout` CreatePipe
    forever do
        _line <- liftIO . hGetLine $ processStdout p
        action
  where
    debounceString = (<> "us") . showBS @Integer . round . (* 1_000_000) $ nominalDiffTimeToSeconds debounce
    showBS :: (Show a) => a -> ByteString
    showBS = encodeUtf8 . pack . show
