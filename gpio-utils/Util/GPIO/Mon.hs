module Util.GPIO.Mon (mon) where

import Control.Monad.Except (MonadIO (..))
import Control.Monad.Loops (iterateM_)
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import RawFilePath (CreatePipe (CreatePipe), proc, processStdout, setStdout, startProcess)
import System.IO (hGetLine)

mon :: (MonadIO m) => ByteString -> (Bool -> Text -> m ()) -> NominalDiffTime -> Int -> m ()
mon gpioChip putLine debounce pin = do
    p <-
        liftIO . startProcess $
            proc "gpiomon" ["-b", "-f", gpioChip, encodeUtf8 . pack $ show pin]
                `setStdout` CreatePipe
    liftIO getCurrentTime >>= iterateM_ \t0 -> do
        line <- liftIO . hGetLine $ processStdout p
        t1 <- liftIO getCurrentTime
        putLine (diffUTCTime t1 t0 < debounce) (pack line)
        pure t1
