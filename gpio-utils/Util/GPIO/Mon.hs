module Util.GPIO.Mon (mon) where

import Control.Monad (unless)
import Control.Monad.Except (MonadIO (..))
import Control.Monad.Loops (iterateM_)
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (diffUTCTime, getCurrentTime)
import RawFilePath (CreatePipe (CreatePipe), proc, processStdout, setStdout, startProcess)
import System.IO (hGetLine)

mon :: (MonadIO m) => ByteString -> (Bool -> Text -> m ()) -> Double -> Int -> m () -> m ()
mon gpioChip putLine debounce pin x = do
    p <-
        liftIO . startProcess $
            proc "gpiomon" ["-b", "-f", gpioChip, encodeUtf8 . pack $ show pin]
                `setStdout` CreatePipe
    liftIO getCurrentTime >>= iterateM_ \t0 -> do
        line <- liftIO . hGetLine $ processStdout p
        t1 <- liftIO getCurrentTime
        let ignore = diffUTCTime t1 t0 < realToFrac debounce
        putLine ignore $ pack line
        unless ignore x
        pure t1