module Util.GPIO.Mon (mon) where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import System.IO (hGetLine)
import System.OsString.Posix (PosixString, unsafeEncodeUtf)
import Util.Posix.Process (spawnWithStdout)

mon :: (MonadIO m) => PosixString -> m () -> NominalDiffTime -> Int -> m ()
mon gpioChip action debounce pin = do
    (_pid, h) <-
        liftIO $
            spawnWithStdout
                (unsafeEncodeUtf "gpiomon")
                [ unsafeEncodeUtf "--edges"
                , unsafeEncodeUtf "falling"
                , unsafeEncodeUtf "--chip"
                , gpioChip
                , unsafeEncodeUtf "--debounce-period"
                , debounceString
                , unsafeEncodeUtf $ show @Int pin
                ]
    forever do
        _line <- liftIO $ hGetLine h
        action
  where
    debounceString = unsafeEncodeUtf . (<> "us") . show @Integer . round . (* 1_000_000) $ nominalDiffTimeToSeconds debounce
