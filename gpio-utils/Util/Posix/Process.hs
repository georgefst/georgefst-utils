-- a semi-vibed minimal PosixPath process library in the absence of https://github.com/haskell/process/issues/252
module Util.Posix.Process (
    spawn,
    spawnWithStdout,
    runAndWait,
    terminateAndWait,
) where

import Control.Monad (void)
import System.IO (Handle)
import System.OsString.Posix (PosixString)
import System.Posix.IO (closeFd, createPipe, dupTo, fdToHandle, stdOutput)
import System.Posix.Process.PosixString (executeFile, forkProcess, getProcessStatus)
import System.Posix.Signals (sigTERM, signalProcess)
import System.Posix.Types (ProcessID)

-- | Run the given command (looked up on the @PATH@) in a new process.
spawn :: PosixString -> [PosixString] -> IO ProcessID
spawn cmd args = forkProcess $ executeFile cmd True args Nothing

-- | Like `spawn`, but the child's stdout is redirected to a pipe, whose read end is returned as a `Handle`.
spawnWithStdout :: PosixString -> [PosixString] -> IO (ProcessID, Handle)
spawnWithStdout cmd args = do
    (readEnd, writeEnd) <- createPipe
    pid <- forkProcess do
        closeFd readEnd
        _ <- dupTo writeEnd stdOutput
        closeFd writeEnd
        executeFile cmd True args Nothing
    closeFd writeEnd
    (pid,) <$> fdToHandle readEnd

-- | Run the given command and wait for it to terminate.
runAndWait :: PosixString -> [PosixString] -> IO ()
runAndWait cmd args = spawn cmd args >>= void . getProcessStatus True False

-- | Send @SIGTERM@ to the given process and wait for it to terminate.
terminateAndWait :: ProcessID -> IO ()
terminateAndWait pid = do
    signalProcess sigTERM pid
    void $ getProcessStatus True False pid
