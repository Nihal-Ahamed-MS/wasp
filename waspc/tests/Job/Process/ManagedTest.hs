module Job.Process.ManagedTest where

import Control.Concurrent (newChan, threadDelay)
import Control.Exception (finally)
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Info (os)
import qualified System.Process as P
import Test.Hspec (Spec, describe, expectationFailure, it, pendingWith, shouldReturn, shouldSatisfy)
import qualified Wasp.Job as J
import Wasp.Job.Process.Managed (getManagedProcessExitCode, startManagedProcess, stopManagedProcess)

spec_ManagedProcess :: Spec
spec_ManagedProcess =
  describe "ManagedProcess" $ do
    it "kills process-group descendants after the root process exits" $ do
      if os == "mingw32"
        then pendingWith "Unix process-group specific regression test"
        else do
          (pidFilePath, pidFileHandle) <- openTempFile "/tmp" "wasp-managed-child.pid"
          hClose pidFileHandle
          removeFile pidFilePath

          chan <- newChan
          managedProcess <- startManagedProcess (P.proc "sh" ["-c", childProcessScript pidFilePath]) J.Server chan
          let cleanup = stopManagedProcess managedProcess >> removeFileIfExists pidFilePath
          ( do
              waitUntil "child pid file" $ doesFileExist pidFilePath
              childPid <- readFile pidFilePath
              waitUntil "root process exit" $ isJust <$> getManagedProcessExitCode managedProcess
              isProcessAlive childPid `shouldReturn` True

              startedAt <- getCurrentTime
              stopManagedProcess managedProcess
              stoppedAt <- getCurrentTime

              realToFrac (stoppedAt `diffUTCTime` startedAt) `shouldSatisfy` (< (2 :: Double))
              waitUntil "child process exit" $ not <$> isProcessAlive childPid
            )
            `finally` cleanup

childProcessScript :: FilePath -> String
childProcessScript pidFilePath =
  "trap '' INT; "
    <> "(trap '' INT; while true; do sleep 1; done) & "
    <> "echo $! > "
    <> shellQuote pidFilePath
    <> "; "
    <> "sleep 0.2; "
    <> "exit 0"

shellQuote :: String -> String
shellQuote value = "'" <> concatMap quoteChar value <> "'"
  where
    quoteChar '\'' = "'\\''"
    quoteChar char = [char]

waitUntil :: String -> IO Bool -> IO ()
waitUntil label condition = go (50 :: Int)
  where
    go remainingAttempts
      | remainingAttempts <= 0 = expectationFailure $ "Timed out waiting for " <> label
      | otherwise = do
          result <- condition
          if result
            then return ()
            else do
              threadDelay 100000
              go $ remainingAttempts - 1

isProcessAlive :: String -> IO Bool
isProcessAlive pid = do
  (exitCode, _, _) <- P.readCreateProcessWithExitCode (P.proc "kill" ["-0", trim pid]) ""
  return $ exitCode == ExitSuccess

trim :: String -> String
trim = unwords . words

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath
