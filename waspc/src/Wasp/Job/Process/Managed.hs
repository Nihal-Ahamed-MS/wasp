module Wasp.Job.Process.Managed
  ( ManagedProcess,
    getManagedProcessExitCode,
    startManagedProcess,
    stopManagedProcess,
    waitForManagedProcess,
  )
where

import Control.Concurrent (Chan, threadDelay, writeChan)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import Data.Text.Encoding (decodeUtf8)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import qualified System.Info
import qualified System.Process as P
import qualified Wasp.Job as J

data ManagedProcess = ManagedProcess
  { waitForManagedProcess :: IO ExitCode,
    stopManagedProcess :: IO (),
    getManagedProcessExitCode :: IO (Maybe ExitCode)
  }

-- Managed processes are Wasp-owned children that don't read from stdin.
-- That lets us close stdin, isolate the process tree on Unix with create_group,
-- and use Windows process jobs so ProcessHandle operations can cover children.
-- We still pipe and drain stdout/stderr: System.Process documents NoStream as
-- unsafe for output when the child writes to the closed file descriptor.
configureManagedProcess :: P.CreateProcess -> P.CreateProcess
configureManagedProcess process =
  process
    { P.create_group = System.Info.os /= "mingw32",
      P.use_process_jobs = System.Info.os == "mingw32",
      P.std_in = P.NoStream,
      P.std_out = P.CreatePipe,
      P.std_err = P.CreatePipe
    }

startManagedProcess :: P.CreateProcess -> J.JobType -> Chan J.JobMessage -> IO ManagedProcess
startManagedProcess process jobType chan = do
  (maybeStdin, maybeStdout, maybeStderr, processHandle) <- P.createProcess $ configureManagedProcess process
  maybeProcessGroupPid <- fmap show <$> P.getPid processHandle
  stdoutAsync <- async $ forwardOutput chan jobType maybeStdout J.Stdout
  stderrAsync <- async $ forwardOutput chan jobType maybeStderr J.Stderr
  let closeHandles = mapM_ closeHandleIfOpen [maybeStdin, maybeStdout, maybeStderr]
  let waitForProcessAndOutput = do
        exitCode <- P.waitForProcess processHandle
        -- Wait for any buffered output after the root process exits.
        waitForOutput stdoutAsync
        waitForOutput stderrAsync
        closeHandles
        return exitCode
  let stopProcessAndOutput = do
        stopProcessTree processHandle maybeProcessGroupPid
        closeHandles
        cancel stdoutAsync
        cancel stderrAsync
  return $
    ManagedProcess
      { waitForManagedProcess = waitForProcessAndOutput,
        stopManagedProcess = stopProcessAndOutput,
        getManagedProcessExitCode = P.getProcessExitCode processHandle
      }

waitForOutput :: Async a -> IO ()
waitForOutput outputAsync = void $ waitCatch outputAsync

forwardOutput :: Chan J.JobMessage -> J.JobType -> Maybe Handle -> J.JobOutputType -> IO ()
forwardOutput _ _ Nothing _ = return ()
forwardOutput chan jobType (Just handle) outputType = forwardChunks
  where
    forwardChunks = do
      output <- BS.hGetSome handle 4096
      if BS.null output
        then return ()
        else do
          writeChan chan $
            J.JobMessage
              { J._data = J.JobOutput (decodeUtf8 output) outputType,
                J._jobType = jobType
              }
          forwardChunks

closeHandleIfOpen :: Maybe Handle -> IO ()
closeHandleIfOpen Nothing = return ()
closeHandleIfOpen (Just handle) = void (try $ hClose handle :: IO (Either SomeException ()))

stopProcessTree :: P.ProcessHandle -> Maybe String -> IO ()
stopProcessTree processHandle maybeProcessGroupPid = do
  isRunning <- isProcessTreeRunning processHandle maybeProcessGroupPid
  when isRunning $ do
    -- First ask the tree to stop, then escalate if it doesn't release resources
    -- such as the dev server port in time.
    interruptProcessTree processHandle maybeProcessGroupPid
    gracefullyStopped <- waitForProcessTreeExit processHandle maybeProcessGroupPid gracefulStopTimeoutMicroseconds
    -- The root process can exit before its descendants, so use the process
    -- group pid captured while the root was still alive.
    unless gracefullyStopped $ do
      terminateProcessTree processHandle maybeProcessGroupPid
      void $ waitForProcessTreeExit processHandle maybeProcessGroupPid hardStopTimeoutMicroseconds

isProcessTreeRunning :: P.ProcessHandle -> Maybe String -> IO Bool
isProcessTreeRunning processHandle maybeProcessGroupPid =
  if System.Info.os == "mingw32"
    then isProcessRunning processHandle
    else case maybeProcessGroupPid of
      Nothing -> isProcessRunning processHandle
      Just processGroupPid -> isProcessGroupRunning processGroupPid

isProcessRunning :: P.ProcessHandle -> IO Bool
isProcessRunning processHandle = do
  maybeExitCode <- P.getProcessExitCode processHandle
  return $ isNothing maybeExitCode

waitForProcessTreeExit :: P.ProcessHandle -> Maybe String -> Int -> IO Bool
waitForProcessTreeExit processHandle maybeProcessGroupPid timeoutMicroseconds = waitForExitOrTimeout timeoutMicroseconds
  where
    waitForExitOrTimeout remainingMicroseconds
      | remainingMicroseconds <= 0 = return False
      | otherwise = do
          stillRunning <- isProcessTreeRunning processHandle maybeProcessGroupPid
          if stillRunning
            then do
              threadDelay pollIntervalMicroseconds
              waitForExitOrTimeout $ remainingMicroseconds - pollIntervalMicroseconds
            else return True

interruptProcessTree :: P.ProcessHandle -> Maybe String -> IO ()
interruptProcessTree processHandle maybeProcessGroupPid =
  if System.Info.os == "mingw32"
    then P.terminateProcess processHandle
    else case maybeProcessGroupPid of
      Nothing -> P.interruptProcessGroupOf processHandle
      Just processGroupPid -> void $ signalProcessGroup "INT" processGroupPid

terminateProcessTree :: P.ProcessHandle -> Maybe String -> IO ()
terminateProcessTree processHandle maybeProcessGroupPid =
  if System.Info.os == "mingw32"
    then terminateRootProcessIfRunning processHandle
    else do
      void $ maybe (return False) (signalProcessGroup "KILL") maybeProcessGroupPid
      terminateRootProcessIfRunning processHandle

terminateRootProcessIfRunning :: P.ProcessHandle -> IO ()
terminateRootProcessIfRunning processHandle = do
  isRunning <- isProcessRunning processHandle
  when isRunning $ P.terminateProcess processHandle

isProcessGroupRunning :: String -> IO Bool
isProcessGroupRunning = signalProcessGroup "0"

signalProcessGroup :: String -> String -> IO Bool
signalProcessGroup signal pid = do
  -- System.Process exposes group interrupts but not arbitrary group signals.
  signalResult <- try $ P.readCreateProcessWithExitCode (P.proc "kill" ["-" <> signal, "-" <> pid]) ""
  case signalResult :: Either SomeException (ExitCode, String, String) of
    Left _ -> return False
    Right (ExitSuccess, _, _) -> return True
    Right (ExitFailure {}, _, _) -> return False

gracefulStopTimeoutMicroseconds :: Int
gracefulStopTimeoutMicroseconds = 5 * secondsInMicroseconds

hardStopTimeoutMicroseconds :: Int
hardStopTimeoutMicroseconds = 5 * secondsInMicroseconds

pollIntervalMicroseconds :: Int
pollIntervalMicroseconds = secondsInMicroseconds `div` 10

secondsInMicroseconds :: Int
secondsInMicroseconds = 1000000
