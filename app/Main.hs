-- | This modules provides an executable for querying system information.
--
-- @since 0.1
module Main (main) where

import Control.Exception (displayException)
import Effectful (runEff)
import Effectful.FileSystem.FileReader.Dynamic
  ( runFileReaderDynamicIO,
  )
import Effectful.FileSystem.PathReader.Dynamic
  ( runPathReaderDynamicIO,
  )
import Effectful.Optparse.Static (runOptparseStaticIO)
import Effectful.Process.Typed (runTypedProcess)
import Effectful.Terminal.Static (runTerminalStaticIO)
import Effectful.Time.Dynamic (runTimeDynamicIO)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Pythia.Runner (runPythia)

-- | Runs the executable.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn ("\n" <> displayException ex)

  run runPythia
  where
    run =
      runEff
        . runFileReaderDynamicIO
        . runOptparseStaticIO
        . runPathReaderDynamicIO
        . runTerminalStaticIO
        . runTimeDynamicIO
        . runTypedProcess
