-- | This modules provides an executable for querying system information.
--
-- @since 0.1
module Main (main) where

import Control.Exception (displayException)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Pythia.Runner (runPythia)

-- | Runs the executable.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn ("\n" <> displayException ex)

  runPythia
