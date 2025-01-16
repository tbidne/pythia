-- | This modules provides an executable for querying system information.
--
-- @since 0.1
module Main (main) where

import Control.Exception.Annotation.Utils
  ( ExceptionProxy (MkExceptionProxy),
  )
import Control.Exception.Annotation.Utils qualified as AnnUtils
import Pythia.Control.Exception (PythiaException)
import Pythia.Runner (runPythia)

-- | Runs the executable.
--
-- @since 0.1
main :: IO ()
main = do
  AnnUtils.setIgnoreKnownCallStackHandler noCallstacks

  runPythia
  where
    noCallstacks =
      [ MkExceptionProxy @PythiaException
      ]
