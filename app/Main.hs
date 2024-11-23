-- | This modules provides an executable for querying system information.
--
-- @since 0.1
module Main (main) where

import Control.Exception.Annotation.Utils
  ( ExceptionProxy (MkExceptionProxy),
  )
import Control.Exception.Annotation.Utils qualified as AnnUtils
import Data.Proxy (Proxy (Proxy))
import Pythia.Control.Exception (PythiaException)
import Pythia.Runner (runPythiaIO)

-- | Runs the executable.
--
-- @since 0.1
main :: IO ()
main = do
  AnnUtils.setUncaughtExceptionDisplayInnerMatch
    noCallstacks
    (putStrLn . ("\n" <>))

  runPythiaIO
  where
    noCallstacks =
      [ MkExceptionProxy $ Proxy @PythiaException
      ]
