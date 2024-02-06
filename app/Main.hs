-- | This modules provides an executable for querying system information.
--
-- @since 0.1
module Main (main) where

import Data.Proxy (Proxy (Proxy))
import Effects.Exception
  ( ExceptionProxy (MkExceptionProxy),
    setUncaughtExceptionDisplayCSNoMatch,
  )
import Pythia.Control.Exception
  ( CommandException,
    NoActionsRunException,
    NotSupportedException,
    SomeExceptions,
  )
import Pythia.Runner (runPythia)
import Pythia.Runner.Toml (ConfigException)

-- | Runs the executable.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionDisplayCSNoMatch
    noCallstacks
    (putStrLn . ("\n" <>))

  runPythia
  where
    noCallstacks =
      [ MkExceptionProxy $ Proxy @ConfigException,
        MkExceptionProxy $ Proxy @CommandException,
        MkExceptionProxy $ Proxy @NoActionsRunException,
        MkExceptionProxy $ Proxy @NotSupportedException,
        MkExceptionProxy $ Proxy @SomeExceptions
      ]
