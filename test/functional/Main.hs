-- | Entrypoint for functional tests. This runs the tests for real, hence
-- guarded behind an environment variable, as the tests will crash if the
-- external apps (e.g. free) do not exist.
--
-- @since 0.1
module Main (main) where

import Functional.Prelude
import Functional.Pythia.Services.Battery qualified as Battery
import Functional.Pythia.Services.GlobalIp qualified as GlobalIp
import Functional.Pythia.Services.Memory qualified as Memory
import Functional.Pythia.Services.NetConnection qualified as NetConn
import Functional.Pythia.Services.NetInterface qualified as NetInterface
import Functional.Pythia.Services.Time qualified as Time
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.IO qualified as IO
import Test.Tasty qualified as Tasty

-- | Runs unit tests.
--
-- @since 0.1
main :: IO ()
main =
  guardOrElse'
    "RUN_FUNC"
    ExpectEnvSet
    tests
    (IO.putStrLn "*** Functional tests disabled. Enable with RUN_FUNC=1. ***")
  where
    tests =
      Tasty.defaultMain
        $ Tasty.testGroup
          "Functional tests"
          [ Battery.tests,
            GlobalIp.tests,
            Memory.tests,
            NetConn.tests,
            NetInterface.tests,
            Time.tests
          ]
