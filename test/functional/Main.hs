-- | Entrypoint for unit tests.
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
import System.Environment.Guard
import Test.Tasty qualified as Tasty

-- | Runs unit tests.
--
-- @since 0.1
main :: IO ()
main =
  guardOrElse'
    "RUN_FUNCTIONAL"
    ExpectEnvSet
    tests
    (putStrLn "*** Functional tests disabled. Enable with RUN_FUNCTIONAL=1. ***")
  where
    tests =
      Tasty.defaultMain $
        Tasty.testGroup
          "Functional tests"
          [ Battery.tests,
            GlobalIp.tests,
            Memory.tests,
            NetConn.tests,
            NetInterface.tests,
            Time.tests
          ]
