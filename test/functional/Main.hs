-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Functional.Prelude
import Functional.Pythia.Services.Battery qualified as Battery
import Functional.Pythia.Services.GlobalIp qualified as GlobalIp
import Functional.Pythia.Services.NetInterface qualified as NetInterface
import System.Environment qualified as Env
import Test.Tasty qualified as Tasty

-- | Runs unit tests.
--
-- @since 0.1
main :: IO ()
main = do
  shouldRun <- Env.lookupEnv "RUN_FUNCTIONAL"
  case shouldRun of
    Just "true" -> do
      putStrLn "running tests"
      Tasty.defaultMain $
        Tasty.testGroup
          "Functional tests"
          [ Battery.tests,
            GlobalIp.tests,
            NetInterface.tests
          ]
    _ -> putStrLn "*** Functional Tests Disabled ***"
