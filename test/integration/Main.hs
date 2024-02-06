-- | Entrypoint for integration tests. These tests are primarily concerned
-- with exe output with the external services mocked. For instance, we mock
-- the output from the 'free' command, and then test that pythia correctly
-- parses it and outputs strings in the expected format.
--
-- @since 0.1
module Main (main) where

import Integration.Configuration qualified as Configuration
import Integration.Prelude
import Integration.Pythia.Services.Battery qualified as Battery
import Integration.Pythia.Services.GlobalIp qualified as GlobalIp
import Integration.Pythia.Services.Memory qualified as Memory
import Integration.Pythia.Services.NetConnection qualified as NetConnection
import Integration.Pythia.Services.NetInterface qualified as NetInterface
import Integration.Pythia.Services.Time qualified as Time
import Test.Tasty qualified as Tasty

-- | Runs integration tests.
--
-- @since 0.1
main :: IO ()
main = do
  Tasty.defaultMain
    $ Tasty.testGroup
      "Integration tests"
      [ Configuration.tests,
        Battery.tests,
        GlobalIp.tests,
        Memory.tests,
        NetConnection.tests,
        NetInterface.tests,
        Time.tests
      ]
