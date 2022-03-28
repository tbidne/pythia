-- | Entrypoint for unit tests.
--
-- @since 0.1.0.0
module Main (main) where

import Test.Tasty qualified as Tasty
import Unit.Prelude
import Unit.Pythia.Services.Battery.Acpi qualified as Battery.Acpi
import Unit.Pythia.Services.Battery.UPower qualified as Battery.UPower
import Unit.Pythia.Services.Network.Connection.NmCli qualified as Connection.NmCli
import Unit.Pythia.Services.Network.IP.Local.IfConfig qualified as IP.Local.IfConfig
import Unit.Pythia.Services.Network.IP.Local.NmCli qualified as IP.Local.NmCli

-- | Runs unit tests.
--
-- @since 0.1.0.0
main :: IO ()
main = do
  Tasty.defaultMain $
    Tasty.testGroup
      "Unit tests"
      [ Battery.Acpi.tests,
        Battery.UPower.tests,
        Connection.NmCli.tests,
        IP.Local.IfConfig.tests,
        IP.Local.NmCli.tests
      ]
