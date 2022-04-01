-- | Entrypoint for unit tests.
--
-- @since 0.1.0.0
module Main (main) where

import Test.Tasty qualified as Tasty
import Unit.Prelude
import Unit.Pythia.Services.Battery.Acpi qualified as Battery.Acpi
import Unit.Pythia.Services.Battery.UPower qualified as Battery.UPower
import Unit.Pythia.Services.Network.Interface.Ip qualified as Interface.Ip
import Unit.Pythia.Services.Network.Interface.NmCli qualified as Interface.NmCli

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
        Interface.Ip.tests,
        Interface.NmCli.tests
      ]
