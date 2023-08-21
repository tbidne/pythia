-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Test.Tasty qualified as Tasty
import Unit.Prelude
import Unit.Pythia.Services.Battery.Acpi qualified as Battery.Acpi
import Unit.Pythia.Services.Battery.UPower qualified as Battery.UPower
import Unit.Pythia.Services.Memory.Free qualified as Memory.Free
import Unit.Pythia.Services.NetInterface.Ip qualified as NetInterface.Ip
import Unit.Pythia.Services.NetInterface.NmCli qualified as NetInterface.NmCli

-- | Runs unit tests.
--
-- @since 0.1
main :: IO ()
main = do
  Tasty.defaultMain
    $ Tasty.testGroup
      "Unit tests"
      [ Battery.Acpi.tests,
        Battery.UPower.tests,
        Memory.Free.tests,
        NetInterface.Ip.tests,
        NetInterface.NmCli.tests
      ]
