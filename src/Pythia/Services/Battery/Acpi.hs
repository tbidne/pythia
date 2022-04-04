-- | This module provides functionality for retrieving battery information
-- using ACPI.
--
-- @since 0.1.0.0
module Pythia.Services.Battery.Acpi
  ( -- * Query
    batteryShellApp,
    supported,

    -- * Misc
    AcpiException (..),
    parseBattery,
  )
where

import Data.Char qualified as Char
import Data.Text qualified as T
import Numeric.Data.Interval qualified as Interval
import Pythia.Prelude
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryPercentage (..),
    BatteryStatus (..),
    batteryExFromException,
    batteryExToException,
  )
import Pythia.ShellApp (CmdException (..), SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils qualified as U
import Text.Megaparsec (Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Error qualified as MPE
import Text.Read qualified as TR

-- | ACPI query for 'Battery'.
--
-- @since 0.1.0.0
batteryShellApp :: (MonadCatch m, MonadIO m, Throws AcpiException) => m Battery
batteryShellApp =
  ShellApp.runSimple shell
    `catch` \(MkCmdErr ex) -> throw (AcpiCmdErr ex)
  where
    shell =
      MkSimpleShell
        { command = "acpi",
          parser = parseBattery
        }

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1.0.0
supported :: MonadIO m => m Bool
supported = U.exeSupported "acpi"

-- | Attempts to parse the output of acpi.
--
-- ==== __Examples__
-- >>> parseBattery "Battery 0: Full, 100%"
-- Right (MkBattery {percentage = MkBatteryPercentage {unBatteryPercentage = UnsafeLRInterval 100}, status = Full})
--
-- >>> parseBattery "Battery 0: Discharging, 80%"
-- Right (MkBattery {percentage = MkBatteryPercentage {unBatteryPercentage = UnsafeLRInterval 80}, status = Discharging})
--
-- >>> parseBattery "Battery 0: Charging, 40%"
-- Right (MkBattery {percentage = MkBatteryPercentage {unBatteryPercentage = UnsafeLRInterval 40}, status = Charging})
--
-- >>> parseBattery "Battery 0: bad status, 80%"
-- Right (MkBattery {percentage = MkBatteryPercentage {unBatteryPercentage = UnsafeLRInterval 80}, status = Unknown "bad status"})
--
-- >>> parseBattery "Battery 0: Discharging, 150%"
-- Left (AcpiParseErr "Acpi.hs:1:28:\n  |\n1 | Battery 0: Discharging, 150%\n  |                            ^\nexpecting percentage\n")
--
-- @since 0.1.0.0
parseBattery :: Text -> Either AcpiException Battery
parseBattery txt = first mkErr parseResult
  where
    parseResult = MP.parse mparseBattery "Acpi.hs" txt
    mkErr err = AcpiParseErr $ MPE.errorBundlePretty err

type MParser = Parsec Void Text

mparseBattery :: MParser Battery
mparseBattery = do
  MPC.string "Battery"
  MPC.space
  MP.takeWhile1P (Just "decimal digits") Char.isDigit
  MPC.char ':'
  MPC.space
  state <- mparseState
  MPC.char ','
  MPC.space
  percentage <- mparsePercent
  pure $ MkBattery percentage state

mparseState :: MParser BatteryStatus
mparseState =
  MP.try discharging
    <|> MP.try charging
    <|> MP.try pending
    <|> MP.try full
    <|> unknown
    <?> "<Discharging|Charging|Not charging>"
  where
    discharging = MPC.string' "Discharging" $> Discharging
    charging = MPC.string' "Charging" $> Charging
    full = MPC.string' "Full" $> Full
    pending = MPC.string' "Not charging" $> Pending
    unknown = do
      s <- MP.takeWhile1P Nothing (/= ',')
      pure $ Unknown s

mparsePercent :: MParser BatteryPercentage
mparsePercent = do
  percent <- MP.takeWhile1P (Just "percentage") Char.isDigit
  percentage <- maybe empty pure (readInterval percent)
  MPC.char '%'
  pure $ MkBatteryPercentage percentage
  where
    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack

-- | Errors that can occur when running acpi.
--
-- @since 0.1.0.0
data AcpiException
  = -- | Parse error.
    --
    -- @since 0.1.0.0
    AcpiParseErr String
  | -- | Error running acpi command.
    --
    -- @since 0.1.0.0
    AcpiCmdErr String
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance Exception AcpiException where
  toException = batteryExToException
  fromException = batteryExFromException
