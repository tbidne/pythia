{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryPercentage (..),
    BatteryStatus (..),
  )
import Pythia.ShellApp (SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils qualified as U
import Text.Megaparsec (Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Error qualified as MPE
import Text.Read qualified as TR

-- | Errors that can occur with acpi.
--
-- @since 0.1.0.0
data AcpiException
  = -- | For general exceptions.
    --
    -- @since 0.1.0.0
    forall e. Exception e => AcpiGeneralException e
  | -- | Parse errors.
    --
    -- @since 0.1.0.0
    AcpiParseException String

-- | @since 0.1.0.0
makePrismLabels ''AcpiException

-- | @since 0.1.0.0
deriving stock instance Show AcpiException

-- | @since 0.1.0.0
instance PrettyPrinter AcpiException where
  pretty (AcpiGeneralException e) = "Acpi exception: <" <> displayException e <> ">"
  pretty (AcpiParseException s) = "Acpi parse error: <" <> s <> ">"

-- | @since 0.1.0.0
instance Exception AcpiException where
  displayException = pretty
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia

-- | ACPI query for 'Battery'. Throws exceptions if the command fails or
-- or we have a parse error.
--
-- @since 0.1.0.0
batteryShellApp :: (MonadCatch m, MonadIO m) => m Battery
batteryShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "acpi",
          parser = parseBattery,
          liftShellEx = AcpiGeneralException
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
-- Left (AcpiParseException "Acpi.hs:1:28:\n  |\n1 | Battery 0: Discharging, 150%\n  |                            ^\nexpecting percentage\n")
--
-- @since 0.1.0.0
parseBattery :: Text -> Either AcpiException Battery
parseBattery txt = first mkErr parseResult
  where
    parseResult = MP.parse mparseBattery "Acpi.hs" txt
    mkErr err = AcpiParseException $ MPE.errorBundlePretty err

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
