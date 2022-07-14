{-# LANGUAGE TemplateHaskell #-}

-- | This module provides functionality for retrieving battery information
-- using ACPI.
--
-- @since 0.1
module Pythia.Services.Battery.Acpi
  ( -- * Query
    batteryShellApp,
    supported,

    -- * Misc
    AcpiParseError (..),
    _MkAcpiParseError,
    parseBattery,
  )
where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as T
import Numeric.Data.Interval qualified as Interval
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Data.Percentage (Percentage (..))
import Pythia.Internal.ShellApp (SimpleShell (..))
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.Battery.Types (Battery (..), BatteryStatus (..))
import Pythia.Utils (Pretty (..))
import Pythia.Utils qualified as U
import Text.Megaparsec (ErrorFancy (..), Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Error qualified as MPE
import Text.Read qualified as TR

-- | Error parsing acpi output.
--
-- ==== __Examples__
--
-- >>> displayException $ MkAcpiParseError "parse error"
-- "Acpi parse error: parse error"
--
-- @since 0.1
type AcpiParseError :: Type
newtype AcpiParseError = MkAcpiParseError
  { -- | @since 0.1
    unAcpiParseError :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makePrisms ''AcpiParseError

-- | @since 0.1
instance Pretty AcpiParseError where
  pretty (MkAcpiParseError s) =
    pretty @Text "Acpi parse error: " <> pretty s
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Exception AcpiParseError where
  displayException = T.unpack . U.prettyToText
  {-# INLINEABLE displayException #-}
  toException = toExceptionViaPythia
  {-# INLINEABLE toException #-}
  fromException = fromExceptionViaPythia
  {-# INLINEABLE fromException #-}

-- | ACPI query for 'Battery'.
--
-- __Throws:__
--
-- * 'AcpiException': if something goes wrong (i.e. exception while running
--       the command, or we have a parse error).
--
-- @since 0.1
batteryShellApp :: IO Battery
batteryShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "acpi",
          parser = parseBattery
        }
{-# INLINEABLE batteryShellApp #-}

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1
supported :: IO Bool
supported = U.exeSupported "acpi"
{-# INLINEABLE supported #-}

-- | Attempts to parse the output of acpi.
--
-- ==== __Examples__
--
-- >>> parseBattery "Battery 0: Full, 100%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval {unLRInterval = 100}}, status = BatteryStatusFull})
--
-- >>> parseBattery "Battery 0: Discharging, 80%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval {unLRInterval = 80}}, status = BatteryStatusDischarging})
--
-- >>> parseBattery "Battery 0: Charging, 40%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval {unLRInterval = 40}}, status = BatteryStatusCharging})
--
-- >>> parseBattery "Battery 0: bad status, 80%"
-- Left (MkAcpiParseError {unAcpiParseError = "Acpi.hs:1:12:\n  |\n1 | Battery 0: bad status, 80%\n  |            ^\nUnknown status\n"})
--
-- >>> parseBattery "Battery 0: Discharging, 150%"
-- Left (MkAcpiParseError {unAcpiParseError = "Acpi.hs:1:28:\n  |\n1 | Battery 0: Discharging, 150%\n  |                            ^\nexpecting percentage\n"})
--
-- @since 0.1
parseBattery :: Text -> Either AcpiParseError Battery
parseBattery txt = first mkErr parseResult
  where
    parseResult = MP.parse mparseBattery "Acpi.hs" txt
    mkErr err = MkAcpiParseError $ T.pack $ MPE.errorBundlePretty err
{-# INLINEABLE parseBattery #-}

type MParser :: Type -> Type
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
{-# INLINEABLE mparseBattery #-}

mparseState :: MParser BatteryStatus
mparseState =
  MP.try discharging
    <|> MP.try charging
    <|> MP.try pending
    <|> MP.try full
    <|> MP.fancyFailure (Set.fromList [ErrorFail "Unknown status"])
    <?> "<Discharging|Charging|Not charging|Full>"
  where
    discharging = MPC.string' "Discharging" $> BatteryStatusDischarging
    charging = MPC.string' "Charging" $> BatteryStatusCharging
    full = MPC.string' "Full" $> BatteryStatusFull
    pending = MPC.string' "Not charging" $> BatteryStatusPending
{-# INLINEABLE mparseState #-}

mparsePercent :: MParser Percentage
mparsePercent = do
  percent <- MP.takeWhile1P (Just "percentage") Char.isDigit
  percentage <- maybe empty pure (readInterval percent)
  MPC.char '%'
  pure $ MkPercentage percentage
  where
    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack
{-# INLINEABLE mparsePercent #-}
