{-# LANGUAGE DeriveAnyClass #-}

-- | This module provides functionality for retrieving battery information
-- using ACPI.
--
-- @since 0.1.0.0
module Pythia.Services.Battery.Acpi
  ( -- * Query
    batteryShellApp,
    supported,

    -- * Misc
    AcpiError (..),
    parseBattery,
  )
where

import Data.Char qualified as Char
import Data.Text qualified as T
import Numeric.Data.Interval qualified as Interval
import Pythia.Prelude
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryLevel,
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

-- | ACPI 'ShellApp' for 'Battery'.
--
-- @since 0.1.0.0
batteryShellApp :: IO Battery
batteryShellApp =
  ShellApp.runSimple $
    MkSimpleShell
      { command = "acpi",
        parser = parseBattery
      }

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1.0.0
supported :: IO Bool
supported = U.exeSupported "acpi"

-- | Attempts to parse the output of acpi.
--
-- @since 0.1.0.0
parseBattery :: Text -> Either AcpiError Battery
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
  level <- mparsePercent
  pure $ MkBattery level state

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

mparsePercent :: MParser BatteryLevel
mparsePercent = do
  percent <- MP.takeWhile1P (Just "percentage") Char.isDigit
  level <- maybe empty pure (readInterval percent)
  MPC.char '%'
  pure level
  where
    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack

-- | Errors that can occur when reading sysfs.
--
-- @since 0.1.0.0
newtype AcpiError
  = -- | Parse error.
    --
    -- @since 0.1.0.0
    AcpiParseErr String
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass (Exception)
