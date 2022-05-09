{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for retrieving battery information
-- using ACPI.
--
-- @since 0.1
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
import Data.Set qualified as Set
import Data.Text qualified as T
import Numeric.Data.Interval qualified as Interval
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Data.Percentage (Percentage (..))
import Pythia.Prelude
import Pythia.Services.Battery.Types (Battery (..), BatteryStatus (..))
import Pythia.ShellApp (SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils (Pretty (..))
import Pythia.Utils qualified as U
import Text.Megaparsec (ErrorFancy (..), Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Error qualified as MPE
import Text.Read qualified as TR

-- $setup
-- >>> import GHC.Exception (errorCallException)

-- | Errors that can occur with acpi.
--
-- ==== __Examples__
--
-- >>> putStrLn $ displayException $ AcpiGeneralException $ errorCallException "oh no"
-- Acpi exception: <oh no>
--
-- >>> putStrLn $ displayException $ AcpiParseException "parse error"
-- Acpi parse exception: <parse error>
--
-- @since 0.1
data AcpiException
  = -- | For general exceptions.
    --
    -- @since 0.1
    forall e. Exception e => AcpiGeneralException e
  | -- | Parse errors.
    --
    -- @since 0.1
    AcpiParseException Text

-- | @since 0.1
makePrismLabels ''AcpiException

-- | @since 0.1
deriving stock instance Show AcpiException

-- | @since 0.1
instance Pretty AcpiException where
  pretty (AcpiGeneralException e) =
    pretty @Text "Acpi exception: <"
      <> pretty (displayException e)
      <> pretty @Text ">"
  pretty (AcpiParseException s) =
    pretty @Text "Acpi parse exception: <"
      <> pretty s
      <> pretty @Text ">"

-- | @since 0.1
instance Exception AcpiException where
  displayException = T.unpack . U.prettyToText
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia

-- | ACPI query for 'Battery'.
--
-- __Throws:__
--
-- * 'AcpiException': if something goes wrong (i.e. exception while running
--       the command, or we have a parse error).
--
-- @since 0.1
batteryShellApp :: MonadUnliftIO m => m Battery
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
-- @since 0.1
supported :: MonadIO m => m Bool
supported = U.exeSupported "acpi"

-- | Attempts to parse the output of acpi.
--
-- ==== __Examples__
--
-- >>> parseBattery "Battery 0: Full, 100%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval {unLRInterval = 100}}, status = Full})
--
-- >>> parseBattery "Battery 0: Discharging, 80%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval {unLRInterval = 80}}, status = Discharging})
--
-- >>> parseBattery "Battery 0: Charging, 40%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval {unLRInterval = 40}}, status = Charging})
--
-- >>> parseBattery "Battery 0: bad status, 80%"
-- Left (AcpiParseException "Acpi.hs:1:12:\n  |\n1 | Battery 0: bad status, 80%\n  |            ^\nUnknown status\n")
--
-- >>> parseBattery "Battery 0: Discharging, 150%"
-- Left (AcpiParseException "Acpi.hs:1:28:\n  |\n1 | Battery 0: Discharging, 150%\n  |                            ^\nexpecting percentage\n")
--
-- @since 0.1
parseBattery :: Text -> Either AcpiException Battery
parseBattery txt = first mkErr parseResult
  where
    parseResult = MP.parse mparseBattery "Acpi.hs" txt
    mkErr err = AcpiParseException $ T.pack $ MPE.errorBundlePretty err

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
    <|> MP.fancyFailure (Set.fromList [ErrorFail "Unknown status"])
    <?> "<Discharging|Charging|Not charging|Full>"
  where
    discharging = MPC.string' "Discharging" $> Discharging
    charging = MPC.string' "Charging" $> Charging
    full = MPC.string' "Full" $> Full
    pending = MPC.string' "Not charging" $> Pending

mparsePercent :: MParser Percentage
mparsePercent = do
  percent <- MP.takeWhile1P (Just "percentage") Char.isDigit
  percentage <- maybe empty pure (readInterval percent)
  MPC.char '%'
  pure $ MkPercentage percentage
  where
    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack
