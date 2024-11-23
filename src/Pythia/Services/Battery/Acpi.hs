{-# LANGUAGE QuasiQuotes #-}

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
    parseBattery,
  )
where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as T
import Pythia.Control.Exception (fromPythiaException, toPythiaException)
import Pythia.Data.Percentage (Percentage)
import Pythia.Data.Percentage qualified as Percentage
import Pythia.Internal.ShellApp
  ( SimpleShell
      ( MkSimpleShell,
        command,
        isSupported,
        parser
      ),
  )
import Pythia.Internal.ShellApp qualified as ShellApp
import Pythia.Prelude
import Pythia.Services.Battery.Types
  ( Battery (MkBattery),
    BatteryStatus
      ( Charging,
        Discharging,
        Full,
        Pending
      ),
  )
import Pythia.Utils qualified as U
import Text.Megaparsec (ErrorFancy (ErrorFail), Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Error qualified as MPE
import Text.Read qualified as TR

-- $setup
-- >>> import Control.Exception (displayException)
-- >>> import Pythia.Prelude

-- | Error parsing acpi output.
--
-- ==== __Examples__
--
-- >>> displayException $ MkAcpiParseError "parse error"
-- "Acpi parse error: parse error"
--
-- @since 0.1
type AcpiParseError :: Type
newtype AcpiParseError = MkAcpiParseError Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception AcpiParseError where
  displayException (MkAcpiParseError e) =
    ("Acpi parse error: " <>)
      . T.unpack
      $ e

  toException = toPythiaException
  fromException = fromPythiaException

-- | ACPI query for 'Battery'.
--
-- @since 0.1
batteryShellApp ::
  ( HasCallStack,
    PathReader :> es,
    TypedProcess :> es
  ) =>
  Eff es Battery
batteryShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "acpi",
          isSupported = supported,
          parser = parseBattery
        }

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1
supported :: (HasCallStack, PathReader :> es) => Eff es Bool
supported = U.exeSupported [osp|acpi|]

-- | Attempts to parse the output of acpi.
--
-- ==== __Examples__
--
-- >>> parseBattery "Battery 0: Full, 100%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval 100}, status = Full})
--
-- >>> parseBattery "Battery 0: Discharging, 80%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval 80}, status = Discharging})
--
-- >>> parseBattery "Battery 0: Charging, 40%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval 40}, status = Charging})
--
-- >>> parseBattery "Battery 0: bad status, 80%"
-- Left (MkAcpiParseError "Acpi.hs:1:12:\n  |\n1 | Battery 0: bad status, 80%\n  |            ^\nUnknown status\n")
--
-- >>> parseBattery "Battery 0: Discharging, 150%"
-- Left (MkAcpiParseError "Acpi.hs:1:28:\n  |\n1 | Battery 0: Discharging, 150%\n  |                            ^\nexpecting percentage\n")
--
-- @since 0.1
parseBattery :: Text -> Either AcpiParseError Battery
parseBattery txt = U.foldMap1 parseLine "<empty input>" tlines
  where
    tlines = T.lines txt

parseLine :: Text -> Either AcpiParseError Battery
parseLine = first mkErr . MP.parse mparseBattery "Acpi.hs"
  where
    mkErr = MkAcpiParseError . T.pack . MPE.errorBundlePretty

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
  percentage <- maybe empty pure (readPercentage percent)
  MPC.char '%'
  pure percentage
  where
    readPercentage = Percentage.mkPercentage <=< TR.readMaybe . T.unpack
