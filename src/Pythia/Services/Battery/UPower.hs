{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for retrieving battery information
-- using UPower.
--
-- @since 0.1.0.0
module Pythia.Services.Battery.UPower
  ( -- * Query
    batteryShellApp,
    supported,

    -- * Misc
    UPowerException (..),
    parseBattery,
  )
where

import Data.Char qualified as Char
import Data.Text qualified as T
import Numeric.Data.Interval qualified as Interval
import Pythia.Class.Printer (PrettyPrinter (..))
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Prelude
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryPercentage (..),
    BatteryStatus (..),
  )
import Pythia.ShellApp (SimpleShell (..))
import Pythia.ShellApp qualified as ShellApp
import Pythia.Utils qualified as U
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Read qualified as TR

-- | Errors that can occur with upower.
--
-- @since 0.1.0.0
data UPowerException
  = -- | General exceptions.
    --
    -- @since 0.1.0.0
    forall e. Exception e => UPowerGeneralException e
  | -- | Did not find percentage.
    --
    -- @since 0.1.0.0
    UPowerNoPercentage String
  | -- | Did not find status.
    --
    -- @since 0.1.0.0
    UPowerNoStatus String
  | -- | Found neither percentage nor status.
    --
    -- @since 0.1.0.0
    UPowerNoPercentageNorStatus String

-- | @since 0.1.0.0
makePrismLabels ''UPowerException

-- | @since 0.1.0.0
deriving stock instance Show UPowerException

-- | @since 0.1.0.0
instance PrettyPrinter UPowerException where
  pretty (UPowerGeneralException e) = "UPower exception: <" <> displayException e <> ">"
  pretty (UPowerNoPercentage s) =
    "UPower parse error. No percentage found in output: <" <> s <> ">"
  pretty (UPowerNoStatus s) =
    "UPower parse error. No status found in output: <" <> s <> ">"
  pretty (UPowerNoPercentageNorStatus s) =
    "UPower parse error. No percentage nor status found in output: <" <> s <> ">"

-- | @since 0.1.0.0
-- | @since 0.1.0.0
instance Exception UPowerException where
  displayException = pretty
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia

-- | UPower query for 'Battery'. Throws exceptions if the command fails or
-- or we have a parse error.
--
-- @since 0.1.0.0
batteryShellApp :: (MonadCatch m, MonadIO m) => m Battery
batteryShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "upower -i `upower -e | grep 'BAT'`",
          parser = parseBattery,
          liftShellEx = UPowerGeneralException
        }

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1.0.0
supported :: MonadIO m => m Bool
supported = U.exeSupported "upower"

-- | Attempts to parse the output of UPower.
--
-- ==== __Examples__
-- >>> parseBattery "state: fully-charged\npercentage: 100%"
-- Right (MkBattery {percentage = MkBatteryPercentage {unBatteryPercentage = UnsafeLRInterval 100}, status = Full})
--
-- >>> parseBattery "state: discharging\npercentage: 70%"
-- Right (MkBattery {percentage = MkBatteryPercentage {unBatteryPercentage = UnsafeLRInterval 70}, status = Discharging})
--
-- >>> parseBattery "state: charging\npercentage: 40%"
-- Right (MkBattery {percentage = MkBatteryPercentage {unBatteryPercentage = UnsafeLRInterval 40}, status = Charging})
--
-- >>> parseBattery "state: bad\npercentage: 40%"
-- Right (MkBattery {percentage = MkBatteryPercentage {unBatteryPercentage = UnsafeLRInterval 40}, status = Unknown "bad"})
--
-- >>> parseBattery "state: pending-charge\npercentage: 40%"
-- Right (MkBattery {percentage = MkBatteryPercentage {unBatteryPercentage = UnsafeLRInterval 40}, status = Pending})
--
-- >>> parseBattery "state: fully-charged"
-- Left (UPowerNoPercentage "state: fully-charged")
--
-- >>> parseBattery "percentage: 80%"
-- Left (UPowerNoStatus "percentage: 80%")
--
-- >>> parseBattery "nothing"
-- Left (UPowerNoPercentageNorStatus "nothing")
--
-- @since 0.1.0.0
parseBattery :: Text -> Either UPowerException Battery
parseBattery txt = case foldMap parseLine ts of
  None -> Left $ UPowerNoPercentageNorStatus $ T.unpack txt
  Percent _ -> Left $ UPowerNoStatus $ T.unpack txt
  Status _ -> Left $ UPowerNoPercentage $ T.unpack txt
  Both bs -> Right bs
  where
    ts = T.lines txt

data BatteryResult
  = None
  | Percent BatteryPercentage
  | Status BatteryStatus
  | Both Battery
  deriving (Show)

instance Semigroup BatteryResult where
  Both s <> _ = Both s
  _ <> Both s = Both s
  None <> r = r
  l <> None = l
  Percent n <> Status s = Both $ MkBattery n s
  Status s <> Percent n = Both $ MkBattery n s
  l <> _ = l

instance Monoid BatteryResult where
  mempty = None

parseLine :: Text -> BatteryResult
parseLine ln = case MP.parse parseStatus "Pythia.Services.battery.UPower" ln of
  Right s -> Status s
  Left _ -> case MP.parse parsePercent "Pythia.Services.battery.UPower" ln of
    Right n -> Percent n
    Left _ -> None

type MParser = Parsec Void Text

parsePercent :: MParser BatteryPercentage
parsePercent = do
  MPC.space
  MPC.string "percentage:"
  MPC.space1
  nn <- parseNN
  MPC.char '%'
  MPC.space
  pure $ MkBatteryPercentage nn
  where
    parseNN = do
      num <- MP.takeWhile1P Nothing Char.isDigit
      maybe empty pure (readInterval num)

    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack

parseStatus :: MParser BatteryStatus
parseStatus = do
  MPC.space
  MPC.string "state:"
  MPC.space1
  MP.try discharging
    <|> MP.try charging
    <|> MP.try full
    <|> MP.try pending
    <|> unknown
  where
    discharging = MPC.string "discharging" $> Discharging
    charging = MPC.string "charging" $> Charging <* rest
    full = MPC.string "fully-charged" $> Full <* rest
    pending = MPC.string "pending-charge" $> Pending <* rest
    unknown = do
      s <- MP.takeWhile1P Nothing (/= '\n')
      MP.eof
      pure $ Unknown s
    rest = MPC.space *> MP.eof
