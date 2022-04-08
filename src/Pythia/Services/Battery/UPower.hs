{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for retrieving battery information
-- using UPower.
--
-- @since 0.1
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

-- $setup
-- >>> import GHC.Exception (errorCallException)

-- | Errors that can occur with upower.
--
-- ==== __Examples__
--
-- >>> putStrLn $ displayException $ UPowerGeneralException $ errorCallException "oh no"
-- UPower exception: <oh no>
--
-- >>> putStrLn $ displayException $ UPowerNoPercentage "output"
-- UPower parse error. No percentage found in output: <output>
--
-- >>> putStrLn $ displayException $ UPowerNoStatus "output"
-- UPower parse error. No status found in output: <output>
--
-- >>> putStrLn $ displayException $ UPowerNoPercentageNorStatus "output"
-- UPower parse error. No percentage nor status found in output: <output>
--
-- @since 0.1
data UPowerException
  = -- | General exceptions.
    --
    -- @since 0.1
    forall e. Exception e => UPowerGeneralException e
  | -- | Did not find percentage.
    --
    -- @since 0.1
    UPowerNoPercentage Text
  | -- | Did not find status.
    --
    -- @since 0.1
    UPowerNoStatus Text
  | -- | Found neither percentage nor status.
    --
    -- @since 0.1
    UPowerNoPercentageNorStatus Text

-- | @since 0.1
makePrismLabels ''UPowerException

-- | @since 0.1
deriving stock instance Show UPowerException

-- | @since 0.1
instance PrettyPrinter UPowerException where
  pretty (UPowerGeneralException e) = "UPower exception: <" <> T.pack (displayException e) <> ">"
  pretty (UPowerNoPercentage s) =
    "UPower parse error. No percentage found in output: <" <> s <> ">"
  pretty (UPowerNoStatus s) =
    "UPower parse error. No status found in output: <" <> s <> ">"
  pretty (UPowerNoPercentageNorStatus s) =
    "UPower parse error. No percentage nor status found in output: <" <> s <> ">"

-- | @since 0.1
instance Exception UPowerException where
  displayException = T.unpack . pretty
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia

-- | UPower query for 'Battery'.
--
-- __Throws:__
--
-- * 'UPowerException': if something goes wrong (i.e. exception while running
--       the command, or we have a parse error).
--
-- @since 0.1
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
-- @since 0.1
supported :: MonadIO m => m Bool
supported = U.exeSupported "upower"

-- | Attempts to parse the output of UPower.
--
-- ==== __Examples__
--
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
-- @since 0.1
parseBattery :: Text -> Either UPowerException Battery
parseBattery txt = case foldMap parseLine ts of
  None -> Left $ UPowerNoPercentageNorStatus txt
  Percent _ -> Left $ UPowerNoStatus txt
  Status _ -> Left $ UPowerNoPercentage txt
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
