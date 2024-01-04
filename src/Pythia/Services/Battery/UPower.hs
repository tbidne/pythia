{-# LANGUAGE QuasiQuotes #-}

-- | This module provides functionality for retrieving battery information
-- using UPower.
--
-- @since 0.1
module Pythia.Services.Battery.UPower
  ( -- * Query
    batteryShellApp,
    supported,

    -- * Misc
    UPowerParseError (..),
    parseBattery,
  )
where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as T
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
import Text.Megaparsec (ErrorFancy (ErrorFail), Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Read qualified as TR

-- $setup
-- >>> import Control.Exception (displayException)
-- >>> import Pythia.Prelude

-- | Errors that can occur with upower.
--
-- ==== __Examples__
--
-- >>> displayException $ UPowerParseErrorPercentage "output"
-- "No percentage found in upower output: output"
--
-- >>> displayException $ UPowerParseErrorStatus "output"
-- "No status found in upower output: output"
--
-- @since 0.1
type UPowerParseError :: Type
data UPowerParseError
  = -- | Did not find percentage.
    --
    -- @since 0.1
    UPowerParseErrorPercentage Text
  | -- | Did not find status.
    --
    -- @since 0.1
    UPowerParseErrorStatus Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception UPowerParseError where
  displayException (UPowerParseErrorPercentage s) =
    "No percentage found in upower output: " <> T.unpack s
  displayException (UPowerParseErrorStatus s) =
    "No status found in upower output: " <> T.unpack s

-- | UPower query for 'Battery'.
--
-- @since 0.1
batteryShellApp ::
  ( MonadPathReader m,
    MonadThrow m,
    MonadTypedProcess m
  ) =>
  m Battery
batteryShellApp = ShellApp.runSimple shell
  where
    shell =
      MkSimpleShell
        { command = "upower -i `upower -e | grep 'BAT'`",
          isSupported = supported,
          parser = parseBattery
        }
{-# INLINEABLE batteryShellApp #-}

-- | Returns a boolean determining if this program is supported on the
-- current system.
--
-- @since 0.1
supported :: (MonadPathReader m) => m Bool
supported = U.exeSupported [osp|upower|]
{-# INLINEABLE supported #-}

-- | Attempts to parse the output of UPower.
--
-- ==== __Examples__
--
-- >>> parseBattery "state: fully-charged\npercentage: 100%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval 100}, status = Full})
--
-- >>> parseBattery "state: discharging\npercentage: 70%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval 70}, status = Discharging})
--
-- >>> parseBattery "state: charging\npercentage: 40%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval 40}, status = Charging})
--
-- >>> parseBattery "state: pending-charge\npercentage: 40%"
-- Right (MkBattery {percentage = MkPercentage {unPercentage = UnsafeLRInterval 40}, status = Pending})
--
-- >>> parseBattery "state: bad\npercentage: 40%"
-- Left (UPowerParseErrorStatus "state: bad\npercentage: 40%")
--
-- >>> parseBattery "state: fully-charged"
-- Left (UPowerParseErrorPercentage "state: fully-charged")
--
-- >>> parseBattery "percentage: 80%"
-- Left (UPowerParseErrorStatus "percentage: 80%")
--
-- >>> parseBattery "nothing"
-- Left (UPowerParseErrorStatus "nothing")
--
-- @since 0.1
parseBattery :: Text -> Either UPowerParseError Battery
parseBattery txt = case foldMap parseLine ts of
  None -> Left $ UPowerParseErrorStatus txt
  Percent _ -> Left $ UPowerParseErrorStatus txt
  Status _ -> Left $ UPowerParseErrorPercentage txt
  Both bs -> Right bs
  where
    ts = T.lines txt
{-# INLINEABLE parseBattery #-}

type BatteryResult :: Type
data BatteryResult
  = None
  | Percent Percentage
  | Status BatteryStatus
  | Both Battery
  deriving stock (Show)

instance Semigroup BatteryResult where
  Both s <> _ = Both s
  _ <> Both s = Both s
  None <> r = r
  l <> None = l
  Percent n <> Status s = Both $ MkBattery n s
  Status s <> Percent n = Both $ MkBattery n s
  l <> _ = l
  {-# INLINEABLE (<>) #-}

instance Monoid BatteryResult where
  mempty = None
  {-# INLINEABLE mempty #-}

parseLine :: Text -> BatteryResult
parseLine ln = case MP.parse parseStatus "Pythia.Services.battery.UPower" ln of
  Right s -> Status s
  Left _ -> case MP.parse parsePercent "Pythia.Services.battery.UPower" ln of
    Right n -> Percent n
    Left _ -> None
{-# INLINEABLE parseLine #-}

type MParser :: Type -> Type
type MParser = Parsec Void Text

parsePercent :: MParser Percentage
parsePercent = do
  MPC.space
  MPC.string "percentage:"
  MPC.space1
  nn <- parseNN
  MPC.char '%'
  MPC.space
  pure nn
  where
    parseNN = do
      num <- MP.takeWhile1P Nothing Char.isDigit
      maybe empty pure (readPercentage num)

    readPercentage = Percentage.mkPercentage <=< TR.readMaybe . T.unpack
{-# INLINEABLE parsePercent #-}

parseStatus :: MParser BatteryStatus
parseStatus = do
  MPC.space
  MPC.string "state:"
  MPC.space1
  MP.try discharging
    <|> MP.try charging
    <|> MP.try full
    <|> MP.try pending
    <|> MP.fancyFailure (Set.fromList [ErrorFail "Unknown status"])
  where
    discharging = MPC.string' "discharging" $> Discharging
    charging = MPC.string' "charging" $> Charging <* rest
    full = MPC.string' "fully-charged" $> Full <* rest
    pending = MPC.string' "pending-charge" $> Pending <* rest
    rest = MPC.space *> MP.eof
{-# INLINEABLE parseStatus #-}
