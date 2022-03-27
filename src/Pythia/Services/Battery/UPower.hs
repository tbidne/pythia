-- | This module provides functionality for retrieving battery information
-- using UPower.
--
-- @since 0.1.0.0
module Pythia.Services.Battery.UPower
  ( batteryStateShellApp,
  )
where

import Data.Char qualified as Char
import Data.Text qualified as T
import Numeric.Data.Interval qualified as Interval
import Pythia.Data (QueryError (..))
import Pythia.Prelude
import Pythia.Services.Battery.Types
  ( BatteryLevel,
    BatteryState (..),
    ChargeStatus (..),
  )
import Pythia.ShellApp (ShellApp (..), SimpleShell (..))
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Read qualified as TR

-- | UPower 'ShellApp' for 'BatteryState'.
--
-- @since 0.1.0.0
batteryStateShellApp :: ShellApp BatteryState
batteryStateShellApp =
  SimpleApp $
    MkSimpleShell
      { command = "upower -i `upower -e | grep 'BAT'`",
        parser = parseBatteryState
      }

parseBatteryState :: Text -> Either QueryError BatteryState
parseBatteryState txt = case foldMap parseLine ts of
  None -> Left $ mkErr $ "Did not find percent or status in: " <> txt
  Percent _ -> Left $ mkErr $ "Did not find status in: " <> txt
  Status _ -> Left $ mkErr $ "Did not find percent in:" <> txt
  Both bs -> Right bs
  where
    ts = T.lines txt
    mkErr err =
      MkQueryError
        { name = "Pythia.Services.Battery.UPower.Parsing",
          short = "Parse error",
          long = err
        }

data BatteryResult
  = None
  | Percent BatteryLevel
  | Status ChargeStatus
  | Both BatteryState
  deriving (Show)

instance Semigroup BatteryResult where
  Both s <> _ = Both s
  _ <> Both s = Both s
  None <> r = r
  l <> None = l
  Percent n <> Status s = Both $ MkBatteryState n s
  Status s <> Percent n = Both $ MkBatteryState n s
  l <> _ = l

instance Monoid BatteryResult where
  mempty = None

parseLine :: Text -> BatteryResult
parseLine ln = case MP.parse parseStatus "" ln of
  Right s -> Status s
  Left _ -> case MP.parse parsePercent "" ln of
    Right n -> Percent n
    Left _ -> None

parsePercent :: Parsec Text Text BatteryLevel
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
      maybe empty pure (readInterval num)

    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack

parseStatus :: Parsec Text Text ChargeStatus
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
