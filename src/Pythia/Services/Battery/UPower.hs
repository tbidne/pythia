-- | This module provides functionality for retrieving battery information
-- using UPower.
--
-- @since 0.1.0.0
module Pythia.Services.Battery.UPower
  ( batteryStateShellApp,
  )
where

import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
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
parseLine ln = case AP.parseOnly parseState ln of
  Right s -> Status s
  Left _ -> case AP.parseOnly parsePercent ln of
    Right n -> Percent n
    Left _ -> None

parsePercent :: Parser BatteryLevel
parsePercent =
  AP.skipSpace
    *> AP.string "percentage:"
    *> AP.skipSpace
    *> parseNN
    <* end
  where
    parseNN = do
      num <- AP.decimal
      maybe empty pure (Interval.mkLRInterval num)
    end = AP.char '%' *> AP.skipSpace

parseState :: Parser ChargeStatus
parseState =
  AP.skipSpace
    *> AP.string "state:"
    *> AP.skipSpace
    *> (discharging <|> charging <|> full)
  where
    discharging = AP.string "discharging" $> Discharging <* rest
    charging = AP.string "charging" $> Charging <* rest
    full = AP.string "fully-charged" $> Full <* rest
    rest = AP.skipSpace *> AP.endOfInput
