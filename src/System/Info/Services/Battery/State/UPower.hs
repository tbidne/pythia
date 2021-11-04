-- | This module provides functionality for retrieving battery information
-- using UPower.
module System.Info.Services.Battery.State.UPower
  ( batteryStateShellApp,
  )
where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Smart.Data.Math.BoundedNat qualified as BN
import System.Info.Data (QueryError (..))
import System.Info.Services.Battery.Types
  ( BatteryLevel,
    BatteryState (..),
    ChargeStatus (..),
  )
import System.Info.ShellApp (ShellApp (..))

-- | UPower 'ShellApp' for 'BatteryState'.
batteryStateShellApp :: ShellApp BatteryState
batteryStateShellApp =
  MkShellApp
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
        { name = "System.Info.Services.Battery.UPower.Parsing",
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
    parseNN = AP.decimal >>= maybe empty pure . BN.mkBoundedNat
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
