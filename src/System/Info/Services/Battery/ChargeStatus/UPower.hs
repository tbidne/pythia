-- | This module provides functionality for retrieving battery information
-- using UPower.
module System.Info.Services.Battery.ChargeStatus.UPower
  ( chargeStatusShellApp,
  )
where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import System.Info.Data (QueryError (..))
import System.Info.Services.Battery.Types (ChargeStatus (..))
import System.Info.ShellApp (ShellApp (..))
import System.Info.Utils qualified as U

-- | UPower 'ShellApp' for 'ChargeStatus'.
chargeStatusShellApp :: ShellApp ChargeStatus
chargeStatusShellApp =
  MkShellApp
    { command = "upower -i `upower -e | grep 'BAT'`",
      parser = parseChargeStatus
    }

-- | Attempts to parse the given text into a 'ChargeStatus'.
parseChargeStatus :: Text -> Either QueryError ChargeStatus
parseChargeStatus txt = case U.foldAlt parseLine ts of
  Nothing -> Left $ mkErr $ "Did not find charging status in: " <> txt
  Just cs -> Right cs
  where
    ts = T.lines txt
    mkErr err =
      MkQueryError
        { name = "System.Info.Services.Battery.UPower.Parsing",
          short = "Parse error",
          long = err
        }

parseLine :: Text -> Maybe ChargeStatus
parseLine ln = case AP.parseOnly parseState ln of
  Right s -> Just s
  Left _ -> Nothing

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
