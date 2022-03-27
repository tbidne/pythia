module Unit.Pythia.Services.Battery.ChargeStatus.UPower
  ( tests,
  )
where

import Data.Text qualified as T
import Pythia.Services.Battery.ChargeStatus.UPower qualified as UPower
import Pythia.Services.Battery.Types (ChargeStatus (..))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pythia.Services.Battery.ChargeStatus.UPower"
    [ parseCharging,
      parseDischarging,
      parseFull
    ]

parseCharging :: TestTree
parseCharging = parseX "charging" Charging

parseDischarging :: TestTree
parseDischarging = parseX "discharging" Discharging

parseFull :: TestTree
parseFull = parseX "fully-charged" Full

parseX :: Text -> ChargeStatus -> TestTree
parseX t cs = testCase desc $ do
  p <-
    maybe
      (assertFailure "Failed to retrieve parser")
      pure
      (UPower.chargeStatusShellApp ^? #_SimpleApp % #parser)
  let result = p chargeStatus
  Right cs @=? result
  where
    desc = "Parses '" <> T.unpack t <> "'"
    chargeStatus = "state: " <> t

{-
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
-}
