module Unit.Pythia.Services.Memory.Types (tests) where

import Data.Bytes (Bytes (MkBytes))
import Pythia.Services.Memory.Types (Memory (MkMemory), SystemMemory (MkSystemMemory))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pythia.Services.Memory.Types"
    [ testDisplay
    ]

testDisplay :: TestTree
testDisplay = testCase "Display" $ do
  "50.00B / 100.00B" @=? display (mkMemory 100 50)
  "10.00G / 16.00G" @=? display (mkMemory 16_000_000_000 10_000_000_000)
  "4.24G / 16.56G" @=? display (mkMemory 16_555_000_000 4_238_000_000)

mkMemory :: Natural -> Natural -> SystemMemory
mkMemory t u = MkSystemMemory (MkMemory $ MkBytes t) (MkMemory $ MkBytes u)
