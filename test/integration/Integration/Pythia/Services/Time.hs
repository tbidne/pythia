{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Pythia.Services.Time (tests) where

import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time.LocalTime (midday, utc)
import Effectful.Time.Dynamic (Time (GetMonotonicTime, GetSystemZonedTime))
import Integration.Prelude

tests :: TestTree
tests =
  testGroup
    "time"
    [ testTimeDefault,
      testTimeDest
    ]

testTimeDefault :: TestTree
testTimeDefault = testCase "default" $ do
  results <- runIntIO ["time"]
  assertSingleOutput "Sun, 31 May 2020 12:00:00 UTC" results

testTimeDest :: TestTree
testTimeDest = testCase "dest" $ do
  results <- runIntIO ["time", "-d", "america/new_york"]
  assertSingleOutput "Sun, 31 May 2020 08:00:00 EDT" results

runIntIO :: [String] -> IO [Text]
runIntIO = runIntegrationIO unIntIO

unIntIO ::
  Eff
    [ Environment,
      FileReader,
      Optparse,
      PathReader,
      Terminal,
      Time,
      TypedProcess,
      Reader (IORef Text),
      IOE
    ]
    a ->
  Eff [Reader (IORef Text), IOE] a
unIntIO =
  runTypedProcessStub
    . runTimeMock
    . runTerminalMock
    . runPathReaderMock
    . runOptparse
    . runFileReaderStub
    . runEnvironment

runTimeMock :: Eff (Time : es) a -> Eff es a
runTimeMock = interpret_ $ \case
  GetSystemZonedTime -> pure $ ZonedTime localTime utc
  GetMonotonicTime -> pure 0

localTime :: LocalTime
localTime = LocalTime (toEnum 59_000) midday
