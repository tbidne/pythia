{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Pythia.Services.Time (tests) where

import Control.Monad.IO.Class (MonadIO)
import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time.LocalTime (midday, utc)
import Effects.Exception (MonadGlobalException)
import Effects.Optparse (MonadOptparse)
import Effects.System.Environment (MonadEnv)
import Effects.Time (MonadTime (getSystemZonedTime), getMonotonicTime)
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

newtype IntIO a = MkIntIO {unIntIO :: IO a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadGlobalException,
      MonadIO,
      MonadOptparse,
      MonadTerminal,
      MonadThrow
    )
    via IO

instance MonadTime IntIO where
  getSystemZonedTime = pure $ ZonedTime localTime utc
  getMonotonicTime = pure 0

localTime :: LocalTime
localTime = LocalTime (toEnum 59_000) midday

instance MonadFileReader IntIO

instance MonadPathReader IntIO where
  doesDirectoryExist _ = pure False
  getXdgDirectory _ _ = pure [osp|test_xdg|]

instance MonadTypedProcess IntIO
