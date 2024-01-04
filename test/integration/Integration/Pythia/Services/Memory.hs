{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Pythia.Services.Memory (tests) where

import Control.Monad.IO.Class (MonadIO)
import Data.List qualified as L
import Effects.Exception (MonadGlobalException)
import Effects.FileSystem.PathReader (MonadPathReader (findExecutable))
import Effects.Optparse (MonadOptparse)
import Effects.Process.Typed
  ( ExitCode (ExitSuccess),
    MonadTypedProcess (readProcess),
  )
import Effects.System.Environment (MonadEnv)
import Integration.Prelude

tests :: TestTree
tests =
  testGroup
    "memory"
    [ testMemoryDefault,
      testMemoryTotal,
      testMemoryUsed,
      testMemoryFree
    ]

testMemoryDefault :: TestTree
testMemoryDefault = testCase "default" $ do
  results <- runIntIO ["memory", "-a", "free"]
  assertSingleOutput "10.95G / 16.56G" results

  resultsp <- runIntIO ["memory", "-a", "free", "-p"]
  assertSingleOutput "66 / 100%" resultsp

testMemoryTotal :: TestTree
testMemoryTotal = testCase "total" $ do
  results <- runIntIO ["memory", "-a", "free", "-f", "total"]
  assertSingleOutput "16.56G" results

  resultsp <- runIntIO ["memory", "-a", "free", "-f", "total", "-p"]
  assertSingleOutput "100%" resultsp

testMemoryUsed :: TestTree
testMemoryUsed = testCase "used" $ do
  results <- runIntIO ["memory", "-a", "free", "-f", "used"]
  assertSingleOutput "10.95G" results

  resultsp <- runIntIO ["memory", "-a", "free", "-f", "used", "-p"]
  assertSingleOutput "66%" resultsp

testMemoryFree :: TestTree
testMemoryFree = testCase "free" $ do
  results <- runIntIO ["memory", "-a", "free", "-f", "free"]
  assertSingleOutput "5.61G" results

  resultsp <- runIntIO ["memory", "-a", "free", "-f", "free", "-p"]
  assertSingleOutput "34%" resultsp

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
      MonadTime,
      MonadThrow
    )
    via IO

instance MonadFileReader IntIO

instance MonadPathReader IntIO where
  findExecutable p
    | p == [osp|free|] = pure $ Just [osp|exe|]
    | otherwise = pure Nothing

instance MonadTypedProcess IntIO where
  readProcess pc = case cmd of
    "free --bytes" ->
      let output =
            L.unlines
              [ "               total        used        free      shared  buff/cache   available",
                "Mem:     16564326400  8188026880  1102393344  2762588160  7273906176  5274624000",
                "Swap:    34359734272    58720256 34301014016"
              ]
       in pure (ExitSuccess, fromString output, "")
    bad -> error $ "Unexpected command: " <> bad
    where
      cmd = processConfigToCmd pc
