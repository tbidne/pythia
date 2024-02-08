{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Pythia.Services.GlobalIp (tests) where

import Data.List qualified as L
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
    "global-ip"
    [ testGlobalIpDefault
    ]

testGlobalIpDefault :: TestTree
testGlobalIpDefault = testCase "default" $ do
  curlResults <- runIntIO ["global-ip", "-a", "curl"]
  assertSingleOutput "192.168.1.0" curlResults

  digResults <- runIntIO ["global-ip", "-a", "dig"]
  assertSingleOutput "192.168.0.1" digResults

runIntIO :: [String] -> IO [Text]
runIntIO = runIntegrationIO unIntIO

newtype IntIO a = MkIntIO {unIntIO :: ReaderT (IORef Text) IO a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadIO,
      MonadOptparse,
      MonadTime,
      MonadThrow
    )
    via ReaderT (IORef Text) IO
  deriving (MonadTerminal) via BaseIO

instance MonadFileReader IntIO

instance MonadPathReader IntIO where
  doesDirectoryExist _ = pure False
  getXdgDirectory _ _ = pure [osp|test_xdg|]

  findExecutable p
    | p == [osp|curl|] = pure $ Just [osp|exe|]
    | p == [osp|dig|] = pure $ Just [osp|exe|]
    | otherwise = pure Nothing

instance MonadTypedProcess IntIO where
  readProcess pc =
    if
      | "Shell command: curl" `L.isPrefixOf` cmd -> pure (ExitSuccess, "192.168.1.0", "")
      | "Shell command: dig" `L.isPrefixOf` cmd -> pure (ExitSuccess, "192.168.0.1", "")
      | otherwise -> error $ "Unexpected command: " <> cmd
    where
      cmd = processConfigToCmd pc
