module Integration.Pythia.Services.GlobalIp (tests) where

import Data.List qualified as L
import Effectful.Process.Typed.Dynamic
  ( ExitCode (ExitSuccess),
    TypedProcess (ReadProcess),
  )
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
  runTypedProcessMock
    . runTime
    . runTerminalMock
    . runPathReaderMock
    . runOptparse
    . runFileReaderStub
    . runEnvironment

runTypedProcessMock :: Eff (TypedProcess : es) a -> Eff es a
runTypedProcessMock = interpret_ $ \case
  ReadProcess pc ->
    if
      | "Shell command: curl" `L.isPrefixOf` cmd -> pure (ExitSuccess, "192.168.1.0", "")
      | "Shell command: dig" `L.isPrefixOf` cmd -> pure (ExitSuccess, "192.168.0.1", "")
      | otherwise -> error $ "Unexpected command: " <> cmd
    where
      cmd = processConfigToCmd pc
  _ -> error "runTypedProcessMock: unimplemented"
