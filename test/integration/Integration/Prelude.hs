{-# LANGUAGE QuasiQuotes #-}

module Integration.Prelude
  ( module X,

    -- * Handlers
    runIntegrationIO,
    runTerminalMock,
    runPathReaderMock,
    runTypedProcessStub,
    runFileReaderStub,

    -- * Assertions
    assertOutput,
    assertSingleOutput,

    -- * Misc
    processConfigToCmd,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Data.Either as X (isLeft)
import Data.IORef as X (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Effectful.Dispatch.Dynamic as X (interpret_)
import Effectful.Environment.Static as X (Environment, runEnvironment, withArgs)
import Effectful.FileSystem.PathReader.Dynamic as X
  ( PathReader
      ( DoesDirectoryExist,
        DoesFileExist,
        FindExecutable,
        GetXdgDirectory
      ),
  )
import Effectful.Optparse.Static as X (Optparse, runOptparse)
import Effectful.Reader.Static as X (Reader, ask, runReader)
import Pythia.Prelude as X
import Pythia.Runner qualified as Runner
import System.Process.Typed (ProcessConfig)
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))

processConfigToCmd :: ProcessConfig i o e -> String
processConfigToCmd = T.unpack . T.strip . T.pack . show

runIntegrationIO ::
  forall es.
  ( Environment :> es,
    FileReader :> es,
    PathReader :> es,
    Optparse :> es,
    Terminal :> es,
    Time :> es,
    TypedProcess :> es
  ) =>
  (forall x. Eff es x -> Eff [Reader (IORef Text), IOE] x) ->
  [String] ->
  IO [Text]
runIntegrationIO toReader args = do
  ref <- newIORef ""
  _ <- runEff $ runReader ref (toReader (withArgs args' Runner.runPythia))
  T.lines <$> readIORef ref
  where
    args' = ["--no-config"] <> args

assertOutput :: [Text] -> [Text] -> IO ()
assertOutput [] [] = pure ()
assertOutput e@(_ : _) [] = assertFailure $ "Empty results but non-empty expected: " <> show e
assertOutput [] r@(_ : _) = assertFailure $ "Empty expected but non-empty results: " <> show r
assertOutput (e : es) (r : rs) = (e @=? r) *> assertOutput es rs

assertSingleOutput :: Text -> [Text] -> IO ()
assertSingleOutput _ [] = assertFailure "Wanted single result, but found empty: "
assertSingleOutput _ r@(_ : _ : _) = assertFailure $ "Wanted single result, found found > 1: " <> show r
assertSingleOutput e [r] = e @=? r

runPathReaderMock :: Eff (PathReader : es) a -> Eff es a
runPathReaderMock = interpret_ $ \case
  DoesDirectoryExist p
    | p == [osp|/sys/class/power_supply|] -> pure True
    | p == [osp|/sys/class/power_supply/BAT0|] -> pure True
    | otherwise -> pure False
  DoesFileExist p
    | p == [osp|/sys/class/power_supply/BAT0/capacity|] -> pure True
    | p == [osp|/sys/class/power_supply/BAT0/status|] -> pure True
    | otherwise -> pure False
  GetXdgDirectory _ _ -> pure [osp|test_xdg|]
  FindExecutable p
    | p == [osp|ip|] -> pure $ Just [osp|exe|]
    | p == [osp|nmcli|] -> pure $ Just [osp|exe|]
    | p == [osp|curl|] -> pure $ Just [osp|exe|]
    | p == [osp|dig|] -> pure $ Just [osp|exe|]
    | p == [osp|free|] -> pure $ Just [osp|exe|]
    | p == [osp|acpi|] -> pure $ Just [osp|exe|]
    | p == [osp|upower|] -> pure $ Just [osp|exe|]
    | otherwise -> pure Nothing
  _ -> error "runPathReaderMock: unimplemented"

runTerminalMock ::
  ( IOE :> es,
    Reader (IORef Text) :> es
  ) =>
  Eff (Terminal : es) a ->
  Eff es a
runTerminalMock = interpret_ $ \case
  PutStrLn s -> ask >>= \ref -> liftIO $ modifyIORef' ref (T.pack s <>)
  _other -> error "runTerminalMock: unimplemented"

runFileReaderStub ::
  Eff (FileReader : es) a ->
  Eff es a
runFileReaderStub = interpret_ $ \case
  _ -> error "runFileReaderStub: unimplemented"

runTypedProcessStub ::
  Eff (TypedProcess : es) a ->
  Eff es a
runTypedProcessStub = interpret_ $ \case
  _ -> error "runTypedProcessStub: unimplemented"
