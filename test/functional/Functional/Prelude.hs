{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.Prelude
  ( module X,
    capturePythia,
    assertNonEmpty,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
{-import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadReader (ask),
    ReaderT (runReaderT),
  )-}
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.FileSystem.PathReader.Dynamic (runPathReader)
import Effectful.Optparse.Static (Optparse, runOptparse)
import Effectful.Reader.Static (Reader, ask, runReader)
import Pythia.Prelude as X
import Pythia.Runner (runPythia)
import System.Environment qualified as SysEnv
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))

-- | Runs pythia and captures output.
--
-- @since 0.1
capturePythia :: [String] -> IO Text
capturePythia argList = SysEnv.withArgs argList (runFuncIO runPythia)

assertNonEmpty :: Text -> IO ()
assertNonEmpty txt =
  assertBool
    ("Should not be empty: " <> T.unpack txt)
    (not . T.null $ txt)

runFuncIO ::
  forall a.
  Eff
    [ FileReader,
      Optparse,
      PathReader,
      Terminal,
      Time,
      TypedProcess,
      Reader (IORef Text),
      IOE
    ]
    a ->
  IO Text
runFuncIO eff = do
  ref <- newIORef ""

  _ <-
    runEff
      . runReader ref
      . runTypedProcess
      . runTime
      . runTerminalMock
      . runPathReader
      . runOptparse
      . runFileReader
      $ eff

  readIORef ref

runTerminalMock ::
  ( IOE :> es,
    Reader (IORef Text) :> es
  ) =>
  Eff (Terminal : es) a ->
  Eff es a
runTerminalMock = interpret_ $ \case
  PutStrLn s -> ask >>= \ref -> liftIO $ modifyIORef' ref (T.pack s <>)
  _other -> error "runTerminalMock: unimplemented"
