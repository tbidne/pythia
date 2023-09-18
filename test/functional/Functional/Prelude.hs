module Functional.Prelude
  ( module X,
    capturePythia,
    assertNonEmpty,
  )
where

import Data.Text qualified as T
import Effectful.Environment (runEnvironment, withArgs)
import Effectful.FileSystem.FileReader.Dynamic
  ( runFileReaderDynamicIO,
  )
import Effectful.FileSystem.PathReader.Dynamic
  ( runPathReaderDynamicIO,
  )
import Effectful.IORef.Static
  ( modifyIORef',
    newIORef,
    readIORef,
    runIORefStaticIO,
  )
import Effectful.Optparse.Static (runOptparseStaticIO)
import Effectful.Process.Typed (runTypedProcess)
import Effectful.Time.Dynamic (runTimeDynamicIO)
import Pythia.Prelude as X
import Pythia.Runner (runPythiaHandler)
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))

-- | Runs pythia and captures output.
--
-- @since 0.1
capturePythia :: [String] -> IO Text
capturePythia argList = run $ do
  output <- newIORef ""
  let handler txt = modifyIORef' output (txt <>)
  withArgs argList (runPythiaHandler handler)
  readIORef output
  where
    run =
      runEff
        . runEnvironment
        . runFileReaderDynamicIO
        . runIORefStaticIO
        . runOptparseStaticIO
        . runPathReaderDynamicIO
        . runTimeDynamicIO
        . runTypedProcess

assertNonEmpty :: Text -> IO ()
assertNonEmpty txt =
  assertBool
    ("Should not be empty: " <> T.unpack txt)
    (not . T.null $ txt)
