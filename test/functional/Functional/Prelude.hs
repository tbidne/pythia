module Functional.Prelude
  ( module X,
    capturePythia,
    assertNonEmpty,
  )
where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Pythia.Prelude as X
import Pythia.Runner (runPythiaHandler)
import System.Environment qualified as SysEnv
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))

-- | Runs pythia and captures output.
--
-- @since 0.1
capturePythia :: [String] -> IO Text
capturePythia argList = do
  output <- newIORef ""
  let handler txt = modifyIORef' output (txt <>)
  SysEnv.withArgs argList (runPythiaHandler handler)
  readIORef output

assertNonEmpty :: Text -> IO ()
assertNonEmpty txt =
  assertBool
    ("Should not be empty: " <> T.unpack txt)
    (not . T.null $ txt)
