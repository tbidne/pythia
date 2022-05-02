module Functional.Prelude
  ( module X,
    stripTasty,
    capturePythia,
    assertNonEmpty,
  )
where

import Data.Text qualified as T
import Pythia.Prelude as X
import Pythia.Runner (runPythia)
import System.Environment qualified as SysEnv
import System.IO.Silently qualified as Shh
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))

-- | When capturing output for testing, need to account for tasty's
-- stdout that is sometimes captured.
--
-- @since 0.1
stripTasty :: Text -> Text
stripTasty (stripThenPrefix "OK" -> Just rest) = rest
stripTasty txt = T.strip txt

stripThenPrefix :: Text -> Text -> Maybe Text
stripThenPrefix prefix = T.stripPrefix prefix . T.strip

capturePythia :: [String] -> IO Text
capturePythia argList =
  stripTasty . T.pack <$> Shh.capture_ (SysEnv.withArgs argList runPythia)

assertNonEmpty :: Text -> IO ()
assertNonEmpty txt =
  assertBool
    ("Should not be empty: " <> T.unpack txt)
    (not . T.null $ txt)
