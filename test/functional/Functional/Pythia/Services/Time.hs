-- | Time tests.
--
-- @since 0.1
module Functional.Pythia.Services.Time
  ( tests,
  )
where

import Data.Text qualified as T
import Functional.Prelude

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "time"
    [ queryLocal,
      queryUtc,
      queryNy,
      formatsHM
    ]

queryLocal :: TestTree
queryLocal = runsTimeNonEmpty Nothing Nothing "Queries local time"

queryUtc :: TestTree
queryUtc = runsTime expect (Just "utc") Nothing "Queries utc time"
  where
    expect result =
      assertBool ("Expected 'UTC' in: " <> T.unpack result) (T.isInfixOf "UTC" result)

queryNy :: TestTree
queryNy = runsTime expect (Just "America/New_York") Nothing "Queries tz time"
  where
    expect result =
      assertBool
        ("Expected 'EST' or 'EDT' in: " <> T.unpack result)
        (T.isInfixOf "EST" result || T.isInfixOf "EDT" result)

formatsHM :: TestTree
formatsHM = runsTime expect Nothing (Just "%H:%M") "Formats %H:%M"
  where
    expect result = case T.unpack result of
      (_ : _ : ':' : _ : _) -> pure ()
      bad -> assertFailure $ "Expected HH:MM: " <> bad

runsTimeNonEmpty :: Maybe String -> Maybe String -> String -> TestTree
runsTimeNonEmpty = runsTime assertNonEmpty

runsTime :: (Text -> IO ()) -> Maybe String -> Maybe String -> String -> TestTree
runsTime expectation ty format desc = testCase desc $ do
  let argList =
        ["time"]
          <> maybe [] (\s -> ["--type", s]) ty
          <> maybe [] (\f -> ["--format", f]) format
  capturePythia argList >>= expectation
