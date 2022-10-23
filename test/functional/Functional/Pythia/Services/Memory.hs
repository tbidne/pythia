-- | Memory tests.
--
-- @since 0.1
module Functional.Pythia.Services.Memory
  ( tests,
  )
where

import Data.Char qualified as Char
import Data.Text qualified as T
import Functional.Prelude
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Read qualified as TR

-- | @since 0.1
tests :: TestTree
tests =
  testGroup
    "memory"
    [ testApps,
      runsFields,
      testsPercentage
    ]

testApps :: TestTree
testApps =
  testGroup
    "Tests Apps"
    [ runsFree
    ]

runsFree :: TestTree
runsFree = runsApp "free" "free"

runsApp :: String -> String -> TestTree
runsApp appCmd desc = testCase desc $ do
  let argList = ["memory", "--app", appCmd]
  result <- capturePythia argList
  parseTest result parseMemFull

type MParser :: Type -> Type
type MParser = Parsec Void Text

runsFields :: TestTree
runsFields =
  testGroup
    "Tests Fields"
    [ runsFieldTotal,
      runsFieldUsed,
      runsFieldFree
    ]

runsFieldTotal :: TestTree
runsFieldTotal = runsField "total"

runsFieldUsed :: TestTree
runsFieldUsed = runsField "used"

runsFieldFree :: TestTree
runsFieldFree = runsField "free"

runsField :: String -> TestTree
runsField field = testCase field $ do
  let argList = ["memory", "--field", field, "--app", "free"]
  result <- capturePythia argList
  parseTest result parseMemWithUnit

testsPercentage :: TestTree
testsPercentage =
  testGroup
    "Tests --percentage"
    [ runsPercentage,
      runsPercentageField "total",
      runsPercentageField "used",
      runsPercentageField "free"
    ]

runsPercentage :: TestTree
runsPercentage = testCase "full" $ do
  let argList = ["memory", "--percentage", "--app", "free"]
  result <- capturePythia argList
  parseTest result parsePercentageFull

runsPercentageField :: String -> TestTree
runsPercentageField field = testCase field $ do
  let argList = ["memory", "--percentage", "--field", field, "--app", "free"]
  result <- capturePythia argList
  parseTest result parsePercentage

parseTest :: Text -> MParser a -> IO ()
parseTest txt parser = case MP.parse parser "Memory Parse Test" txt of
  Right _ -> pure ()
  Left err -> assertFailure $ MP.errorBundlePretty err

parseMemFull :: MParser (Float, Char, Float, Char)
parseMemFull = do
  (m1, u1) <- parseMemWithUnit
  MPC.string " / "
  (m2, u2) <- parseMemWithUnit
  pure (m1, u1, m2, u2)

parsePercentageFull :: MParser (Word8, Word8)
parsePercentageFull = do
  p1 <- parsePercentageNum
  MPC.string " / "
  p2 <- parsePercentage
  pure (p1, p2)

parsePercentage :: MParser Word8
parsePercentage = parsePercentageNum <* MPC.char '%'

parsePercentageNum :: MParser Word8
parsePercentageNum = do
  digits <- MP.takeWhile1P Nothing Char.isDigit
  case TR.readMaybe (T.unpack digits) of
    Nothing -> empty
    Just n
      | n > 100 -> empty
      | otherwise -> pure n

parseMemWithUnit :: MParser (Float, Char)
parseMemWithUnit = do
  m <- parseMem
  u <- parseUnit
  pure (m, u)

parseMem :: MParser Float
parseMem = do
  result <- MP.takeWhileP Nothing (\c -> Char.isDigit c || c == '.')
  maybe empty pure (TR.readMaybe $ T.unpack result)

parseUnit :: MParser Char
parseUnit = do
  MP.try (MPC.char 'B')
    <|> MP.try (MPC.char 'K')
    <|> MP.try (MPC.char 'M')
    <|> MP.try (MPC.char 'G')
    <|> MP.try (MPC.char 'T')
    <|> MP.try (MPC.char 'P')
    <|> MP.try (MPC.char 'E')
    <|> MP.try (MPC.char 'Z')
    <|> MPC.char 'Y'
