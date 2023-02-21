-- | This module provides functionality for retrieving battery information
-- using SysFS.
--
-- @since 0.1
module Pythia.Services.Battery.SysFs
  ( -- * Query
    batteryQuery,
    supported,

    -- * Misc
    SysFsDirNotFound (..),
    SysFsBatteryDirNotFound (..),
    SysFsFileNotFound (..),
    SysFsBatteryParseError (..),
  )
where

import Data.Text qualified as T
import Numeric.Data.Interval qualified as Interval
import Pythia.Data.Percentage (Percentage (..))
import Pythia.Prelude
import Pythia.Services.Battery.Types (Battery (..), BatteryStatus (..))
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Text.Read qualified as TR

-- $setup
-- >>> import Control.Exception (displayException)
-- >>> import Pythia.Prelude

sysDir :: FilePath
sysDir = "/sys/class/power_supply"
{-# INLINEABLE sysDir #-}

sysfsDir :: FilePath
sysfsDir = "/sysfs/class/power_supply"
{-# INLINEABLE sysfsDir #-}

-- | Sysfs dir not found error.
--
-- ==== __Examples__
--
-- >>> displayException MkSysFsDirNotFound
-- "Could not find either sysfs dirs: /sys/class/power_supply, /sysfs/class/power_supply"
--
-- @since 0.1
type SysFsDirNotFound :: Type
data SysFsDirNotFound = MkSysFsDirNotFound
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception SysFsDirNotFound where
  displayException _ =
    mconcat
      [ "Could not find either sysfs dirs: ",
        sysDir,
        ", ",
        sysfsDir
      ]

-- | Sysfs battery dir not found.
--
-- ==== __Examples__
--
-- >>> displayException MkSysFsBatteryDirNotFound
-- "Could not find BAT[0-5]? subdirectory under /sys(fs)/class/power_supply"
--
-- @since 0.1
type SysFsBatteryDirNotFound :: Type
data SysFsBatteryDirNotFound = MkSysFsBatteryDirNotFound
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception SysFsBatteryDirNotFound where
  displayException _ =
    mconcat
      [ "Could not find BAT[0-5]? subdirectory under ",
        "/sys(fs)/class/power_supply"
      ]

-- | Sysfs file not found.
--
-- ==== __Examples__
--
-- >>> displayException $ MkSysFsFileNotFound "foo"
-- "Could not find sysfs file: foo"
--
-- @since 0.1
type SysFsFileNotFound :: Type
newtype SysFsFileNotFound = MkSysFsFileNotFound Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception SysFsFileNotFound where
  displayException (MkSysFsFileNotFound e) =
    ("Could not find sysfs file: " <>)
      . T.unpack
      $ e

-- | Sysfs battery parse error.
--
-- ==== __Examples__
--
-- >>> displayException $ MkSysFsBatteryParseError "bad"
-- "SysFs parse error: bad"
--
-- @since 0.1
type SysFsBatteryParseError :: Type
newtype SysFsBatteryParseError = MkSysFsBatteryParseError Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception SysFsBatteryParseError where
  displayException (MkSysFsBatteryParseError e) =
    ("SysFs parse error: " <>)
      . T.unpack
      $ e

-- | @\/sys\/class@ query for 'Battery'.
--
-- @since 0.1
batteryQuery :: IO Battery
batteryQuery = queryBattery
{-# INLINEABLE batteryQuery #-}

-- | Returns a boolean determining if this program is supported on the
-- current system. In particular, we return 'True' if the directory
--
-- @\/sys(fs)\/class\/power_supply\/BAT[0-5]+@ exists.
--
-- Example valid paths:
--
-- * @\/sys\/class\/power_supply\/BAT0@
-- * @\/sysfs\/class\/power_supply\/BAT3@
-- * @\/sys\/class\/power_supply\/BAT@
--
-- @since 0.1
supported :: IO Bool
supported = do
  efp <- tryAny findSysBatDir
  case efp of
    Left _ -> pure False
    Right _ -> pure True
{-# INLINEABLE supported #-}

queryBattery :: IO Battery
queryBattery = do
  batDir <- findSysBatDir
  statusPath <- fileExists (batDir </> "status")
  status <- parseStatus statusPath
  percentPath <- fileExists (batDir </> "capacity")
  percentage <- parsePercentage percentPath
  pure $ MkBattery percentage status
{-# INLINEABLE queryBattery #-}

findSysBatDir :: IO FilePath
findSysBatDir = do
  sysExists <- Dir.doesDirectoryExist sysDir
  sysBase <-
    if sysExists
      then pure sysDir
      else do
        sysFsExists <- Dir.doesDirectoryExist sysfsDir
        if sysFsExists
          then pure sysfsDir
          else throwCS MkSysFsDirNotFound
  findBatteryDir sysBase
{-# INLINEABLE findSysBatDir #-}

findBatteryDir :: FilePath -> IO FilePath
findBatteryDir sysBase = do
  mResult <- foldr firstExists (pure Nothing) batDirs
  case mResult of
    Nothing -> throwCS MkSysFsBatteryDirNotFound
    Just result -> pure result
  where
    firstExists bd acc = do
      e <- maybeDirExists (sysBase </> bd)
      case e of
        Nothing -> acc
        Just e' -> pure $ Just e'
    batDirs =
      [ "BAT0",
        "BAT1",
        "BAT2",
        "BAT3",
        "BAT4",
        "BAT5",
        "BAT"
      ]
{-# INLINEABLE findBatteryDir #-}

maybeDirExists :: FilePath -> IO (Maybe FilePath)
maybeDirExists fp = do
  b <- Dir.doesDirectoryExist fp
  pure $
    if b
      then Just fp
      else Nothing
{-# INLINEABLE maybeDirExists #-}

fileExists :: FilePath -> IO FilePath
fileExists fp = do
  b <- Dir.doesFileExist fp
  if b
    then pure fp
    else throwCS $ MkSysFsFileNotFound $ T.pack fp
{-# INLINEABLE fileExists #-}

parseStatus :: FilePath -> IO BatteryStatus
parseStatus fp = do
  statusTxt <-
    T.toLower
      . T.strip
      <$> readFileUtf8Lenient fp
  case statusTxt of
    "charging" -> pure Charging
    "discharging" -> pure Discharging
    "not charging" -> pure Pending
    "full" -> pure Full
    bad -> throwCS $ MkSysFsBatteryParseError $ "Unknown status: " <> bad
{-# INLINEABLE parseStatus #-}

parsePercentage :: FilePath -> IO Percentage
parsePercentage fp = do
  percentTxt <- readFileUtf8Lenient fp
  case readInterval percentTxt of
    Nothing -> throwCS $ MkSysFsBatteryParseError percentTxt
    Just bs -> pure $ MkPercentage bs
  where
    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack
{-# INLINEABLE parsePercentage #-}
