{-# LANGUAGE QuasiQuotes #-}

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
import Effects.FileSystem.PathReader qualified as Dir
import Effects.FileSystem.Utils (OsPath, decodeOsToFpShow, osp, (</>))
import Numeric.Data.Interval qualified as Interval
import Pythia.Data.Percentage (Percentage (..))
import Pythia.Prelude
import Pythia.Services.Battery.Types (Battery (..), BatteryStatus (..))
import Text.Read qualified as TR

-- $setup
-- >>> import Control.Exception (displayException)
-- >>> import Pythia.Prelude

sysDir :: OsPath
sysDir = [osp|/sys/class/power_supply|]

sysfsDir :: OsPath
sysfsDir = [osp|/sysfs/class/power_supply|]

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
      [ "Could not find either sysfs dirs: '",
        decodeOsToFpShow sysDir,
        "', '",
        decodeOsToFpShow sysfsDir,
        "'"
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
newtype SysFsFileNotFound = MkSysFsFileNotFound OsPath
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception SysFsFileNotFound where
  displayException (MkSysFsFileNotFound e) =
    mconcat
      [ "Could not find sysfs file: '",
        decodeOsToFpShow e,
        "'"
      ]

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

queryBattery :: IO Battery
queryBattery = do
  batDir <- findSysBatDir

  let statusPath = batDir </> [osp|status|]
  statusPathExists <- Dir.doesFileExist statusPath
  unless statusPathExists $ throwCS $ MkSysFsFileNotFound statusPath

  let percentPath = batDir </> [osp|capacity|]
  percentPathExists <- Dir.doesFileExist percentPath
  unless percentPathExists $ throwCS $ MkSysFsFileNotFound percentPath

  status <- parseStatus statusPath
  percentage <- parsePercentage percentPath
  pure $ MkBattery percentage status

findSysBatDir :: IO OsPath
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

findBatteryDir :: OsPath -> IO OsPath
findBatteryDir sysBase = do
  mResult <- foldr firstExists (pure Nothing) batDirs
  case mResult of
    Nothing -> throwCS MkSysFsBatteryDirNotFound
    Just result -> pure result
  where
    firstExists batDir acc = do
      e <- maybeDirExists (sysBase </> batDir)
      case e of
        Nothing -> acc
        Just e' -> pure $ Just e'
    batDirs =
      [ [osp|BAT0|],
        [osp|BAT1|],
        [osp|BAT2|],
        [osp|BAT3|],
        [osp|BAT4|],
        [osp|BAT5|],
        [osp|BAT|]
      ]

maybeDirExists :: OsPath -> IO (Maybe OsPath)
maybeDirExists fp = do
  b <- Dir.doesDirectoryExist fp
  pure
    $ if b
      then Just fp
      else Nothing

parseStatus :: OsPath -> IO BatteryStatus
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

parsePercentage :: OsPath -> IO Percentage
parsePercentage fp = do
  percentTxt <- readFileUtf8Lenient fp
  case readInterval percentTxt of
    Nothing -> throwCS $ MkSysFsBatteryParseError percentTxt
    Just bs -> pure $ MkPercentage bs
  where
    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack
