{-# LANGUAGE DeriveAnyClass #-}

-- | This module provides functionality for retrieving battery information
-- using SysFS.
--
-- @since 0.1.0.0
module Pythia.Services.Battery.SysFs
  ( -- * Query
    batteryQuery,
    supported,

    -- * Misc
    SysFsError (..),
  )
where

import Data.Text qualified as T
import Numeric.Data.Interval qualified as Interval
import Pythia.Prelude
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryPercentage,
    BatteryStatus (..),
  )
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Text.Read qualified as TR

-- | @/sys/class@ query for 'Battery'.
--
-- @since 0.1.0.0
batteryQuery :: Throws SysFsError => IO Battery
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
-- @since 0.1.0.0
supported :: IO Bool
supported = do
  efp <- try @_ @SysFsError findSysBatDir
  case efp of
    Left _ -> pure False
    Right _ -> pure True

queryBattery :: Throws SysFsError => IO Battery
queryBattery = do
  batDir <- findSysBatDir
  statusPath <- fileExists (batDir </> "status")
  status <- parseStatus statusPath
  percentPath <- fileExists (batDir </> "capacity")
  percentage <- parsePercentage percentPath
  pure $ MkBattery percentage status

findSysBatDir :: Throws SysFsError => IO FilePath
findSysBatDir = do
  sysExists <- liftIO $ Dir.doesDirectoryExist sys
  sysBase <-
    if sysExists
      then pure sys
      else do
        sysFsExists <- liftIO $ Dir.doesDirectoryExist sysfs
        if sysFsExists
          then pure sysfs
          else throw SysFsDirErr
  findBatteryDir sysBase
  where
    sys = "/sys/class/power_supply"
    sysfs = "/sysfs/class/power_supply"

findBatteryDir :: Throws SysFsError => FilePath -> IO FilePath
findBatteryDir sysBase = do
  mResult <- foldr firstExists (pure Nothing) batDirs
  case mResult of
    Nothing -> throw SysFsBatteryDirErr
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

maybeDirExists :: FilePath -> IO (Maybe FilePath)
maybeDirExists fp = do
  b <- Dir.doesFileExist fp
  pure $
    if b
      then Just fp
      else Nothing

fileExists :: Throws SysFsError => FilePath -> IO FilePath
fileExists fp = do
  b <- Dir.doesFileExist fp
  if b
    then pure fp
    else throw $ SysFsFileNotFoundErr fp

parseStatus :: FilePath -> IO BatteryStatus
parseStatus fp = do
  statusTxt <- T.toLower . T.strip <$> readFileUtf8Lenient fp
  case statusTxt of
    "charging" -> pure Charging
    "discharging" -> pure Discharging
    "not charging" -> pure Pending
    "full" -> pure Full
    bad -> pure $ Unknown bad

parsePercentage :: Throws SysFsError => FilePath -> IO BatteryPercentage
parsePercentage fp = do
  percentTxt <- readFileUtf8Lenient fp
  case readInterval percentTxt of
    Nothing -> throw $ SysFsBatteryFormatErr $ T.unpack percentTxt
    Just bs -> pure bs
  where
    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack

-- | Errors that can occur when reading sysfs.
--
-- @since 0.1.0.0
data SysFsError
  = -- | Error searching for /sys/class/power_supply or
    -- /sysfs/class/power_supply.
    --
    -- @since 0.1.0.0
    SysFsDirErr
  | -- | Error searching for <sysfs>/BAT{0-5}?.
    --
    -- @since 0.1.0.0
    SysFsBatteryDirErr
  | -- | Errors searching for files.
    --
    -- @since 0.1.0.0
    SysFsFileNotFoundErr FilePath
  | -- | Errors with the battery percentage format.
    --
    -- @since 0.1.0.0
    SysFsBatteryFormatErr String
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      Exception
    )
