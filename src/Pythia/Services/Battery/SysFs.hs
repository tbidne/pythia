{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for retrieving battery information
-- using SysFS.
--
-- @since 0.1
module Pythia.Services.Battery.SysFs
  ( -- * Query
    batteryQuery,
    supported,

    -- * Misc
    SysFsException (..),
  )
where

import Data.Text qualified as T
import Numeric.Data.Interval qualified as Interval
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Data.Percentage (Percentage (..))
import Pythia.Prelude
import Pythia.Services.Battery.Types (Battery (..), BatteryStatus (..))
import Pythia.Utils (Pretty (..))
import Pythia.Utils qualified as U
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Text.Read qualified as TR

-- $setup
-- >>> import GHC.Exception (errorCallException)

-- | Errors that can occur with sysfs.
--
-- ==== __Examples__
--
-- >>> putStrLn $ displayException SysFsDirNotFound
-- SysFs exception: Could not find either dir: /sys/class/power_supply, /sysfs/class/power_supply
--
-- >>> putStrLn $ displayException SysFsBatteryDirNotFound
-- SysFs exception: Could not find BAT[0-5]? subdirectory under /sys(fs)/class/power_supply
--
-- >>> putStrLn $ displayException $ SysFsFileNotFound "foo"
-- SysFs exception: Could not find file: <foo>
--
-- >>> putStrLn $ displayException $ SysFsBatteryParseException "parse error"
-- SysFs parse error: <parse error>
--
-- >>> putStrLn $ displayException $ SysFsReadFileException "foo" (errorCallException "oh no")
-- SysFs read file <foo> exception: <oh no>
--
-- @since 0.1
type SysFsException :: Type
data SysFsException
  = -- | Error searching for \/sys\/class\/power_supply or
    -- \/sysfs\/class\/power_supply.
    --
    -- @since 0.1
    SysFsDirNotFound
  | -- | Error searching for \<sysfs\>/BAT{0-5}?.
    --
    -- @since 0.1
    SysFsBatteryDirNotFound
  | -- | Errors searching for files.
    --
    -- @since 0.1
    SysFsFileNotFound Text
  | -- | Errors with the battery percentage format.
    --
    -- @since 0.1
    SysFsBatteryParseException Text
  | -- | Error reading a file.
    --
    -- @since 0.1
    forall e. Exception e => SysFsReadFileException Text e

-- | @since 0.1
makePrismLabels ''SysFsException

-- | @since 0.1
deriving stock instance Show SysFsException

-- | @since 0.1
instance Pretty SysFsException where
  pretty SysFsDirNotFound =
    pretty @Text "SysFs exception: Could not find either dir: "
      <> pretty sysDir
      <> pretty @Text ", "
      <> pretty sysfsDir
  pretty SysFsBatteryDirNotFound =
    pretty @Text $
      "SysFs exception: Could not find BAT[0-5]? subdirectory under"
        <> " /sys(fs)/class/power_supply"
  pretty (SysFsFileNotFound f) =
    pretty @Text "SysFs exception: Could not find file: <"
      <> pretty f
      <> pretty @Text ">"
  pretty (SysFsBatteryParseException e) =
    pretty @Text "SysFs parse error: <"
      <> pretty e
      <> pretty @Text ">"
  pretty (SysFsReadFileException fp e) =
    pretty @Text "SysFs read file <"
      <> pretty fp
      <> pretty @Text "> exception: <"
      <> pretty (displayException e)
      <> pretty @Text ">"
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Exception SysFsException where
  displayException = T.unpack . U.prettyToText
  {-# INLINEABLE displayException #-}
  toException = toExceptionViaPythia
  {-# INLINEABLE toException #-}
  fromException = fromExceptionViaPythia
  {-# INLINEABLE fromException #-}

-- | @\/sys\/class@ query for 'Battery'.
--
-- __Throws:__
--
-- * 'SysFsException': if something goes wrong (e.g. cannot find a directory,
--       file, or we have a parse error).
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
  efp <- try @_ @SysFsException findSysBatDir
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
          else throwIO SysFsDirNotFound
  findBatteryDir sysBase
{-# INLINEABLE findSysBatDir #-}

sysDir :: FilePath
sysDir = "/sys/class/power_supply"
{-# INLINEABLE sysDir #-}

sysfsDir :: FilePath
sysfsDir = "/sysfs/class/power_supply"
{-# INLINEABLE sysfsDir #-}

findBatteryDir :: FilePath -> IO FilePath
findBatteryDir sysBase = do
  mResult <- foldr firstExists (pure Nothing) batDirs
  case mResult of
    Nothing -> throwIO SysFsBatteryDirNotFound
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
    else throwIO $ SysFsFileNotFound $ T.pack fp
{-# INLINEABLE fileExists #-}

parseStatus :: FilePath -> IO BatteryStatus
parseStatus fp = do
  statusTxt <-
    T.toLower
      . T.strip
      <$> readFileUtf8Lenient fp
      `catchAny` \e -> throwIO $ SysFsReadFileException (T.pack fp) e
  case statusTxt of
    "charging" -> pure Charging
    "discharging" -> pure Discharging
    "not charging" -> pure Pending
    "full" -> pure Full
    bad -> throwIO $ SysFsBatteryParseException $ "Unknown status: <" <> bad <> ">"
{-# INLINEABLE parseStatus #-}

parsePercentage :: FilePath -> IO Percentage
parsePercentage fp = do
  percentTxt <-
    readFileUtf8Lenient fp
      `catchAny` \e -> throwIO $ SysFsReadFileException (T.pack fp) e
  case readInterval percentTxt of
    Nothing -> throwIO $ SysFsBatteryParseException percentTxt
    Just bs -> pure $ MkPercentage bs
  where
    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack
{-# INLINEABLE parsePercentage #-}
