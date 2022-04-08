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
import Pythia.Class.Printer (PrettyPrinter (..))
import Pythia.Control.Exception (fromExceptionViaPythia, toExceptionViaPythia)
import Pythia.Prelude
import Pythia.Services.Battery.Types
  ( Battery (..),
    BatteryPercentage (..),
    BatteryStatus (..),
  )
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
instance PrettyPrinter SysFsException where
  pretty SysFsDirNotFound =
    "SysFs exception: Could not find either dir: " <> T.pack sysDir
      <> ", "
      <> T.pack sysfsDir
  pretty SysFsBatteryDirNotFound =
    "SysFs exception: Could not find BAT[0-5]? subdirectory under"
      <> " /sys(fs)/class/power_supply"
  pretty (SysFsFileNotFound f) =
    "SysFs exception: Could not find file: <" <> f <> ">"
  pretty (SysFsBatteryParseException e) =
    "SysFs parse error: <" <> e <> ">"
  pretty (SysFsReadFileException fp e) =
    "SysFs read file <" <> fp <> "> exception: <"
      <> T.pack (displayException e)
      <> ">"

-- | @since 0.1
instance Exception SysFsException where
  displayException = T.unpack . pretty
  toException = toExceptionViaPythia
  fromException = fromExceptionViaPythia

-- | @\/sys\/class@ query for 'Battery'.
--
-- __Throws:__
--
-- * 'SysFsException': if something goes wrong (e.g. cannot find a directory,
--       file, or we have a parse error).
--
-- @since 0.1
batteryQuery :: MonadIO m => m Battery
batteryQuery = liftIO queryBattery

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
supported :: MonadIO m => m Bool
supported = liftIO $ do
  efp <- try @_ @SysFsException findSysBatDir
  case efp of
    Left _ -> pure False
    Right _ -> pure True

queryBattery :: IO Battery
queryBattery = do
  batDir <- findSysBatDir
  statusPath <- fileExists (batDir </> "status")
  status <- parseStatus statusPath
  percentPath <- fileExists (batDir </> "capacity")
  percentage <- parsePercentage percentPath
  pure $ MkBattery percentage status

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
          else throw SysFsDirNotFound
  findBatteryDir sysBase

sysDir :: FilePath
sysDir = "/sys/class/power_supply"

sysfsDir :: FilePath
sysfsDir = "/sysfs/class/power_supply"

findBatteryDir :: FilePath -> IO FilePath
findBatteryDir sysBase = do
  mResult <- foldr firstExists (pure Nothing) batDirs
  case mResult of
    Nothing -> throw SysFsBatteryDirNotFound
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
  b <- Dir.doesDirectoryExist fp
  pure $
    if b
      then Just fp
      else Nothing

fileExists :: FilePath -> IO FilePath
fileExists fp = do
  b <- Dir.doesFileExist fp
  if b
    then pure fp
    else throw $ SysFsFileNotFound $ T.pack fp

parseStatus :: FilePath -> IO BatteryStatus
parseStatus fp = do
  statusTxt <-
    T.toLower . T.strip <$> readFileUtf8Lenient fp
      `catch` \(e :: SomeException) -> throw $ SysFsReadFileException (T.pack fp) e
  case statusTxt of
    "charging" -> pure Charging
    "discharging" -> pure Discharging
    "not charging" -> pure Pending
    "full" -> pure Full
    bad -> throw $ SysFsBatteryParseException $ "Unknown status: <" <> bad <> ">"

parsePercentage :: FilePath -> IO BatteryPercentage
parsePercentage fp = do
  percentTxt <-
    readFileUtf8Lenient fp
      `catch` \(e :: SomeException) -> throw $ SysFsReadFileException (T.pack fp) e
  case readInterval percentTxt of
    Nothing -> throw $ SysFsBatteryParseException percentTxt
    Just bs -> pure $ MkBatteryPercentage bs
  where
    readInterval = Interval.mkLRInterval <=< TR.readMaybe . T.unpack
