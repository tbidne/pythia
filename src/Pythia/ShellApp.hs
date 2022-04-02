{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the functionality for running shell
-- commands and parsing the result.
--
-- @since 0.1.0.0
module Pythia.ShellApp
  ( -- * SimpleShell
    SimpleShell (..),
    runSimple,

    -- * Trying Multiple IO
    AppAction (..),
    tryAppActions,
    tryIOs,

    -- * Exceptions
    CmdError (..),
    NotSupportedError (..),
    Exceptions (..),

    -- * Utilities
    runCommand,
  )
where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import GHC.IO.Exception (ExitCode (..))
import Pythia.Data (Command (..))
import Pythia.Prelude
import System.Process.Typed qualified as TP

-- | Type for running a "simple" shell command given by 'Command'.
-- The 'parser' is used to parse the result.
--
-- @since 0.1.0.0
data SimpleShell err result = MkSimpleShell
  { -- | The shell command to run.
    --
    -- @since 0.1.0.0
    command :: Command,
    -- | The parser for the result of running the command.
    --
    -- @since 0.1.0.0
    parser :: Text -> Either err result
  }

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''SimpleShell

-- | @since 0.1.0.0
instance Bifunctor SimpleShell where
  bimap f g (MkSimpleShell c p) = MkSimpleShell c p'
    where
      p' = bimap f g . p

-- | Runs a simple shell, throwing an error if any occur.
--
-- @since 0.1.0.0
runSimple :: (Exception err, MonadIO m, Throws CmdError, Throws err) => SimpleShell err result -> m result
runSimple simple =
  runCommand (simple ^. #command)
    >>= parseAndThrow
  where
    parseAndThrow t' = liftIO $ case (simple ^. #parser) t' of
      Left err -> throw err
      Right r -> pure r

-- | Error that can occur when running a shell command.
--
-- @since 0.1.0.0
newtype CmdError = MkCmdErr String
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      Exception
    )

-- | Runs a 'Command' and returns either the text result or error encountered.
-- This is used by 'SimpleShell' to run its command before the result is
-- parsed. This function is exported as it can be for convenience.
--
-- @since 0.1.0.0
runCommand :: (MonadIO m, Throws CmdError) => Command -> m Text
runCommand command = liftIO $ do
  (exitCode, out, err) <- TP.readProcess $ TP.shell $ T.unpack cmdStr
  case exitCode of
    ExitSuccess -> case TEnc.decodeUtf8' (LBS.toStrict out) of
      Right result -> pure result
      Left ex -> throw $ MkCmdErr $ "Decode UTF-8 error: " <> show ex
    ExitFailure n ->
      throw $ MkCmdErr $ T.unpack $ shellErr n cmdStr (LBS.toStrict err)
  where
    cmdStr = command ^. #unCommand

shellErr :: Int -> Text -> ByteString -> Text
shellErr exitCode cmd err = err'
  where
    err' =
      T.concat
        [ "Error running command `",
          cmd,
          "`. Received exit code ",
          T.pack $ show exitCode,
          " and message: ",
          -- err'
          decodeUtf8Lenient err
        ]

-- | Represents some IO app to retrieve a result @r@. Includes a string
-- name and support query for determining if this action is supported by the
-- current system. Intended for when we want to try multiple actions
-- i.e. 'tryAppActions'.
--
-- @since 0.1.0.0
data AppAction m r = MkAppAction
  { action :: m r,
    supported :: m Bool,
    name :: String
  }

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''AppAction

-- | Queries for information via multiple apps. Returns the first success
-- or all errors, if there are no successes.
--
-- @since 0.1.0.0
tryAppActions :: (MonadCatch m, MonadIO m, Throws Exceptions) => [AppAction m result] -> m result
tryAppActions apps = do
  eResult <- foldr tryAppAction (pure (Left mempty)) apps
  case eResult of
    Left errs -> liftIO $ throw errs
    Right result -> pure result

-- | Error for when the current app is not supported.
--
-- @since 0.1.0.0
newtype NotSupportedError = MkNotSupportedErr String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Collects multiple errors.
--
-- @since 0.1.0.0
newtype Exceptions = MkExceptions {unExceptions :: [SomeException]}
  deriving stock (Show)
  deriving anyclass (Exception)

-- | @since 0.1.0.0
instance Semigroup Exceptions where
  MkExceptions l <> MkExceptions r = MkExceptions $ l <> r

-- | @since 0.1.0.0
instance Monoid Exceptions where
  mempty = MkExceptions mempty

tryAppAction ::
  MonadCatch m =>
  AppAction m result ->
  m (Either Exceptions result) ->
  m (Either Exceptions result)
tryAppAction appAction acc = do
  isSupported <- appAction ^. #supported
  if isSupported
    then tryIO (appAction ^. #action) acc
    else first (appendEx appUnsupportedEx) <$> acc
  where
    appendEx e (MkExceptions es) = MkExceptions (e : es)
    appUnsupportedEx = toException $ MkNotSupportedErr $ show $ appAction ^. #name

-- | Generalized 'tryAppActions' to any 'IO'. Has the same semantics
-- (i.e. returns the first success or all errs if none succeeds) without
-- checking for "support".
--
-- @since 0.1.0.0
tryIOs :: (MonadCatch m, Throws Exceptions) => [m result] -> m result
tryIOs actions = do
  eResult <- foldr tryIO (pure (Left mempty)) actions
  case eResult of
    Left errs -> throw errs
    Right result -> pure result

tryIO ::
  MonadCatch m =>
  m result ->
  m (Either Exceptions result) ->
  m (Either Exceptions result)
tryIO action acc = do
  eResult :: Either SomeException result <- try action
  case eResult of
    Right result -> pure $ Right result
    Left ex -> first (appendEx ex) <$> acc
  where
    appendEx e (MkExceptions es) = MkExceptions (e : es)
