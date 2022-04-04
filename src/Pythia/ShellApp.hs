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

    -- * MultiExceptions
    CmdException (..),
    NotSupportedException (..),
    MultiExceptions (..),

    -- * Utilities
    runCommand,
  )
where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import GHC.IO.Exception (ExitCode (..))
import Pythia.Data.Command (Command (..))
import Pythia.Prelude
import Pythia.Printer (PrettyPrinter (..))
import Pythia.Printer qualified as Printer
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

-- | Error that can occur when running a shell command.
--
-- @since 0.1.0.0
newtype CmdException = MkCmdErr String
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      Exception
    )

-- | Error for when the current app is not supported.
--
-- @since 0.1.0.0
newtype NotSupportedException = MkNotSupportedErr String
  deriving stock
    ( -- | @since 0.1.0.0
      Show
    )
  deriving anyclass
    ( -- | @since 0.1.0.0
      Exception
    )

-- | Collects multiple errors.
--
-- @since 0.1.0.0
newtype MultiExceptions = MkMultiExceptions
  { -- | @since 0.1.0.0
    unExceptions :: [SomeException]
  }

-- | Using pretty so that it shows in exception output.
--
-- @since 0.1.0.0
instance Show MultiExceptions where
  show = pretty

-- | @since 0.1.0.0
instance PrettyPrinter MultiExceptions where
  pretty (MkMultiExceptions []) = "MkMultiExceptions: []"
  pretty (MkMultiExceptions xs) =
    "MkMultiExceptions\n\t"
      <> Printer.joinX "\n\t" (fmap displayException xs)

-- | @since 0.1.0.0
instance Exception MultiExceptions where
  displayException = pretty

-- | @since 0.1.0.0
instance Semigroup MultiExceptions where
  MkMultiExceptions l <> MkMultiExceptions r = MkMultiExceptions $ l <> r

-- | @since 0.1.0.0
instance Monoid MultiExceptions where
  mempty = MkMultiExceptions mempty

-- | Runs a simple shell, throwing an error if any occur.
--
-- @since 0.1.0.0
runSimple :: (Exception err, MonadIO m, Throws CmdException, Throws err) => SimpleShell err result -> m result
runSimple simple =
  runCommand (simple ^. #command)
    >>= parseAndThrow
  where
    parseAndThrow t' = liftIO $ case (simple ^. #parser) t' of
      Left err -> throw err
      Right r -> pure r

-- | Runs a 'Command' and returns either the text result or error encountered.
-- This is used by 'SimpleShell' to run its command before the result is
-- parsed. This function is exported for convenience.
--
-- @since 0.1.0.0
runCommand :: (MonadIO m, Throws CmdException) => Command -> m Text
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
        [ "Command: ",
          cmd,
          ". Exit code: ",
          T.pack $ show exitCode,
          ". Message: ",
          decodeUtf8Lenient err
        ]

-- | Represents some IO app to retrieve a result @r@. Includes a string
-- name and support query for determining if this action is supported by the
-- current system. Intended for when we want to try multiple actions
-- i.e. 'tryAppActions'.
--
-- @since 0.1.0.0
data AppAction m r = MkAppAction
  { -- | @since 0.1.0.0
    action :: m r,
    -- | @since 0.1.0.0
    supported :: m Bool,
    -- | @since 0.1.0.0
    name :: String
  }

-- | @since 0.1.0.0
makeFieldLabelsNoPrefix ''AppAction

-- | Queries for information via multiple apps. Returns the first success
-- or all errors, if there are no successes.
--
-- @since 0.1.0.0
tryAppActions ::
  ( MonadCatch m,
    MonadIO m,
    Throws MultiExceptions
  ) =>
  [AppAction m result] ->
  m result
tryAppActions apps = do
  eResult <- foldr tryAppAction (pure (Left mempty)) apps
  case eResult of
    Left errs -> liftIO $ throw errs
    Right result -> pure result

tryAppAction ::
  MonadCatch m =>
  AppAction m result ->
  m (Either MultiExceptions result) ->
  m (Either MultiExceptions result)
tryAppAction appAction acc = do
  isSupported <- appAction ^. #supported
  if isSupported
    then tryIO (appAction ^. #action) acc
    else first (appendEx appUnsupportedEx) <$> acc
  where
    appendEx e (MkMultiExceptions es) = MkMultiExceptions (e : es)
    appUnsupportedEx = toException $ MkNotSupportedErr $ appAction ^. #name

-- | Generalized 'tryAppActions' to any 'IO'. Has the same semantics
-- (i.e. returns the first success or all errs if none succeeds) without
-- checking for "support".
--
-- @since 0.1.0.0
tryIOs :: (MonadCatch m, Throws MultiExceptions) => [m result] -> m result
tryIOs actions = do
  eResult <- foldr tryIO (pure (Left mempty)) actions
  case eResult of
    Left errs -> throw errs
    Right result -> pure result

tryIO ::
  MonadCatch m =>
  m result ->
  m (Either MultiExceptions result) ->
  m (Either MultiExceptions result)
tryIO action acc = do
  eResult :: Either SomeException result <- try action
  case eResult of
    Right result -> pure $ Right result
    Left ex -> first (appendEx ex) <$> acc
  where
    appendEx e (MkMultiExceptions es) = MkMultiExceptions (e : es)
