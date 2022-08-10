{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the functionality for running shell
-- commands and parsing the result.
--
-- @since 0.1
module Pythia.Internal.ShellApp
  ( -- * SimpleShell
    SimpleShell (..),
    runSimple,

    -- * Trying Multiple IO
    AppAction (..),
    tryAppActions,
    tryIOs,

    -- * Utilities
    runCommand,
  )
where

import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import GHC.IO.Exception (ExitCode (..))
import Pythia.Control.Exception
  ( CommandException (..),
    NoActionsRunException (..),
    NotSupportedException (..),
    SomeExceptions (..),
  )
import Pythia.Data.Command (Command (..), _MkCommand)
import Pythia.Prelude
import System.Process.Typed qualified as TP

-- | Type for running a "simple" shell command given by 'Command'.
-- The 'parser' is used to parse the result.
--
-- @since 0.1
type SimpleShell :: Type -> Type -> Type
data SimpleShell err result = MkSimpleShell
  { -- | The shell command to run.
    --
    -- @since 0.1
    command :: Command,
    -- | The parser for the result of running the command.
    --
    -- @since 0.1
    parser :: Text -> Either err result
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''SimpleShell

-- | @since 0.1
instance Bifunctor SimpleShell where
  bimap f g (MkSimpleShell c p) = MkSimpleShell c p'
    where
      p' = bimap f g . p
  {-# INLINEABLE bimap #-}

-- | Runs a simple shell.
--
-- __Throws:__
--
-- * @err@: if running the command throws 'SomeException' or a parse
-- error is encountered.
--
-- @since 0.1
runSimple :: Exception err => SimpleShell err result -> IO result
runSimple simple = runCommand (simple ^. #command) >>= parseAndThrow
  where
    parseAndThrow = throwLeft . (simple ^. #parser)
{-# INLINEABLE runSimple #-}

-- | Runs a 'Command' and returns either the text result or error encountered.
-- This is used by 'SimpleShell' to run its command before the result is
-- parsed. This function is exported for convenience.
--
-- __Throws:__
--
-- * 'CommandException': if running the command returns 'ExitFailure' exit
-- code.
--
-- @since 0.1
runCommand :: Command -> IO Text
runCommand command = do
  (exitCode, out, err) <- TP.readProcess $ TP.shell $ T.unpack cmdStr
  case exitCode of
    ExitSuccess -> pure $ decodeUtf8Lenient (LBS.toStrict out)
    ExitFailure _ ->
      throwIO $ MkCommandException command $ T.pack $ show $ LBS.toStrict err
  where
    cmdStr = command ^. _MkCommand
{-# INLINEABLE runCommand #-}

-- | Represents some IO app to retrieve a result @r@. Includes a string
-- name and support query for determining if this action is supported by the
-- current system. Intended for when we want to try multiple actions
-- i.e. 'tryAppActions'.
--
-- @since 0.1
type AppAction :: (Type -> Type) -> Type -> Type
data AppAction m r = MkAppAction
  { -- | @since 0.1
    action :: m r,
    -- | @since 0.1
    supported :: m Bool,
    -- | @since 0.1
    name :: Text
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''AppAction

-- Three possible results when running actions:
--
-- 1. None are given/supported, so none are run.
-- 2. We encounter at least one error.
-- 3. We have a success.
--
-- The semigroup takes the first success, as that gives us the semantics we
-- want.
type ActionsResult :: Type -> Type
data ActionsResult r
  = NoRuns
  | Errs (NonEmpty SomeException)
  | Success r

instance Semigroup (ActionsResult r) where
  Success x <> _ = Success x
  _ <> Success x = Success x
  NoRuns <> r = r
  l <> NoRuns = l
  Errs x <> Errs y = Errs $ x <> y
  {-# INLINEABLE (<>) #-}

instance Monoid (ActionsResult r) where
  mempty = NoRuns
  {-# INLINEABLE mempty #-}

-- | Queries for information via multiple apps. Returns the first success.
-- If any errors are encountered or no actions are run (either because the
-- list is empty or none are supported), an exception is thrown.
--
-- __Throws:__
--
-- * 'NoActionsRunException': if no actions are run (i.e. the list is empty
--       or none are supported).
--
-- * 'SomeExceptions': if at least one command is run yet there were no
--       successes.
--
-- @since 0.1
tryAppActions :: [AppAction IO result] -> IO result
tryAppActions apps =
  foldr tryAppAction (pure mempty) apps >>= \case
    Success result -> pure result
    Errs errs -> throwIO $ MkSomeExceptions errs
    NoRuns -> throwIO MkNoActionsRunException
{-# INLINEABLE tryAppActions #-}

tryAppAction ::
  AppAction IO result -> IO (ActionsResult result) -> IO (ActionsResult result)
tryAppAction appAction acc = do
  isSupported <- appAction ^. #supported
  if isSupported
    then tryIO (appAction ^. #action) acc
    else appendEx appUnsupportedEx <$> acc
  where
    appUnsupportedEx = toException $ MkNotSupportedException $ appAction ^. #name
{-# INLINEABLE tryAppAction #-}

-- | Generalized 'tryAppActions' to any 'IO'. Has the same semantics
-- (i.e. returns the first success or throws an exception if none
-- succeeds) without checking for "support".
--
-- __Throws:__
--
-- * 'NoActionsRunException': if no actions are run (i.e. the list is empty).
--
-- * 'SomeExceptions': if at least one command is run yet there were no
--       successes.
--
-- @since 0.1
tryIOs :: [IO result] -> IO result
tryIOs actions =
  foldr tryIO (pure mempty) actions >>= \case
    Success result -> pure result
    Errs errs -> throwIO $ MkSomeExceptions errs
    NoRuns -> throwIO MkNoActionsRunException
{-# INLINEABLE tryIOs #-}

tryIO :: IO result -> IO (ActionsResult result) -> IO (ActionsResult result)
tryIO action acc =
  tryAny action >>= \case
    Right result -> pure $ Success result
    Left ex -> appendEx ex <$> acc
{-# INLINEABLE tryIO #-}

appendEx :: SomeException -> ActionsResult r -> ActionsResult r
appendEx e x = errs <> x
  where
    errs = Errs $ e :| []
{-# INLINEABLE appendEx #-}
