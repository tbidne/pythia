{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides the functionality for running shell
-- commands and parsing the result.
--
-- @since 0.1
module Pythia.ShellApp
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
import Pythia.Data.Command (Command (..))
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
    parser :: Text -> Either err result,
    -- | Lifts an exception into @err@. Used so that a 'SimpleShell' will
    -- throw exceptions of the same type.
    --
    -- @since 0.1
    liftShellEx :: forall e. Exception e => e -> err
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''SimpleShell

-- | @since 0.1
instance Bifunctor SimpleShell where
  bimap f g (MkSimpleShell c p le) = MkSimpleShell c p' (f . le)
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
runSimple ::
  forall m err result.
  (Exception err, MonadIO m, MonadCatch m) =>
  SimpleShell err result ->
  m result
runSimple simple =
  rethrowErr `handle` runCommand (simple ^. #command) >>= parseAndThrow
  where
    rethrowErr = \(e :: SomeException) -> throwM $ (simple ^. #liftShellEx) e
    parseAndThrow t' = throwLeft $ (simple ^. #parser) t'
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
runCommand :: MonadIO m => Command -> m Text
runCommand command = liftIO $ do
  (exitCode, out, err) <- TP.readProcess $ TP.shell $ T.unpack cmdStr
  case exitCode of
    ExitSuccess -> pure $ decodeUtf8Lenient (LBS.toStrict out)
    ExitFailure _ ->
      throwM $ MkCommandException command $ T.pack $ show $ LBS.toStrict err
  where
    cmdStr = command ^. #unCommand
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
tryAppActions :: MonadCatch m => [AppAction m result] -> m result
tryAppActions apps = do
  eResult <- foldr tryAppAction (pure mempty) apps
  case eResult of
    Success result -> pure result
    Errs errs -> throwM $ MkSomeExceptions errs
    NoRuns -> throwM MkNoActionsRunException
{-# INLINEABLE tryAppActions #-}

tryAppAction ::
  MonadCatch m =>
  AppAction m result ->
  m (ActionsResult result) ->
  m (ActionsResult result)
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
tryIOs :: MonadCatch m => [m result] -> m result
tryIOs actions = do
  eResult <- foldr tryIO (pure mempty) actions
  case eResult of
    Success result -> pure result
    Errs errs -> throwM $ MkSomeExceptions errs
    NoRuns -> throwM MkNoActionsRunException
{-# INLINEABLE tryIOs #-}

tryIO ::
  MonadCatch m =>
  m result ->
  m (ActionsResult result) ->
  m (ActionsResult result)
tryIO action acc = do
  eResult :: Either SomeException result <- try action
  case eResult of
    Right result -> pure $ Success result
    Left ex -> appendEx ex <$> acc
{-# INLINEABLE tryIO #-}

appendEx :: SomeException -> ActionsResult r -> ActionsResult r
appendEx e x = errs <> x
  where
    errs = Errs $ e :| []
{-# INLINEABLE appendEx #-}
