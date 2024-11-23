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
    tryIOs,

    -- * Utilities
    runCommand,
  )
where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Effectful (Effect)
import Effectful.Process.Typed.Dynamic qualified as TP
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Pythia.Control.Exception
  ( CommandException (MkCommandException),
    NoActionsRunException (MkNoActionsRunException),
    NotSupportedException (MkNotSupportedException),
    SomeExceptions (MkSomeExceptions),
  )
import Pythia.Data.Command (Command)
import Pythia.Prelude

-- | Type for running a "simple" shell command given by 'Command'.
-- The 'parser' is used to parse the result.
--
-- @since 0.1
type SimpleShell :: [Effect] -> Type -> Type -> Type
data SimpleShell es err result = MkSimpleShell
  { -- | The shell command to run.
    --
    -- @since 0.1
    command :: Command,
    -- | Determines if the shell command is supported on this system.
    --
    -- @since 0.1
    isSupported :: Eff es Bool,
    -- | The parser for the result of running the command.
    --
    -- @since 0.1
    parser :: Text -> Either err result
  }

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Command, b ~ Command) =>
  LabelOptic "command" k (SimpleShell es err result) (SimpleShell es err result) a b
  where
  labelOptic = lensVL $ \f (MkSimpleShell _command _isSupported _parser) ->
    fmap (\command' -> MkSimpleShell command' _isSupported _parser) (f _command)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Eff es Bool, b ~ Eff es Bool) =>
  LabelOptic "isSupported" k (SimpleShell es err result) (SimpleShell es err result) a b
  where
  labelOptic = lensVL $ \f (MkSimpleShell _command _isSupported _parser) ->
    fmap (\isSupported' -> MkSimpleShell _command isSupported' _parser) (f _isSupported)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ (Text -> Either err result), b ~ (Text -> Either err result)) =>
  LabelOptic "parser" k (SimpleShell es err result) (SimpleShell es err result) a b
  where
  labelOptic = lensVL $ \f (MkSimpleShell _command _isSupported _parser) ->
    fmap (MkSimpleShell _command _isSupported) (f _parser)
  {-# INLINE labelOptic #-}

-- | Runs a simple shell.
--
-- __Throws:__
--
-- * @'NotSupportedException'@: if the command is not supported on this system.
-- * @err@: if running the command throws 'SomeException' or a parse
-- error is encountered.
--
-- @since 0.1
runSimple ::
  forall err es result.
  ( Exception err,
    HasCallStack,
    TypedProcess :> es
  ) =>
  SimpleShell es err result ->
  Eff es result
runSimple simple = do
  supported <- simple ^. #isSupported
  if supported
    then runCommand command >>= parseAndThrow
    else throwM $ MkNotSupportedException (command ^. #unCommand)
  where
    command = simple ^. #command

    parseAndThrow :: Text -> Eff es result
    parseAndThrow = throwLeft . (simple ^. #parser)

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
runCommand :: (HasCallStack, TypedProcess :> es) => Command -> Eff es Text
runCommand command = do
  (exitCode, out, err) <- TP.readProcess $ TP.shell $ T.unpack cmdStr
  case exitCode of
    ExitSuccess -> pure $ decodeUtf8Lenient (LBS.toStrict out)
    ExitFailure _ ->
      throwM $ MkCommandException command $ T.pack $ show $ LBS.toStrict err
  where
    cmdStr = command ^. #unCommand

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

instance Monoid (ActionsResult r) where
  mempty = NoRuns

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
tryIOs ::
  ( HasCallStack
  ) =>
  [Eff es result] ->
  Eff es result
tryIOs actions =
  foldr tryIO (pure mempty) actions >>= \case
    Success result -> pure result
    Errs errs -> throwM $ MkSomeExceptions errs
    NoRuns -> throwM MkNoActionsRunException

tryIO ::
  ( HasCallStack
  ) =>
  Eff es result ->
  Eff es (ActionsResult result) ->
  Eff es (ActionsResult result)
tryIO action acc =
  trySync action >>= \case
    Right result -> pure $ Success result
    Left ex -> appendEx ex <$> acc

appendEx :: SomeException -> ActionsResult r -> ActionsResult r
appendEx e x = errs <> x
  where
    errs = Errs $ e :| []
