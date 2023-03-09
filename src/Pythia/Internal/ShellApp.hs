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
import Effects.System.Process qualified as TP
import GHC.IO.Exception (ExitCode (..))
import Pythia.Control.Exception
  ( CommandException (..),
    NoActionsRunException (..),
    NotSupportedException (..),
    SomeExceptions (..),
  )
import Pythia.Data.Command (Command (..))
import Pythia.Prelude

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
    -- | Determines if the shell command is supported on this system.
    --
    -- @since 0.1
    isSupported :: IO Bool,
    -- | The parser for the result of running the command.
    --
    -- @since 0.1
    parser :: Text -> Either err result
  }

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Command, b ~ Command) =>
  LabelOptic "command" k (SimpleShell err result) (SimpleShell err result) a b
  where
  labelOptic = lensVL $ \f (MkSimpleShell _command _isSupported _parser) ->
    fmap (\command' -> MkSimpleShell command' _isSupported _parser) (f _command)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ IO Bool, b ~ IO Bool) =>
  LabelOptic "isSupported" k (SimpleShell err result) (SimpleShell err result) a b
  where
  labelOptic = lensVL $ \f (MkSimpleShell _command _isSupported _parser) ->
    fmap (\isSupported' -> MkSimpleShell _command isSupported' _parser) (f _isSupported)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ (Text -> Either err result), b ~ (Text -> Either err result)) =>
  LabelOptic "parser" k (SimpleShell err result) (SimpleShell err result) a b
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
runSimple :: (Exception err) => SimpleShell err result -> IO result
runSimple simple = do
  supported <- simple ^. #isSupported
  if supported
    then runCommand command >>= parseAndThrow
    else throwCS $ MkNotSupportedException (command ^. #unCommand)
  where
    command = simple ^. #command
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
      throwCS $ MkCommandException command $ T.pack $ show $ LBS.toStrict err
  where
    cmdStr = command ^. #unCommand
{-# INLINEABLE runCommand #-}

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
    Errs errs -> throwCS $ MkSomeExceptions errs
    NoRuns -> throwCS MkNoActionsRunException
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
