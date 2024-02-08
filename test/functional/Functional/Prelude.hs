{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.Prelude
  ( module X,
    capturePythia,
    assertNonEmpty,
  )
where

import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadReader (ask),
    ReaderT (runReaderT),
  )
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Effects.Optparse (MonadOptparse)
import Pythia.Prelude as X
import Pythia.Runner (runPythia)
import System.Environment qualified as SysEnv
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))

-- | Runs pythia and captures output.
--
-- @since 0.1
capturePythia :: [String] -> IO Text
capturePythia argList = SysEnv.withArgs argList (runFuncIO runPythia)

assertNonEmpty :: Text -> IO ()
assertNonEmpty txt =
  assertBool
    ("Should not be empty: " <> T.unpack txt)
    (not . T.null $ txt)

newtype FuncIO a = MkFuncIO (ReaderT (IORef Text) IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadFileReader,
      MonadOptparse,
      MonadPathReader,
      MonadIO,
      MonadTime,
      MonadThrow,
      MonadTypedProcess
    )
    via (ReaderT (IORef Text) IO)
  deriving (MonadReader (IORef Text)) via (ReaderT (IORef Text) IO)

instance MonadTerminal FuncIO where
  putStrLn s = do
    ref <- ask
    liftIO $ modifyIORef' ref (T.pack s <>)

runFuncIO :: FuncIO a -> IO Text
runFuncIO (MkFuncIO io) = do
  ref <- newIORef ""
  _ <- runReaderT io ref
  readIORef ref
