{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Prelude
  ( module X,
    BaseIO (..),
    processConfigToCmd,
    runIntegrationIO,
    assertOutput,
    assertSingleOutput,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader as X
  ( MonadIO (liftIO),
    MonadReader (ask),
    ReaderT (runReaderT),
  )
import Data.Either as X (isLeft)
import Data.IORef as X (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Effects.FileSystem.PathReader as X
  ( MonadPathReader
      ( doesDirectoryExist,
        getXdgDirectory
      ),
  )
import Effects.Optparse (MonadOptparse)
import Effects.System.Environment (MonadEnv (withArgs))
import Pythia.Prelude as X
import Pythia.Runner qualified as Runner
import System.Process.Typed (ProcessConfig)
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))

processConfigToCmd :: ProcessConfig i o e -> String
processConfigToCmd = T.unpack . T.strip . T.pack . show

runIntegrationIO ::
  forall m.
  ( MonadCatch m,
    MonadEnv m,
    MonadFileReader m,
    MonadPathReader m,
    MonadOptparse m,
    MonadTerminal m,
    MonadTime m,
    MonadTypedProcess m
  ) =>
  (forall a. m a -> ReaderT (IORef Text) IO a) ->
  [String] ->
  IO [Text]
runIntegrationIO toReader args = do
  ref <- newIORef ""
  _ <- runReaderT (toReader (withArgs args' Runner.runPythia)) ref
  T.lines <$> readIORef ref
  where
    args' = ["--config", "off"] <> args

assertOutput :: [Text] -> [Text] -> IO ()
assertOutput [] [] = pure ()
assertOutput e@(_ : _) [] = assertFailure $ "Empty results but non-empty expected: " <> show e
assertOutput [] r@(_ : _) = assertFailure $ "Empty expected but non-empty results: " <> show r
assertOutput (e : es) (r : rs) = (e @=? r) *> assertOutput es rs

assertSingleOutput :: Text -> [Text] -> IO ()
assertSingleOutput _ [] = assertFailure "Wanted single result, but found empty: "
assertSingleOutput _ r@(_ : _ : _) = assertFailure $ "Wanted single result, found found > 1: " <> show r
assertSingleOutput e [r] = e @=? r

newtype BaseIO a = MkBaseIO (ReaderT (IORef Text) IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO
    )
    via (ReaderT (IORef Text) IO)
  deriving (MonadReader (IORef Text)) via (ReaderT (IORef Text) IO)

instance MonadTerminal BaseIO where
  putStrLn s = ask >>= \ref -> liftIO $ modifyIORef' ref (T.pack s <>)
