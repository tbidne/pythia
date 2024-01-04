module Integration.Prelude
  ( module X,
    processConfigToCmd,
    runIntegrationIO,
    assertOutput,
    assertSingleOutput,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either as X (isLeft)
import Data.IORef
import Data.List qualified as L
import Data.Text qualified as T
import Effects.Exception (MonadGlobalException)
import Effects.Optparse (MonadOptparse)
import Effects.System.Environment (MonadEnv (withArgs))
import Pythia.Prelude as X
import Pythia.Runner qualified as Runner
import System.Process (CmdSpec (RawCommand, ShellCommand))
import System.Process.Typed.Internal (ProcessConfig (pcCmdSpec))
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))

processConfigToCmd :: ProcessConfig i o e -> String
processConfigToCmd pc = case pcCmdSpec pc of
  ShellCommand s -> s
  RawCommand fp args ->
    mconcat
      [ fp,
        "\n",
        L.intercalate "\n" args
      ]

runIntegrationIO ::
  forall m.
  ( MonadCatch m,
    MonadEnv m,
    MonadFileReader m,
    MonadGlobalException m,
    MonadIO m,
    MonadPathReader m,
    MonadOptparse m,
    MonadTerminal m,
    MonadTime m,
    MonadTypedProcess m
  ) =>
  (forall a. m a -> IO a) ->
  [String] ->
  IO [Text]
runIntegrationIO toIO args = do
  ref <- newIORef ""
  let handler :: Text -> m ()
      handler t = liftIO $ modifyIORef' ref (<> t)

  _ <- toIO $ withArgs args (Runner.runPythiaHandler handler)
  T.lines <$> readIORef ref

assertOutput :: [Text] -> [Text] -> IO ()
assertOutput [] [] = pure ()
assertOutput e@(_ : _) [] = assertFailure $ "Empty results but non-empty expected: " <> show e
assertOutput [] r@(_ : _) = assertFailure $ "Empty expected but non-empty results: " <> show r
assertOutput (e : es) (r : rs) = (e @=? r) *> assertOutput es rs

assertSingleOutput :: Text -> [Text] -> IO ()
assertSingleOutput _ [] = assertFailure "Wanted single result, but found empty: "
assertSingleOutput _ r@(_ : _ : _) = assertFailure $ "Wanted single result, found found > 1: " <> show r
assertSingleOutput e [r] = e @=? r
