{-# LANGUAGE TemplateHaskell #-}

-- | This module provides functionality for reading the cabal version
-- at compile-time.
--
-- @since 0.1.0.0
module CabalVersion
  ( -- * Type
    CabalVersion (..),
    showVersion,

    -- * TemplateHaskell
    cabalVersionTH,
    cabalVersionEitherTH,
  )
where

import Control.Applicative qualified as A
import Control.Exception.Safe (SomeException)
import Control.Exception.Safe qualified as SafeEx
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Char qualified as C
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (Version (..))
import Data.Version qualified as Vs
import Language.Haskell.TH (Q, TExp (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (..))
import Optics.Core ((%~), _Right)
import System.IO qualified as IO
import Text.Read qualified as TR

-- | Newtype wrapper over 'Version' so we can give it a 'Lift'
-- instance.
--
-- @since 0.1.0.0
newtype CabalVersion = MkCabalVersion
  { -- | @since 0.1.0.0
    unCabalVersion :: Version
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

showVersion :: CabalVersion -> String
showVersion = Vs.showVersion . unCabalVersion

-- | @since 0.1.0.0
instance Lift CabalVersion where
  liftTyped (MkCabalVersion (Version v tags)) =
    [||MkCabalVersion (Version v tags)||]

-- | TemplateHaskell for reading the cabal file's version at compile-time.
--
-- @since 0.1.0.0
cabalVersionTH :: Q (TExp CabalVersion)
cabalVersionTH = TH.runIO cabalVersion >>= either error liftTyped

-- | Version of 'cabalVersionTH' that returns an 'Either' rather than a
-- compilation error for when something goes wrong.
--
-- @since 0.1.0.0
cabalVersionEitherTH :: Q (TExp (Either String CabalVersion))
cabalVersionEitherTH = TH.runIO cabalVersion >>= liftTyped

cabalVersion :: IO (Either String CabalVersion)
cabalVersion = do
  eContents :: Either SomeException [Text] <-
    (_Right %~ T.lines . T.pack) <$> SafeEx.try (readFile' "pythia.cabal")
  pure $ case eContents of
    Left err -> Left $ show err
    Right contents -> foldr findVers noVersErr contents
  where
    noVersErr = Left "No version found in cabal file"
    findVers line acc = case AP.parseOnly cabalVersParser line of
      Left _ -> acc
      Right vers -> vers

cabalVersParser :: Parser (Either String CabalVersion)
cabalVersParser =
  (_Right %~ MkCabalVersion . Vs.makeVersion)
    <$> ( AP.string "version:"
            *> AP.skipSpace
            *> parseVers
        )
  where
    parseVers = traverse (readEither . T.unpack) <$> AP.many1 parseDigits
    parseDigits =
      AP.takeWhile1 C.isDigit
        <* A.many (AP.string ".")

readEither :: Read a => String -> Either String a
readEither str = maybe (Left err) Right (TR.readMaybe str)
  where
    err = "Could not read: " <> str

hGetContents' :: IO.Handle -> IO String
hGetContents' h = IO.hGetContents h >>= \s -> length s `seq` pure s

readFile' :: FilePath -> IO String
readFile' name = IO.openFile name IO.ReadMode >>= hGetContents'
