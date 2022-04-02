module Main (main) where

import System.Environment qualified as Env
import Test.DocTest qualified as DocTest
import Prelude

main :: IO ()
main = do
  shouldRun <- Env.lookupEnv "RUN_DOCTEST"
  case shouldRun of
    Just "true" -> DocTest.doctest args
    _ -> putStrLn "*** Doctests Disabled ***"
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Pythia/Services/Battery/Acpi.hs",
    "src/Pythia/Services/Battery/Types.hs",
    "src/Pythia/Services/Battery/UPower.hs",
    "src/Pythia/Services/Network/GlobalIP/Types.hs",
    "src/Pythia/Services/Network/Interface/Types.hs",
    "src/Pythia/Data.hs",
    "src/Pythia/Printer.hs",
    "src/Pythia/Utils.hs"
  ]

-- This is needed because DocTest does not read the cabal
-- file's default-extensions
exts :: [String]
exts =
  [ "-XNoImplicitPrelude",
    "-XNoStarIsType",
    "-XApplicativeDo",
    "-XDataKinds",
    "-XDeriveGeneric",
    "-XDerivingVia",
    "-XDuplicateRecordFields",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XImportQualifiedPost",
    "-XInstanceSigs",
    "-XLambdaCase",
    "-XMultiParamTypeClasses",
    "-XMultiWayIf",
    "-XNamedFieldPuns",
    "-XNumericUnderscores",
    "-XOverloadedLabels",
    "-XOverloadedStrings",
    "-XPatternSynonyms",
    "-XScopedTypeVariables",
    "-XStandaloneKindSignatures",
    "-XTupleSections",
    "-XTypeApplications",
    "-XTypeFamilies"
  ]
