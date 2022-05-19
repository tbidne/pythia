module Main (main) where

import System.Environment.Guard (guardSet_)
import Test.DocTest qualified as DocTest
import Prelude

main :: IO ()
main = guardSet_ "RUN_DOCTEST" (DocTest.doctest args)
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Pythia.hs",
    "src/Pythia/Prelude.hs",
    "src/Pythia/Services/Battery/Acpi.hs",
    "src/Pythia/Services/Battery/Types.hs",
    "src/Pythia/Services/Battery/UPower.hs",
    "src/Pythia/Services/GlobalIp/Types.hs",
    "src/Pythia/Services/NetInterface/Ip.hs",
    "src/Pythia/Services/NetInterface/NmCli.hs",
    "src/Pythia/Services/NetInterface/Types.hs",
    "src/Pythia/Services/Types/Network.hs",
    "src/Pythia/Data/RunApp.hs",
    "src/Pythia/Utils.hs"
  ]

-- This is needed because DocTest does not read the cabal
-- file's default-extensions
exts :: [String]
exts =
  [ "-XNoImplicitPrelude",
    "-XNoStarIsType",
    "-XAllowAmbiguousTypes",
    "-XApplicativeDo",
    "-XConstraintKinds",
    "-XDataKinds",
    "-XDeriveAnyClass",
    "-XDeriveFunctor",
    "-XDeriveGeneric",
    "-XDerivingVia",
    "-XDuplicateRecordFields",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XFunctionalDependencies",
    "-XGADTs",
    "-XGeneralizedNewtypeDeriving",
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
    "-XRankNTypes",
    "-XScopedTypeVariables",
    "-XStandaloneDeriving",
    "-XStandaloneKindSignatures",
    "-XTupleSections",
    "-XTypeApplications",
    "-XTypeFamilyDependencies",
    "-XTypeOperators"
  ]
