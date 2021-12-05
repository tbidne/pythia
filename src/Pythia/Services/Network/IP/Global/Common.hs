-- | This module provides helper functions for retrieving global ip addresss.
--
-- @since 0.1.0.0
module Pythia.Services.Network.IP.Global.Common
  ( globalIpShellApp,
  )
where

import Data.Char qualified as Ch
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text qualified as T
import Optics.Core (Iso', (%~), (^.), _Left, _Right)
import Pythia.Data (Command (..), QueryError)
import Pythia.Data qualified as Data
import Pythia.Services.Network.IP.Global.Types
  ( GlobalIpAddresses (..),
    GlobalIpCommand (..),
    IpStrategy (..),
    Ipv4,
    Ipv4Command (..),
    Ipv6,
    Ipv6Command (..),
  )
import Pythia.Services.Network.IP.Global.Types qualified as IpTypes
import Pythia.ShellApp (GeneralShell (..), QueryResult, ShellApp (..))
import Pythia.ShellApp qualified as ShellApp
import Refined (Predicate, Refined)
import Refined qualified as R

-- | Helper for creating a 'ShellApp' for 'GlobalIpAddresses' based on
-- parameter commands and strategy.
--
-- @since 0.1.0.0
globalIpShellApp ::
  [Ipv4Command] ->
  [Ipv6Command] ->
  IpStrategy ->
  ShellApp GlobalIpAddresses
globalIpShellApp ipv4Commands ipv6Commands strategy =
  GeneralApp $
    MkGeneralShell $ action ipv4Commands ipv6Commands strategy

action ::
  [Ipv4Command] ->
  [Ipv6Command] ->
  IpStrategy ->
  IO (QueryResult GlobalIpAddresses)
action defIpv4Cmds defIpv6Cmds strategy = do
  case strategy of
    Defaults -> anySuccess defIpv4Cmds defIpv6Cmds
    CustomUrl gic -> cmdsToResultNoDefaults gic
    CustomWithDefaults gic -> cmdsToResult defIpv4Cmds defIpv6Cmds gic

cmdsToResultNoDefaults :: GlobalIpCommand -> IO (Either [QueryError] GlobalIpAddresses)
cmdsToResultNoDefaults = cmdsToResult [] []

cmdsToResult ::
  [Ipv4Command] ->
  [Ipv6Command] ->
  GlobalIpCommand ->
  IO (Either [QueryError] GlobalIpAddresses)
cmdsToResult defIpv4Cmds defIpv6Cmds = \case
  GIpv4Command ipv4cmds ->
    (_Right %~ GIpv4)
      <$> getIpv4 (ipv4cmds <> defIpv4Cmds)
  GIpv6Command ipv6cmds ->
    (_Right %~ GIpv6)
      <$> getIpv6 (ipv6cmds <> defIpv6Cmds)
  GIpBothCommand ipv4cmds ipv6cmds ->
    anySuccess
      (ipv4cmds <> defIpv4Cmds)
      (ipv6cmds <> defIpv6Cmds)

anySuccess :: [Ipv4Command] -> [Ipv6Command] -> IO (Either [QueryError] GlobalIpAddresses)
anySuccess ipv4Cmds ipv6Cmds = do
  eIpv4s <- getIpv4 ipv4Cmds
  eIpv6s <- getIpv6 ipv6Cmds
  pure $ case (,) eIpv4s eIpv6s of
    (Right ipv4s, Right ipv6s) -> Right $ GIpBoth ipv4s ipv6s
    (Right ipv4s, _) -> Right $ GIpv4 ipv4s
    (_, Right ipv6s) -> Right $ GIpv6 ipv6s
    (Left ipv4sErrs, Left ipv6Errs) -> Left $ ipv4sErrs <> ipv6Errs

getIpv4 :: [Ipv4Command] -> IO (Either [QueryError] Ipv4)
getIpv4 = getIp IpTypes.ipv4CmdIso

getIpv6 :: [Ipv6Command] -> IO (Either [QueryError] Ipv6)
getIpv6 = getIp IpTypes.ipv6CmdIso

getIp ::
  Predicate (Proxy ps) Text =>
  Iso' a Command ->
  [a] ->
  IO (Either [QueryError] (Refined ps Text))
getIp iso = foldr go (pure (Left []))
  where
    go cmd acc = do
      res <- ShellApp.runCommand $ cmd ^. iso
      case res of
        Left err -> appendErr err <$> acc
        Right txt -> case R.refineAll (trim txt) of
          Left refEx ->
            appendErr (Data.refineExToQueryError refEx) <$> acc
          Right ip -> pure $ Right ip

    appendErr e = _Left %~ (e :)

trim :: Text -> Text
trim = T.dropAround Ch.isSpace
