-- | This module exports interface related services.
--
-- @since 0.1.0.0
module Pythia.Services.Network.Interface
  ( -- * Queries
    queryNetInterfaces,
    queryNetInterfacesConfig,

    -- * Types
    NetInterfaceApp (..),
    module Pythia.Services.Network.Interface.Types,
  )
where

import Pythia.Data (RunApp (..))
import Pythia.Prelude
import Pythia.Services.Network.Interface.Ip qualified as Ip
import Pythia.Services.Network.Interface.NmCli qualified as NmCli
import Pythia.Services.Network.Interface.Types (Interface, Interfaces (..), NetInterfaceApp (..), NetInterfaceConfig (..))
import Pythia.Services.Network.Types (Device)
import Pythia.ShellApp (AppAction (..))
import Pythia.ShellApp qualified as ShellApp

-- | Attempts to query for interface information by detecting supported
-- apps. Tries, in the following order: ['NetInterfaceNmCli', 'NetInterfaceIp'
--
-- @since 0.1.0.0
queryNetInterfaces :: IO Interfaces
queryNetInterfaces = queryNetInterfacesConfig mempty

-- | Attempts to query for interface information.
--
-- @since 0.1.0.0
queryNetInterfacesConfig :: NetInterfaceConfig -> IO Interfaces
queryNetInterfacesConfig config = do
  case config ^. #interfaceApp of
    Many -> ShellApp.tryAppActions allApps
    Single app -> singleRun app
  where
    allApps =
      [ MkAppAction (singleRun NetInterfaceNmCli) NmCli.supported (show NetInterfaceNmCli),
        MkAppAction (singleRun NetInterfaceIp) Ip.supported (show NetInterfaceIp)
      ]
    singleRun a =
      queryNetInterfacesDeviceApp device a
    device = config ^. #interfaceDevice

queryNetInterfacesDeviceApp :: Maybe Device -> NetInterfaceApp -> IO Interfaces
queryNetInterfacesDeviceApp Nothing app = toSingleShellApp app
queryNetInterfacesDeviceApp (Just device) app =
  filterDevice device <$> toSingleShellApp app

filterDevice :: Device -> Interfaces -> Interfaces
filterDevice device (MkInterfaces ifs) =
  MkInterfaces $
    filter ((== device) . view #idevice) ifs

toSingleShellApp :: NetInterfaceApp -> IO Interfaces
toSingleShellApp NetInterfaceNmCli = NmCli.netInterfaceShellApp
toSingleShellApp NetInterfaceIp = Ip.netInterfaceShellApp
