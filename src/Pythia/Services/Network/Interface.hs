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

import Pythia.Data (QueryResult, RunApp (..))
import Pythia.Prelude
import Pythia.Services.Network.Interface.Ip qualified as Ip
import Pythia.Services.Network.Interface.NmCli qualified as NmCli
import Pythia.Services.Network.Interface.Types (Interface, Interfaces (..), NetInterfaceApp (..), NetInterfaceConfig (..))
import Pythia.Services.Network.Types (Device)
import Pythia.ShellApp (ShellApp (..))
import Pythia.ShellApp qualified as ShellApp

-- | Attempts to query for interface information by detecting supported
-- apps. Tries, in the following order: ['InterfaceSysFs', 'InterfaceAcpi',
-- 'InterfaceUPower']
--
-- @since 0.1.0.0
queryNetInterfaces :: IO (QueryResult Interfaces)
queryNetInterfaces = queryNetInterfacesConfig mempty

-- | Attempts to query for interface information by detecting supported
-- apps. Tries, in the following order: ['InterfaceSysFs', 'InterfaceAcpi',
-- 'InterfaceUPower']
--
-- @since 0.1.0.0
queryNetInterfacesConfig :: NetInterfaceConfig -> IO (QueryResult Interfaces)
queryNetInterfacesConfig config = do
  case config ^. #interfaceApp of
    Many -> ShellApp.tryIOs allApps
    Single app -> singleRun app
  where
    allApps =
      [ (singleRun NetInterfaceNmCli, NmCli.supported),
        (singleRun NetInterfaceIp, Ip.supported)
      ]
    singleRun a =
      queryNetInterfacesDeviceApp device a
    device = (config ^. #interfaceDevice)

queryNetInterfacesDeviceApp :: Maybe Device -> NetInterfaceApp -> IO (QueryResult Interfaces)
queryNetInterfacesDeviceApp Nothing app = ShellApp.runShellApp (toSingleShellApp app)
queryNetInterfacesDeviceApp (Just device) app =
  second (filterDevice device) <$> ShellApp.runShellApp (toSingleShellApp app)

filterDevice :: Device -> Interfaces -> Interfaces
filterDevice device (MkInterfaces ifs) =
  MkInterfaces $
    filter ((== device) . view #idevice) ifs

toSingleShellApp :: NetInterfaceApp -> ShellApp Interfaces
toSingleShellApp NetInterfaceNmCli = NmCli.netInterfaceShellApp
toSingleShellApp NetInterfaceIp = Ip.netInterfaceShellApp
