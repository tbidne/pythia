-- | @since 0.1
module Pythia.Runner.Command
  ( -- * Command
    PythiaCommandP (..),
    PythiaCommand1,
    PythiaCommand2,

    -- * Phase
    Phase (..),
  )
where

import Pythia
  ( BatteryApp,
    GlobalIpApp,
    IpType (Ipv4, Ipv6),
    MemoryApp,
    NetInterfaceApp,
    UrlSource,
  )
import Pythia.Prelude
import Pythia.Runner.Command.Battery (BatteryField)
import Pythia.Runner.Command.GlobalIp (GlobalIpField)
import Pythia.Runner.Command.Memory (MemoryField, MemoryUnits)
import Pythia.Runner.Command.NetConn (NetConnField)
import Pythia.Runner.Command.NetInterface (NetInterfaceDevice, NetInterfaceField)
import Pythia.Runner.Command.Time (TimeFormat, TimezoneDest)
import Pythia.Runner.These (These)

-- | For "Phased" data i.e. data that we want to "evolve" across phases.
-- E.g. some field might be @Maybe a@ in phase 1 but then definite @a@ in
-- phase 2.
--
-- | @since 0.1
data Phase
  = Phase1
  | Phase2

-- | General type family for:
--
-- Phase 1 -> Maybe a
-- Phase 2 -> a
type MaybePhaseF :: Phase -> Type -> Type
type family MaybePhaseF p a where
  MaybePhaseF Phase1 a = Maybe a
  MaybePhaseF Phase2 a = a

-- | Phase for GlobalIp's fields. The Field type (ipv4 vs. ipv6) and given
-- sources is mapped to These i.e.
--
-- Ipv4 -> This Ipv4Sources
-- Ipv6 -> That Ipv6Sources
-- Both -> These Ipv4Sources Ipv6Sources
type GlobalIpPhaseF :: Phase -> Type
type family GlobalIpPhaseF p where
  GlobalIpPhaseF Phase1 = (Maybe GlobalIpField, [UrlSource Ipv4], [UrlSource Ipv6])
  GlobalIpPhaseF Phase2 = These [UrlSource Ipv4] [UrlSource Ipv6]

-- | Possible commands. These type represents both the CLI Args and the
-- final result after combining CLI Args and Toml. The general idea is that
-- we start with a (Phase 1) command based purely on CLI args.
--
-- We then combine the CLI args with possible TOML config to produce the final
-- (Phase 2) command. This command has all the data necessary to be handed
-- off to Pythia (i.e. custom handlers).
--
-- @since 0.1
type PythiaCommandP :: Phase -> Type
data PythiaCommandP p
  = BatteryCmd
      (MaybePhaseF p BatteryApp)
      (MaybePhaseF p BatteryField)
  | GlobalIpCmd
      (MaybePhaseF p GlobalIpApp)
      (GlobalIpPhaseF p)
  | MemoryCmd
      (MaybePhaseF p MemoryApp)
      (MaybePhaseF p MemoryField)
      (MaybePhaseF p MemoryUnits)
  | NetConnCmd
      (MaybePhaseF p NetInterfaceApp)
      (MaybePhaseF p NetConnField)
  | NetInterfaceCmd
      (MaybePhaseF p NetInterfaceApp)
      (MaybePhaseF p NetInterfaceDevice)
      (MaybePhaseF p NetInterfaceField)
  | TimeCmd
      (MaybePhaseF p TimezoneDest)
      (MaybePhaseF p TimeFormat)

-- | @since 0.1
type PythiaCommand1 = PythiaCommandP Phase1

-- | @since 0.1
type PythiaCommand2 = PythiaCommandP Phase2

-- | @since 0.1
deriving stock instance Eq (PythiaCommandP Phase1)

-- | @since 0.1
deriving stock instance Show (PythiaCommandP Phase1)

-- | @since 0.1
deriving stock instance Eq (PythiaCommandP Phase2)

-- | @since 0.1
deriving stock instance Show (PythiaCommandP Phase2)
