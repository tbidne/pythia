-- | This module exports memory related services.
--
-- @since 0.1
module Pythia.Services.Memory
  ( -- * Queries
    queryMemory,

    -- * Types
    Memory (..),

    -- ** Configuration
    MemoryConfig (..),
    MemoryApp (..),
    RunApp (..),

    -- ** Errors
    FreeException (..),
  )
where

import Pythia.Data.RunApp (RunApp (..))
import Pythia.Prelude
import Pythia.Services.Memory.Free (FreeException)
import Pythia.Services.Memory.Free qualified as Free
import Pythia.Services.Memory.Types
  ( Memory (..),
    MemoryApp (..),
    MemoryConfig (..),
  )
import Pythia.ShellApp (AppAction (..))
import Pythia.ShellApp qualified as ShellApp

-- | Queries the memory based on the configuration. If 'memoryApp' is
-- 'Many' then we try supported apps in the following order:
--
-- @
-- ['MemoryFree']
-- @
--
-- __Throws:__
--
-- * 'Pythia.Control.Exception.PythiaException': if an error is
-- encountered (e.g. running a command or parse error).
--
-- @since 0.1
queryMemory :: MonadUnliftIO m => MemoryConfig -> m Memory
queryMemory config =
  case config ^. #memoryApp of
    Many -> ShellApp.tryAppActions allApps
    Single app -> toShellApp app
  where
    allApps =
      [ MkAppAction (toShellApp MemoryFree) Free.supported (showt MemoryFree)
      ]

toShellApp :: MonadUnliftIO m => MemoryApp -> m Memory
toShellApp MemoryFree = Free.memoryShellApp
