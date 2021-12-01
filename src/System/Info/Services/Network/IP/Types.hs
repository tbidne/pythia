-- | Export LocalIps types.
--
-- @since 0.1.0.0
module System.Info.Services.Network.IP.Types
  ( -- * IP Types
    Ipv4,
    Ipv6,

    -- * Refined Re-exports
    Refined,
    type (\/),
    All,
    Digit,
    HexDigit,
    MaxLength,
    SymEquals,
  )
where

import Data.Text (Text)
import Refined
  ( All,
    Digit,
    HexDigit,
    MaxLength,
    Refined,
    SymEquals,
    type (\/),
  )

-- | Type for an Ipv4 address, i.e., a string of max length 15 with a mix of
-- digits and dots.
--
-- @since 0.1.0.0
type Ipv4 = Refined [All (Digit \/ SymEquals "."), MaxLength 15] Text

-- | Type for an Ipv6 address, i.e., a string of max length 39 with a mix of
-- hex digits and colons.
--
-- @since 0.1.0.0
type Ipv6 = Refined [All (HexDigit \/ SymEquals ":"), MaxLength 39] Text
