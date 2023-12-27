{-# LANGUAGE OverloadedLists #-}

module Unit.Pythia.Services.NetInterface.Types (tests) where

import Data.Text qualified as T
import Pythia.Services.NetInterface.Types
  ( NetInterface (MkNetInterface),
    NetInterfaceState (NetStateDown, NetStateUnknown, NetStateUp),
    NetInterfaceType (Wifi, Wifi_P2P),
    NetInterfaces (MkNetInterfaces),
  )
import Pythia.Services.Types.Network (IpAddress (MkIpAddress), IpRefinement)
import Refined (Predicate)
import Refined.Unsafe qualified as RUnsafe
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pythia.Services.NetInterface.Types"
    [ testDisplayNetIf,
      testDisplayNetIfs
    ]

testDisplayNetIf :: TestTree
testDisplayNetIf = testCase "Display individual ifs" $ do
  e1 @=? display n1
  e2 @=? display n2
  e3 @=? display n3
  where
    (n1, e1, n2, e2, n3, e3) = ifs

testDisplayNetIfs :: TestTree
testDisplayNetIfs = testCase "Display multiple ifs" $ do
  e @=? display (MkNetInterfaces [n1, n2, n3])
  where
    e =
      T.intercalate
        "\n\n"
        [ e1,
          e2,
          e3
        ]
    (n1, e1, n2, e2, n3, e3) = ifs

ifs :: (NetInterface, Text, NetInterface, Text, NetInterface, Text)
ifs = (n1, e1, n2, e2, n3, e3)
  where
    n1 = MkNetInterface "" Nothing NetStateUp Nothing [] []
    e1 =
      comb
        [ "Device: ",
          "Type: Nothing",
          "State: Up",
          "Name: Nothing",
          "IPv4: ",
          "IPv6: "
        ]

    n2 =
      MkNetInterface
        "device"
        (Just Wifi)
        NetStateDown
        (Just "name")
        [mkIpAddress "192.168.1.0", mkIpAddress "192.168.1.1"]
        [mkIpAddress "fe80::c34b:de57:8835:c2c7", mkIpAddress "fe80::c34b:de57:8835:c2c8"]
    e2 =
      comb
        [ "Device: device",
          "Type: Wifi",
          "State: Down",
          "Name: name",
          "IPv4: 192.168.1.0, 192.168.1.1",
          "IPv6: fe80::c34b:de57:8835:c2c7, fe80::c34b:de57:8835:c2c8"
        ]

    n3 =
      MkNetInterface
        "device"
        (Just Wifi_P2P)
        (NetStateUnknown "error")
        Nothing
        []
        [mkIpAddress "fe80::c34b:de57:8835:c2c7"]
    e3 =
      comb
        [ "Device: device",
          "Type: Wifi_P2P",
          "State: Unknown: error",
          "Name: Nothing",
          "IPv4: ",
          "IPv6: fe80::c34b:de57:8835:c2c7"
        ]

    comb = T.intercalate "\n"

mkIpAddress :: (Predicate (IpRefinement a) Text) => Text -> IpAddress a
mkIpAddress = MkIpAddress . RUnsafe.unsafeRefine
