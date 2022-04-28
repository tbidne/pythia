module Unit.Pythia.Services.Memory.Free
  ( tests,
  )
where

import ByteTypes.Bytes (Bytes (..))
import Data.Text qualified as T
import Pythia.Services.Memory.Free qualified as Free
import Pythia.Services.Memory.Types (Memory (..))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pythia.Services.Memory.Free"
    [parseFree]

parseFree :: TestTree
parseFree = testCase "Parses free output" $ do
  let result = Free.parseMemory freeTxt
  Just expected @=? result ^? _Right
  where
    expected = MkMemory (MkBytes 16176768) (MkBytes (3549664 + 2276344))
    freeTxt =
      T.unlines
        [ "              total        used        free      shared  buff/cache   available",
          "Mem:       16176768     3549664     2732324     2276344     9894780    10046376",
          "Swap:      33554428     2282752    31271676"
        ]
