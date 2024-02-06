-- | @since 0.1
module Pythia.Runner.Utils
  ( decodeKeyValPairs,
    decodeKeyValPairsDefault,
  )
where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Pythia.Prelude

-- | Like 'decodeKeyValPairs', except we use the parameter function in case
-- of a match failure.
--
-- @since 0.1
decodeKeyValPairsDefault ::
  (Functor m) =>
  (Text -> a) ->
  [(Text, a)] ->
  m Text ->
  m a
decodeKeyValPairsDefault defaultVal =
  decodeMapDefault defaultVal . M.fromList

-- | Decodes the text from the list of expected keys, failing if there are no
-- matches.
--
-- @since 0.1
decodeKeyValPairs :: (MonadFail m) => [(Text, a)] -> m Text -> m a
decodeKeyValPairs = decodeMap . M.fromList

decodeMapDefault :: (Functor m) => (Text -> a) -> Map Text a -> m Text -> m a
decodeMapDefault defaultVal mp getText =
  getText <&> \k ->
    case M.lookup k mp of
      Nothing -> defaultVal k
      Just v -> v

decodeMap :: (MonadFail m) => Map Text a -> m Text -> m a
decodeMap mp getText = do
  k <- getText
  case M.lookup k mp of
    Nothing -> fail $ unknownKeyErr (prettyKeys mp) k
    Just v -> pure v

unknownKeyErr :: String -> Text -> String
unknownKeyErr expected actual =
  mconcat
    [ "Expected one of ",
      expected,
      ", received: ",
      T.unpack actual
    ]

prettyKeys :: Map Text a -> String
prettyKeys =
  T.unpack
    . (\t -> "'" <> t <> "'")
    . T.intercalate ", "
    . M.keys
