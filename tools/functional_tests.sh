set -e

export LANG="C.UTF-8"

cabal configure --enable-tests --ghc-options -Werror

cabal update

cabal build

cabal test functional