set -e

export LANG="C.UTF-8"

export hs_dirs="app exe-internal src test"

nixpkgs-fmt ./

cabal-fmt --inplace pythia.cabal

# shellcheck disable=SC2046,SC2086
ormolu -m inplace $(find $hs_dirs -type f -name '*.hs')