#!/usr/bin/env bash
exec ghcid --command='cabal new-repl --ghc-options="-j -Wwarn +RTS -N8 -A128m -qn4 -RTS"' --warnings "$@"
