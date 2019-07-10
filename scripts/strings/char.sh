#!/usr/bin/env bash

source "$(dirname $0)/../../ghci_helper.sh"

char_type() {
    ghci <<'HASKELL'
putStrLn ['\x6210']
HASKELL
}

char_type
