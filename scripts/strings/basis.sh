#!/usr/bin/env bash

source "$(dirname $0)/../../ghci_helper.sh"

string_basis() {
    ghci <<HASKELL
:info String
-- String is an alias to [Char]

-- get length
length "thereis acow"

-- prepend a character (same applies to list container)
'a' : "there"
-- must use concat
-- "asdasd" : "there"

HASKELL
}

string_basis
