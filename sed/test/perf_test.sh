#!/usr/bin/env bash

THISDIR="$(dirname "${0}")"

# baseline performance: bsd sed
echo "//////// BSD sed"
time sed '' "${THISDIR}/lines4k.txt" >/dev/null
time sed '' "${THISDIR}/lines1M.txt" >/dev/null
# hsed performance:
echo "//////// haskell sed"
time hsed <"${THISDIR}/lines4k.txt" >/dev/null
time hsed <"${THISDIR}/lines1M.txt" >/dev/null  # 2.4s

# py -c "[print('thereisacow-{}'.format(n)) for n in range(10)]" | hsed
