#!/usr/bin/env bash

# baseline performance: bsd sed
echo "//////// BSD sed"
perl -E 'say "thereisacow-$_" foreach(1..4096)' >/var/tmp/sut/lines4K.txt
perl -E 'say "thereisacow-$_" foreach(1..1048576)' >/var/tmp/sut/lines1M.txt 

time sed '' /var/tmp/sut/lines4K.txt >/dev/null
time sed '' /var/tmp/sut/lines1M.txt >/dev/null
# hsed performance:
echo "//////// haskell sed"
time hsed </var/tmp/sut/lines4K.txt >/dev/null
time hsed </var/tmp/sut/lines1M.txt >/dev/null  # 2.4s

# py -c "[print('thereisacow-{}'.format(n)) for n in range(10)]" | hsed
# perl -E 'say "thereisacow-$_" foreach(0..9)'
