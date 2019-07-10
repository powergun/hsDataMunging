#!/usr/bin/env perl

# see rot16 for character rotation-encryption using y/// operator
# perlAlgorithms/algorithms/rot16_oneliners.sh
# dd if=/dev/urandom bs=128 count=1 2>/dev/null | \
#     xxd | \
#     perl -wnl -a -E 'my $l = join(" ", @F[1..8]); $l =~ y/a-z/0-9/; say $l'

use 5.016;
use strict;
use warnings;

sub main {
    foreach(1..10) {
        say join " ", map {int(rand(99-34))+34} 1..10
    }
}

main();
