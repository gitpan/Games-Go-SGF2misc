# vi:fdm=marker fdl=0 syntax=perl:
# $Id: 07_load_sgfs.t,v 1.4 2004/03/19 15:45:49 jettero Exp $

use strict;
use Test;
use Games::Go::SGF2misc;

my @sgf = <sgf/*.sgf>;

plan tests => int @sgf;

my $sgf = new Games::Go::SGF2misc;
for my $f (@sgf) {
    my $r = $sgf->parse($f);

    if( $r ) {
        ok 1;
    } else {
        if( $f =~ m/error\.sgf/ ) {
            ok 1;
        } else {
            print STDERR " ", $sgf->errstr, "\n";
            ok 0;
        }
    }
}
