# vi:fdm=marker fdl=0 syntax=perl:
# $Id: 10_as_perl_extractions.t,v 1.2 2004/03/21 16:32:45 jettero Exp $

use strict;
use Test;
use Games::Go::SGF2misc;

my $sgf = new Games::Go::SGF2misc;
   $sgf->parse("sgf/crazy.sgf");

plan tests => 8;

ok( $sgf->as_perl->[0]{game_properties}{SZ} == 5 );
ok( $sgf->as_perl->[0]{game_properties}{FF} == 3 );
ok( $sgf->as_perl->[0]{game_properties}{RU} eq 'Japanese' );
ok( $sgf->as_perl->[0]{variations} == 6 );

my $g1 = $sgf->node_list->{'game #1'};

ok( ref($g1) );
ok( $sgf->as_perl->[0]{variations} == @$g1 );
ok( $g1->[3][$#{$g1->[3]}] eq '4-26' );

my $n1 = $sgf->as_perl('4-26') or die $sgf->errstr;
ok( $n1->{move_no} == 26 and $n1->{variation} == 4 );
