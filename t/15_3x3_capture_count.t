# vi:fdm=marker fdl=0 syntax=perl:
# $Id: 15_3x3_capture_count.t,v 1.1 2004/03/21 16:05:35 jettero Exp $

use strict;
use Test;
use Games::Go::SGF2misc;

my $sgf = new Games::Go::SGF2misc;
   $sgf->parse("sgf/3x3-cap.sgf");

plan tests => 5;

my $end_node = $sgf->node_list->{'game #1'}[0];  ok( ref($end_node) eq "ARRAY" );

$end_node = $end_node->[$#{ $end_node }];
ok( $end_node eq "1-8" );

$end_node = $sgf->as_perl( $end_node ) or die $sgf->errstr;
ok( ref($end_node) eq "HASH" );

ok( $end_node->{captures}{W} == 1 );
ok( $end_node->{captures}{B} == 2 );
