# vi:fdm=marker fdl=0 syntax=perl:
# $Id: 17_board_draw.t,v 1.2 2004/03/21 17:01:20 jettero Exp $

use strict;
use Test;
use Games::Go::SGF2misc;

my $sgf = new Games::Go::SGF2misc;
   $sgf->parse("sgf/3x3-cap.sgf");

plan tests => 4;

my $end_node = $sgf->node_list->{'game #1'}[0];  ok( ref($end_node) eq "ARRAY" );

$end_node = $end_node->[$#{ $end_node }];
ok( $end_node eq "1-8" );

ok( $sgf->as_text($end_node) eq $sgf->as_text($sgf->as_perl($end_node)) );

$sgf->parse("sgf/jettero-sixrusses-2004-03-18.sgf");

my $the_node = $sgf->node_list;
   $the_node = $the_node->{'game #1'}[0];
   $the_node = $the_node->[ $#{ $the_node } ];

my $lhs = $sgf->as_text($the_node) or die $sgf->errstr;
my $rhs = $sgf->as_perl($the_node) or die $sgf->errstr;
   $rhs = $sgf->as_text($rhs)      or die $sgf->errstr;

ok( $lhs eq $rhs );
