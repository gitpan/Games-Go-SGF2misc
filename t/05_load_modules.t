# vi:fdm=marker fdl=0 syntax=perl:
# $Id: 05_load_modules.t,v 1.2 2004/03/19 12:07:13 jettero Exp $

use strict;
use Test;

plan tests => 1;

use Games::Go::SGF2misc; ok 1;
