# vi:set syntax=perl:

use ExtUtils::MakeMaker;

eval "use Parse::Lex";

if( $@ ) {
    print STDERR "\n\nIf Parse::Lex gives you trouble, try the patch for", 
        "Parse::Lex 2.15 that's included in this distribution.\n\n";

    sleep 3;
}

eval "use strict; use GD; gdAntiAliased";
if( $@ ) {
    print STDERR "\n\nIf GD turns out to be way more trouble than it's worth ",
        "it can be patched out of this pacakge.  See the README.\n\n";

    sleep 3;
}

WriteMakefile(
    'NAME'         => 'Games::Go::SGF2misc',
    'VERSION_FROM' => 'SGF2misc.pm',
    'PREREQ_PM'    => {
        'Parse::Lex'     => 0, 
        'Data::Dumper'   => 0, 
        'Compress::Zlib' => 0,

        'Unix::Process'  => 0, # for the memory tests

        # These are only used in debug mode... 
        # But the whole thing may parse poorly without them.
        # They're not much trouble though.
        'Devel::Size'    => 0, 
        'Number::Format' => 0, 
        'Time::HiRes'    => 0,

        # I didn't want to have to do this...
        # apparently use ::GD was getting executed before my eval()s ... 
        'GD' => 2.12,
    },
);
