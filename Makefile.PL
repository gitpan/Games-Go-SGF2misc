# vi:set syntax=perl:

use ExtUtils::MakeMaker;

eval "use Parse::Lex";

if( $@ ) {
    print STDERR "\n\nIf Parse::Lex gives you trouble, try the patch for", 
        "Parse::Lex 2.15 that's included in this distribution.\n\n";

    sleep 1;
}

WriteMakefile(
    'NAME'         => 'Games::Go::SGF2misc',
    'VERSION_FROM' => 'SGF2misc.pm',
    'PREREQ_PM'    => {'Parse::Lex'=>0, 'Data::Dumper'=>0, 'Devel::Size'=>0, 'Number::Format'=>0},
);
