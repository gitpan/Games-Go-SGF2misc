# vi:set syntax=perl:

use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'         => 'Games::Go::SGF2misc',
    'VERSION_FROM' => 'SGF2misc.pm',
    'PREREQ_PM'    => {'Parse::Lex'=>0, 'Data::Dumper'=>0, 'Devel::Size'=>0, 'Number::Format'=>0},
);
