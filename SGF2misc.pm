# vi:fdm=marker fdl=0
# $Id: SGF2misc.pm,v 1.7 2006/03/09 19:00:22 jettero Exp $ 

package Games::Go::SGF2misc;

use strict;
no warnings;

use Carp;
use Parse::Lex;
use Data::Dumper;
use Compress::Zlib;

our $VERSION = "0.9.6";

1;

# main calls
# new {{{
sub new {
    my $this = shift;
       $this = bless {}, $this;

    if( $ENV{DEBUG} > 0 ) {
        use Number::Format;
        use Devel::Size qw(total_size);
        use Time::HiRes qw(time);

        $this->{frm} = new Number::Format;
    }

    return $this;
}
# }}}
# parse {{{
sub parse {
    my $this = shift;
    my $file = shift;

    if( -f $file ) {
        local $/;  # Enable local "slurp" ... ie, by unsetting $/ for this local scope, it will not end lines on \n
        open SGFIN, $file or die "couldn't open $file: $!";

        return $this->parse_internal(\*SGFIN);
    }

    $this->{error} = "Parse Error reading $file: unknown";
    return 0;
}
# }}}
# parse_string {{{
sub parse_string {
    my $this = shift;
    my $string = shift;

    return $this->parse_internal($string);
}
# }}}
# parse_internal {{{
sub parse_internal {
    my $this = shift;
    my $file = shift;

    for my $k (keys %$this) {
        delete $this->{$k} unless {Time=>1, frm=>1}->{$k};
    }
    $global::lex_error = undef;

    $this->_time("parse");

    my @rules = (
        VALUE  => '\[(?ms:.*?(?<!\x5c))\]',

        BCOL   => '\(',         # begin collection
        ECOL   => '\)',         # end collection
        PID    => '[A-Z]+',     # property identifier
        NODE   => ';',          # new node
        WSPACE => '[\s\r\n]',

        qw(ERROR  .*), sub {
            $global::lex_error = "Parse Error reading $file: $_[1]\n";
        }
    );

    Parse::Lex->trace if $ENV{DEBUG} > 30;

    my $lex = new Parse::Lex(@rules); $^W = 0; no warnings;
       $lex->from($file);

    $this->{parse} = { p => undef, n => [], c=>[] }; # p => parent, n => nodes, c => child Collections

    my $ref = $this->{parse};  # our current position

    # parse rules:
    my $nos = -1;  # the current node (array position).  -1 when we're not in a node
    my $pid = 0;   # 0 unless we just got a pid; otherwise, node array position

    TOKEN: while (1) {
        my $token = $lex->next;

        if (not $lex->eoi) {
            my $C = $token->name;
            my $V = $token->text;

            if( $C eq "ERROR" or defined $global::lex_error ) {
                $global::lex_error = "Parse Error reading $file: unknown";
                $this->{error} = $global::lex_error;
                return 0;
            }

            if( $C eq "BCOL" ) { 
                push @{ $ref->{c} }, { p=>$ref, n=>[], c=>[] };
                $ref = $ref->{c}[$#{ $ref->{c} }];
                $nos = -1;

            } elsif( $C eq "ECOL" ) { 
                $ref = $ref->{p};
                $nos = -1;

            } elsif( $C eq "NODE" ) {
                push @{ $ref->{n} }, [];
                $nos = $#{ $ref->{n} };

            } 
            
            # this get's it's own if block for the $pid
            if( $C eq "PID" ) {
                if( $nos == -1 ) {
                    $this->{error} = "Parse Error reading $file: property identifier ($V) in strange place";
                    return 0;
                }
                push @{ $ref->{n}[$nos] }, {P=>$V};
                $pid = $#{ $ref->{n}[$nos] };

            } elsif( $C eq "VALUE" ) {
                $V =~ s/^\[//ms; $V =~ s/\]$//ms;
                $V =~ s/\\(.)/$1/msg;

                if( $nos == -1 or $pid == -1 ) {
                    $this->{error} = "Parse Error reading $file: property value ($V) in strange place";
                    return 0;
                }
                if( defined $ref->{n}[$nos][$pid]{V} ) {
                    push @{ $ref->{n}[$nos] }, {P=>$ref->{n}[$nos][$pid]{P}};
                    $pid = $#{ $ref->{n}[$nos] };
                }

                $ref->{n}[$nos][$pid]{V} = $V;

            } else {
                $pid = -1;
            }

        } else {
            last TOKEN;
        }
    }

    $this->_time("parse");

    print STDERR "SGF Parsed!  Calling internal _parse() routine\n" if $ENV{DEBUG} > 0;
    print STDERR "\$this size (before _parse)= ", $this->{frm}->format_bytes(total_size( $this )), "\n" if $ENV{DEBUG} > 0;

    $this->_time("_parse");

    my $r = $this->_parse(0, $this->{parse});

    $this->_time("_parse");

    print STDERR "\$this size (after _parse)= ", $this->{frm}->format_bytes(total_size( $this )), "\n" if $ENV{DEBUG} > 0;

    print STDERR "rebuilding {refdb} (for ref2id/id2ref)\n" if $ENV{DEBUG} > 0;

    $this->_time("_nodelist");

    $this->{nodelist} = { map {$this->_ref2id($_) => $this->_nodelist([], $_)} @{$this->{gametree}} };

    $this->_time("_nodelist");

    print STDERR "\$this size (after _nodelist())= ", $this->{frm}->format_bytes(total_size( $this )), "\n" if $ENV{DEBUG} > 0;

    $this->_time("nuke(gametree and parse)");
    my @to_nuke;
    
    push @to_nuke, (@{$this->{gametree}}) if ref($this->{gametree}) eq "ARRAY";
    push @to_nuke, $this->{parse}         if ref($this->{parse})    eq "HASH";

    while( @to_nuke ) {
        my $ref = shift @to_nuke;
        for my $k (qw(p c kids parent)) {
            if( my $v = $ref->{$k} ) {
                if( ref($v) eq "ARRAY" ) {
                    push @to_nuke, @$v;
                }

                delete $ref->{$k};
            }
        }
    }
    $this->_time("nuke(gametree and parse)");

    $this->_show_timings if $ENV{DEBUG} > 0;

    return $r;
}
# }}}
# freeze {{{
sub freeze {
    my $this = shift;

    local $Data::Dumper::Indent = 0;
    local $Data::Dumper::Purity = 1;

    my $fm = {};
    for my $k (qw(nodelist refdb)) {
        $fm->{$k} = $this->{$k};
    }

    $this->_time("freeze Dumper");
    my $buf = Dumper( $fm );
    $this->_time("freeze Dumper");

    return Compress::Zlib::memGzip( $buf );
}
# }}}
# thaw {{{
sub thaw {
    my $this = shift;
    my $frz  = shift;
    my ($VAR1);

    if( ref($frz) eq "GLOB" ) {
        $this->_time("gzreads");
        my $gz = gzopen($frz, "r"); 

        $frz = "";

        my $x;
        while( my $r = $gz->gzread($x, 32768) ) {
            $frz .= $x;
        }
        $gz->gzclose;

        $this->_time("gzreads");

        $this->_time("eval");
        eval $frz;
        $this->_time("eval");
    } else {
        $this->_time("memgunzip/eval");
        eval Compress::Zlib::memGunzip( $frz );
        $this->_time("memgunzip/eval");
        if( $@ ) {
            $this->{error} = $@;
            return 0;
        }
    }

    $this->_time("assign refs");
    for my $k (keys %$VAR1) {
        $this->{$k} = $VAR1->{$k};
    }
    $this->_time("assign refs");

    $this->_show_timings if $ENV{DEBUG} > 0;

    return 1;
}
# }}}
# errstr {{{
sub errstr {
    my $this = shift;

    $this->{error} =~ s/[\r\n\s]$//msg;

    return $this->{error};
}
# }}}

# tools -- can croak()!
# sgfco2numco {{{
sub sgfco2numco {
    my $this = shift;
    my $gref = shift;
    my $co   = shift;

    my ($sz, $ff);
    if( ref($gref) eq "HASH" and ref($gref->{game_properties}) eq "HASH" ) {
        $sz = $gref->{game_properties}{SZ};
        $ff = $gref->{game_properties}{FF};

        unless( $sz and $ff ) {
            croak "Error: sgfco2numco needs FF and SZ properties to function, sorry.\n";
        }
    } else {
        croak "Syntax Error: You must pass a game reference to sgfco2numco because it needs the FF and SZ properties.\n";
    }

    if( $co =~ m/\w{2}\:\w{2}/ ) {
        croak "Parsed Stupidly: SGF2misc.pm doesn't handle compressed co-ordinates ($co) yet... *sigh*\n";
    }

    my $inty = sub {
        my $x = -1;

        $x = int(hex(unpack("H*", $_[0]))) - 97 if $_[0] =~ m/[a-z]/;
        $x = int(hex(unpack("H*", $_[0]))) - 65 if $_[0] =~ m/[A-Z]/;

        die "unexpected error reading column identifier" unless $x > -1;

        return $x;
    };

    if( not $co or ($co eq "tt" and ($ff == 3 or $sz<=19)) ) {
        return (wantarray ? (qw(PASS PASS)) : [qw(PASS PASS)]);
    }

    if( $co =~ m/^([a-zA-Z])([a-zA-Z])$/ ) {
        my ($row, $col) = ($1, $2);

        return (wantarray ? ($inty->($col), $inty->($row)) : [ $inty->($col), $inty->($row) ]);
    }

    croak "Parse Error: co-ordinate not understood ($co)\n";
}
# }}}

# outputers
# parse_hash {{{
sub parse_hash {
    my $this = shift;

    return $this->{parse};
}
# }}}
# nodelist {{{
sub nodelist {
    my $this  = shift;

    return $this->{nodelist};
}
# }}}
# is_node {{{
sub is_node {
    my $this = shift;
    my $node = shift;

    return ($this->{refdb}{$node} ? 1:0);
}
# }}}
# as_perl {{{
sub as_perl {
    my $this = shift;
    my $node = shift;
    my $soft = shift;

    if( $node ) {
        if( my $ref = $this->{refdb}{$node} ) {
            return $ref;
        }
    }

    $this->{error} = "no such node: $node";
    return 0 if $soft;

    croak $this->{error};
}
# }}}
# as_text {{{
sub as_text {
    my $this = shift;
    my $node = shift;

    $node = $this->as_perl( $node, 1 ) or croak $this->errstr;

    my $board = $node->{board};

    my $x = "";
    for my $i (0..$#{ $board }) {
        for my $j (0..$#{ $board->[$i] }) {
            $x .= " " . { ' '=>'.', 'W'=>'O', 'B'=>'X' }->{$board->[$i][$j]};
        }
        $x .= "\n";
    }

    return $x;
}
# }}}
# as_html {{{
sub as_html {
    my $this = shift;
    my $node = shift;
    my $dir  = shift;
       $dir  = "./img" unless $dir;

    $node = $this->as_perl( $node, 1 ) or croak $this->errstr;

    my $board = $node->{board};
    my $size  = @{$board->[0]}; # inaccurate?

    my $hoshi = {};
    if( $size == 19 ) {
        $hoshi = { "3 3" => 1, "3 15" => 1, "15 3" => 1, "15 15" => 1,
            "9 3" => 1, "9 15" => 1, "3 9" => 1, "15 9" => 1, "9 9" => 1, };
    } elsif( $size == 13 ) {
        $hoshi = { "3 3" => 1, "9 9" => 1, "3 9" => 1, "9 3" => 1,
            "6 3" => 1, "3 6" => 1, "9 6" => 1, "6 9" => 1, "6 6" => 1, };
    } elsif( $size == 9 ) {
        $hoshi = { "2 2" => 1, "2 6" => 1, "6 6" => 1, "6 2" => 1, "4 4" => 1, };
    }

    $size--;

    my $crazy_moku_alg = sub { my ($i, $j) = @_;
        return "ulc.gif" if $i == 0     and $j == 0;
        return "urc.gif" if $i == 0     and $j == $size;
        return "llc.gif" if $i == $size and $j == 0;
        return "lrc.gif" if $i == $size and $j == $size;
        return "ts.gif"  if $i == 0     and $j != 0 and $j != $size;
        return "bs.gif"  if $i == $size and $j != 0 and $j != $size;
        return "ls.gif"  if $j == 0     and $i != 0 and $i != $size;
        return "rs.gif"  if $j == $size and $i != 0 and $i != $size;

        return "h.gif" if $hoshi->{"$i $j"};
        return "p.gif",
    };

    my %marks = ();
    for my $m (@{ $node->{marks} }) {
        $marks{"$m->[1] $m->[2]"} = ($m->[0] eq "LB" ? $m->[4] : $m->[0]);
    }

    my $mark_alg = sub { 
        my ($mark, $img) = @_;

        return "bt.gif" if $mark eq "TR" and $img eq "b.gif";
        return "wt.gif" if $mark eq "TR" and $img eq "w.gif";
        return "bc.gif" if $mark eq "CR" and $img eq "b.gif";
        return "wc.gif" if $mark eq "CR" and $img eq "w.gif";
        return "bq.gif" if $mark eq "SQ" and $img eq "b.gif";
        return "wq.gif" if $mark eq "SQ" and $img eq "w.gif";

        if( ($mark = int($mark)) > 0 and $mark <= 100 ) {
            return "b$mark.gif" if $img eq "b.gif";
            return "w$mark.gif"
        }

        return $img;
    };

    my $x = "<table cellpadding=0 cellspacing=0>\n";
    for my $i (0..$#{ $board }) {
        $x .= "<tr>";
        for my $j (0..$#{ $board->[$i] }) {

            my $c = { 
                'B' => "b.gif",
                'W' => "w.gif",
            }->{$board->[$i][$j]};

            $c = $crazy_moku_alg->($i, $j) unless $c;
            $c = $mark_alg->($marks{"$i $j"}, $c);

            $c  = "$dir/$c";
            $x .= "<td><img src=\"$c\" width=19 height=19></td> ";
        }
        $x .= "</tr>\n";
    }

    return "$x</table>";
}
# }}}
# as_image {{{
sub as_image {
    my $this = shift;
    my $node = shift; my $nm = $node;
    my $argu = shift;
    my %opts = (imagesize=>256, antialias=>0);

    $node = $this->as_perl( $node, 1 ) or croak $this->errstr;

    my $board = $node->{board};
    my $size  = @{$board->[0]}; # inaccurate?

    if( ref($argu) ne "HASH" ) {
        croak 
        "as_image() takes a hashref argument... e.g., {imagesize=>256, etc=>1} or nothing at all.";
    }

    my $package = $argu->{'use'} || 'Games::Go::SGF2misc::GD';
    if ($package =~ /svg/i) {
        $opts{'imagesize'} = '256px';
    }

    @opts{keys %$argu}  = (values %$argu);
    $opts{boardsize}    = $size;
    $opts{filename}     = "$nm.png" unless $opts{filename};

    my $image;
    eval qq( use $package; \$image = $package->new(%opts); );

    $image->drawGoban();

    # draw moves
    for my $i (0..$#{ $board }) {
        for my $j (0..$#{ $board->[$i] }) {
            if( $board->[$i][$j] =~ m/([WB])/ ) {
                if( $ENV{DEBUG} > 0 ) {
                    print STDERR "placeStone($1, [$i, $j])\n";
                }

                # SGFs are $y, $x, the matrix is $x, $y ...
                $image->placeStone(lc($1), [reverse( $i, $j )]);  
            }
        }
    }

    my $marks = 0;
    # draw marks
    for my $m (@{ $node->{marks} }) {
        $image->addCircle($m->[3])   if $m->[0] eq "CR";
        $image->addSquare($m->[3])   if $m->[0] eq "SQ";
        $image->addTriangle($m->[3]) if $m->[0] eq "TR";

        $image->addLetter($m->[3], 'X', "./times.ttf") if $m->[0] eq "MA";
        $image->addLetter($m->[3], $m->[4], "./times.ttf") if $m->[0] eq "LB";
        $marks++;
    }

    if ($argu->{'automark'}) {
        unless ($marks) {
            my $moves = $node->{moves};
            foreach my $m (@$moves) {
                $image->addCircle($m->[3]) unless $m->[3];
            }
        }
    }

    if ($package =~ /svg/i) {
        if( $opts{filename} =~ m/.png$/ ) {
            $image->export($opts{'filename'});
        } else {
            $image->save($opts{filename});
        }
    } else {
        if( $opts{filename} =~ m/^\-\.(\w+)$/ ) {
            return $image->dump($1);
        }

        $image->save($opts{filename});
    }
}
# }}}
# as_freezerbag {{{
sub as_freezerbag {
    my $this = shift;
    my $file = shift or croak "You must name your freezerbag.";
    my $code = shift;
       $code = "# your code here\n" unless $code;
    my $perl = shift;

    if( not $perl ) {
        for my $try (qw{ /usr/bin/perl /usr/local/bin/perl }) {
            $perl = $try if -x $try;
        }
        croak "couldn't find perl" unless -x $perl;
    }
    
    open  OUTMF, ">$file" or croak "Couldn't open freezerbag ($file) for output: $!";
    print OUTMF "#!$perl\n# vi:fdm=marker fdl=0:\n\nuse strict;\nno warnings;\nuse Games::Go::SGF2misc;\n\n";
    print OUTMF "my \$sgf = new Games::Go::SGF2misc;\n";
    print OUTMF "   \$sgf->thaw(\\*DATA);\n\n$code\n\n# freezer DATA {\{\{\n__DATA__\n";

    $this->_time("print freeze");
    print OUTMF $this->freeze;
    $this->_time("print freeze");

    close OUTMF;

    $this->_show_timings if $ENV{DEBUG} > 0;
}
# }}}

# internals
# _show_timings {{{
sub _show_timings {
    my $this = shift;

    my @times = ();
    for my $k (keys %{ $this->{Time} }) {
        my $x   = $this->{Time}{$k}{diffs};  next unless ref($x) eq "ARRAY";
        my $n   = int @$x;
        my $sum = 0;
           $sum += $_ for @$x;

        push @times, [ $k, $sum, $n, ($sum/$n) ];
    }

    for my $x (sort {$b->[1] <=> $a->[1]} @times) {
        printf('%-35s: sum=%3.4fs cardinality=%5d avg=%3.2fs%s', @$x, "\n");
    }

    delete $this->{Time};
}
# }}}
# _time {{{
sub _time {
    return unless $ENV{DEBUG} > 0;

    my $this = shift; 
    my $tag  = shift;

    if( $ENV{DEBUG} == 1.2 ) {
        my @a;

        for (sort keys %{ $this->{Time} }) {
           push @a, $_ if $this->{Time}{$_}{start};
        }

        print STDERR "clocks: @a\n";
    }

    if( defined $this->{Time}{$tag}{start} ) {
        push @{ $this->{Time}{$tag}{diffs} }, (time - $this->{Time}{$tag}{start});
        delete $this->{Time}{$tag}{start};
    } else {
        $this->{Time}{$tag}{start} = time;
    }
}
# }}}
# _nodelist {{{
sub _nodelist {
    my $this = shift;
    my $list = shift;
    my $cur  = shift;

    # $this->{nodelist} = { map {$this->_ref2id($_) => $this->_nodelist([], $_)} @{$this->{gametree}} };

    for my $kid (@{ $cur->{kids} }) {
        my $id = $this->_ref2id( $kid );

        die "problem parsing node id" unless $id =~ m/(\d+)\.(\d+)\-(.+)/;

        my ($g, $v, $m) = ($1, $2, $3);

        if( $v > @{ $list } ) {
            my $x = [];
            push @$list, $x;
            for (1..$m) {
                push @$x, undef;
            }
        }

        push @{ $list->[$v-1] }, $id;

        $this->_nodelist($list, $kid);
    }

    return $list;
}
# }}}
# _parse (aka, the internal parse) {{{
sub _parse {
    my $this   = shift;
    my $level  = shift;
    my $pref   = shift;
    my $gref   = shift;
    my $parent = shift;

    if( $ENV{DEBUG} > 1 ) {
        print STDERR "\t_parse($level)";
        print STDERR " ... variation = $gref->{variations} " if ref($gref) and defined $gref->{variations};
        print STDERR "\n";
    }

    my $gm_pr_reg = qr{^(?:GM|SZ|CA|AP|RU|KM|HA|FF|PW|PB|RE|TM|OT|BR|WR|DT|PC|AN|BT|CP|EV|GN|GC|ON|RO|SO|US)$};

    if( $level == 0 ) { 
        # The file level... $gref is most certainly undefined...
        # We're also starting the gametree from scratch here

        $this->{gametree} = [];

        if( int(@{ $this->{parse}{n} }) ) {
            $this->{error} = "Parse Error: nodes found at top level... very strange.";
            return 0;
        }

        for my $c (@{ $this->{parse}{c} }) {
            $this->_parse($level+1, $c, undef) or return 0;
        }

        return 1;

    } elsif( $level == 1 ) { 
        # Every collection should be a new game
        # At this $level, all we do is make a game and look for game properties.
        # Then we re _parse() at our current position

        $gref = { variations=>1, kids=>[] }; push @{ $this->{gametree} }, $gref;
        $gref->{gnum} = int @{ $this->{gametree} };

        my $pnode = $pref->{n}[0];
        for my $p (@$pnode) {
            if( $p->{P} =~ m/$gm_pr_reg/ ) {
                $gref->{game_properties}{$p->{P}} = $p->{V};
            }
        }

        unless( $gref->{game_properties}{GM} == 1 ) {
            $this->{error} = "Parse Error: Need GM[1] property in the first node of the game... not found.";
            return 0;
        }

        unless( $gref->{game_properties}{FF} == 3 or $gref->{game_properties}{FF} == 4 ) {
            unless( $ENV{ALLOW_STRANGE_FFs} ) {
                $this->{error} = "Parse Error: Need FF[3] or FF[4] property in the first node of the game... not found.";
                return 0;
            }
        }

        if( $gref->{game_properties}{SZ} < 3 ) {
            $this->{error} = "Parse Error: SZ must be set and be greater than 2 (SZ was $gref->{game_properties}{SZ})";
            return 0;
        }

        if( $gref->{game_properties}{FF} == 3 and $gref->{game_properties}{SZ} > 19 ) {
            $this->{error} = "Parse Error: In FF[3] a move of B[tt] is a pass and therefore, SZ must be less than 20 " .
                "(SZ was $gref->{game_properties}{SZ}).";
            return 0;
        }

        if( $gref->{game_properties}{FF} == 4 and $gref->{game_properties}{SZ} > 52 ) {
            $this->{error} = "Parse Error: In FF[4] the size of the board must be no greater than 52" .
                "(SZ was $gref->{game_properties}{SZ})";
            return 0;
        }

        $this->_parse($level+1, $pref, $gref) or return 0;

        return 1;

    } elsif( defined $gref ) { 
        # OK, now we're getting into some serious parsing.

        my $gnode;  # this has the effect of forking the variations off the last node in the collection.
                    # is that correct?

        for my $i (0..$#{ $pref->{n} }) {
            my $pnode = $pref->{n}[$i];

            $parent = ($gnode ? $gnode : $parent ? $parent : $gref);

            $gnode = { variation=>$gref->{variations}, kids=>[] };
            push @{ $parent->{kids} }, $gnode;

            $gnode->{board} = $this->_copy_board_matrix( $parent->{board} ) if $parent->{board};
            $gnode->{board} = $this->_new_board_matrix( $gref ) unless $gnode->{board};

            $gnode->{captures} = { B=>0, W=>0 };
            if( ref($parent) and ref(my $pc = $parent->{captures}) ) {
                $gnode->{captures}{B} += $pc->{B};
                $gnode->{captures}{W} += $pc->{W};
            }

            for my $p (@$pnode) {
                if( $p->{P} =~ m/^([BW])$/) {
                    my $c = $1;
                    my @c = $this->sgfco2numco($gref, $p->{V});

                    print STDERR "\t\tmove: $c($p->{V}) == [@c]\n" if $ENV{DEBUG} >= 4;

                    push @{ $gnode->{moves} }, [ $c, @c, $p->{V} ];

                    unless( $c[0] eq "PASS" ) {
                        # fix up board
                        $gnode->{board}[$c[0]][$c[1]] = $c;

                        # check for captures
                        $this->_check_for_captures($gref->{game_properties}{SZ}, $gnode, @c );
                    }

                } elsif( $p->{P} =~ m/^A([WBE])$/ ) {
                    my $c = $1;
                    my @c = $this->sgfco2numco($gref, $p->{V});

                    push @{ $gnode->{edits} }, [ $c, @c, $p->{V} ];

                    # fix up board
                    # do NOT check for captures
                    if( $c eq "E" ) {
                        $gnode->{board}[$c[0]][$c[1]] = ' ';
                    } else {
                        $gnode->{board}[$c[0]][$c[1]] = $c;
                    }
                } elsif( $p->{P} =~ m/^C$/ ) {
                    push @{ $gnode->{comments} }, $p->{V};

                } elsif( $p->{P} =~ m/^(?:CR|TR|SQ)$/ ) {
                    my @c = $this->sgfco2numco($gref, $p->{V});

                    push @{ $gnode->{marks} }, [ $p->{P}, @c, $p->{V} ];

                    # It's tempting to put the marks ON THE BOARD Do not do
                    # this.  They'd need to get handled in _copy, and also,
                    # whosoever get's the $board out of the $gnode, can
                    # also get the $marks!

                } elsif( $p->{P} =~ m/^(?:LB)$/ and $p->{V} =~ m/^(..)\:(.+)$/ ) {
                    push @{ $gnode->{marks} }, [ "LB", $this->sgfco2numco($gref, $1), $1, $2 ];

                } elsif( not $p->{P} =~ m/$gm_pr_reg/ ) {
                    push @{ $gnode->{other}{$p->{P}} }, $p->{V};
                }
            }

            $gnode->{gnum}    = $parent->{gnum};
            $gnode->{move_no} = 
                  (ref($gnode->{moves}) ? int(@{ $gnode->{moves} }) : 0)
                + (ref($parent) and defined $parent->{move_no} ? $parent->{move_no} : 0);
        }

        my $j = @{ $pref->{c} };
        if( $j > 1 ) {
            # pretend we're in the node with move #12
             
            # The first fork is still this variation, and contains move #13
            $this->_parse($level+1, $pref->{c}[0], $gref, $gnode) or return 0;

            # Every other fork is an alternate move #13
            for my $i (1..$#{ $pref->{c} }) {
                $gref->{variations}++;
                $this->_parse($level+1, $pref->{c}[$i], $gref, $gnode) or return 0;
            }
        } elsif( $j == 1 ) {
            $this->{error} = "Parse Error: the author didn't think this condition could come up ... ";
            return 0;
        }

        return 1;
    }

    $this->{error} = "Parse Error: unknown parse depth ($level) or broken reference(s) ($pref, $gref)... error unknown";
    return 0;
}
# }}}
# _ref2id {{{
sub _ref2id {
    my $this = shift;
    my $ref  = shift;

    croak "invalid ref given to _ref2id()" unless ref($ref) eq "HASH";

    unless( defined $this->{refdb2}{$ref} ) {
        my $id;
        my $c = 2;
        if( defined($ref->{variation}) and defined($ref->{move_no}) ) {
            $id = "$ref->{gnum}." . 
                   $ref->{variation} . "-" . ($ref->{move_no} ? $ref->{move_no} : "root");
            my $cur = $id;
            while( defined $this->{refdb}{$cur} ) {
                $cur = $id . "-" . $c++;
            }
            $id = $cur;
        } else {
            $id = ++$this->{games};
        }

        print STDERR "$ref 2 id: $id\n" if $ENV{DEBUG} >= 10;

        $this->{refdb2}{$ref} = $id;

        for my $k (qw(comments board marks moves other captures game_properties variations)) {
            $this->{refdb}{$id}{$k} = $ref->{$k} if defined $ref->{$k};
        }

        for my $k (qw(gnum kids)) {
            delete $this->{refdb}{$id}{$k};
        }

        if( $ENV{DEBUG} > 20 ) {
            print STDERR "\$this\->\{refdb\}\{\$ref($ref)\} = $this->{refdb2}{$ref} ",
                        "/ \$this\-\>\{refdb\}\{\$id($id)\} = $this->{refdb}{$id}\n";
        }
    }

    return $this->{refdb2}{$ref};
}
# }}}
# _new_board_matrix {{{
sub _new_board_matrix {
    my $this = shift;
    my $gref = shift;

    $this->_time("_new_board_matrix");

    my $board = [];

    my $size = $gref->{game_properties}{SZ};
    croak "Syntax Error: You must pass a game reference to sgfco2numco because it needs the FF and SZ properties.\n" unless $size;

    for my $i (1..$size) {
        my $row = [];
        for my $j (1..$size) {
            push @$row, ' ';
        }
        push @$board, $row;
    }

    $this->_time("_new_board_matrix");

    return $board;
}
# }}}
# _copy_board_matrix {{{
sub _copy_board_matrix {
    my $this = shift;
    my $tocp = shift;

    $this->_time("_copy_board_matrix");

    my $board = [];

    my $double_check = int @$tocp;
    for (@$tocp) {
        my @a = @{ $_ }; 
        push @$board, \@a;

        die "Problem copying board (" . (int @a) . " vs $double_check)!" unless int @a == $double_check;
    }

    $this->_time("_copy_board_matrix");

    return $board;
}
# }}}

# _check_for_captures {{{
sub _check_for_captures {
    my ($this, $SZ, $node, @p) = @_;
    my $board = $node->{board};
    my $caps  = $node->{captures};

    $this->_time("_check_for_captures");

    my $tc = $board->[$p[0]][$p[1]];

    croak "crazy unexpected error: checking for caps, and current pos doesn't have a stone.  Two times double odd, and fatal" 
        unless $tc =~ m/^[WB]$/;

    my $oc = ($tc eq "W" ? "B" : "W");

    # 1. Find groups for all adjacent stones.  

    $this->_time("for(_find_group)");

    my %checked = ();
    my @groups  = ();
    for my $p ( [$p[0]-1, $p[1]+0], [$p[0]+1, $p[1]+0], [$p[0]+0, $p[1]-1], [$p[0]+0, $p[1]+1] ) {
        my @g = $this->_find_group( \%checked, $SZ, $oc, $board, @$p );

        push @groups, [ @g ] if @g;
    }

    $this->_time("for(_find_group)");
    $this->_time("for(\@groups), _count_liberties");

    if( @groups ) {
        # 2. Any groups without liberties are toast!
        print STDERR "_check_for_captures() found ", int(@groups), " neighboring groups:" if $ENV{DEBUG} > 3 and int(@groups);

        for my $group (@groups) {
            my $l = $this->_count_liberties( $SZ, $board, @$group );

            print STDERR " liberties($l)" if $ENV{DEBUG}>3;
            if( $l < 1 ) {
                print STDERR "-killed! " if $ENV{DEBUG}>3;
                for my $p (@$group) {
                    $caps->{$tc}++;
                    $board->[$p->[0]][$p->[1]] = ' ';
                }
            }
        }

        print STDERR "\n" if $ENV{DEBUG} > 3;
    }

    $this->_time("for(\@groups), _count_liberties");
    $this->_time("_find_group/_count_liberties of me");

    # 3. Check my own liberties, I may be toast
    %checked = ();
    my @me_group = $this->_find_group( \%checked, $SZ, $tc, $board, @p );
    my $me_lifec = $this->_count_liberties( $SZ, $board, @me_group );
    print STDERR "_check_for_captures() me_group ", int(@me_group), " stones: " if $ENV{DEBUG} > 3;
    print STDERR " me liberties($me_lifec)" if $ENV{DEBUG}>3;
    if( $me_lifec < 1 ) {
        print STDERR "-killed! " if $ENV{DEBUG}>3;
        for my $p (@me_group) {
            $caps->{$oc}++;
            $board->[$p->[0]][$p->[1]] = ' ';
        }
    }
    print STDERR "\n" if $ENV{DEBUG}>3;

    $this->_time("_find_group/_count_liberties of me");
    $this->_time("_check_for_captures");
}
# }}}
# _count_liberties {{{
sub _count_liberties {
    my ($this, $SZ, $board, @group) = @_;

    $this->_time("_count_liberties");

    my %checked = ();
    my $count   = 0;

    for my $g (@group) {
        for my $p ( [$g->[0]-1, $g->[1]+0], [$g->[0]+1, $g->[1]+0], [$g->[0]+0, $g->[1]-1], [$g->[0]+0, $g->[1]+1] ) {
            if( not $checked{"@$p"} ) {
                $checked{"@$p"} = 1;
                unless( ($p->[0] < 0 or $p->[0] > ($SZ-1)) or ($p->[1] < 0 or $p->[1] > ($SZ-1)) ) {
                    if( $board->[$p->[0]][$p->[1]] eq ' ' ) {
                        $count++;
                    }
                }
            }
        }
    }

    $this->_time("_count_liberties");

    return $count;
}
# }}}
# _find_group {{{
sub _find_group {
    my ($this, $checked, $SZ, $oc, $board, @p) = @_;

    $this->_time("_find_group");

    print STDERR "\t_find_group(@p)" if $ENV{DEBUG}>12;
    my @g;

    if( not $checked->{"@p"} ) {
        $checked->{"@p"} = 1;
        print STDERR "." if $ENV{DEBUG}>12;
        unless( ($p[0] < 0 or $p[0] > ($SZ-1)) or ($p[1] < 0 or $p[1] > ($SZ-1)) ) {
            print STDERR ".." if $ENV{DEBUG}>12;
            if( $board->[$p[0]][$p[1]] eq $oc ) {
                print STDERR " !" if $ENV{DEBUG}>12;
                push @g, [ @p ];
                for my $p ( [$p[0]-1, $p[1]+0], [$p[0]+1, $p[1]+0], [$p[0]+0, $p[1]-1], [$p[0]+0, $p[1]+1] ) {
                    push @g, $this->_find_group( $checked, $SZ, $oc, $board, @$p );
                }
            }
        }
    }
    print STDERR "\n" if $ENV{DEBUG}>12;

    $this->_time("_find_group");

    return @g;
}
# }}}

__END__
# Below is stub documentation for your module. You better edit it!

=head1 NAME

    Games::Go::SGF2misc - Reads SGF files and produces usable output in many formats

=head1 SYNOPSIS

    use Games::Go::SGF2misc;

    my $sgf = new Games::Go::SGF2misc;
    for my $file (<*.sgf>) {
        $sgf->parse($file);

        # do things with the parsed sgf:
        # the output options are listed below
    }

=head1 nodelist

    my $nodelist = $sgf->nodelist;  

    # This returns a special list of node-id's (human readable
    # $game_number.$variation-$move_no node descriptors).  example:

    $nodelist = { 1 => [
        [ '1.1-root', '1.1-1', '1.1-2', '1.1-3', '1.1-4', '1.1-5', ],
        [      undef,   undef,   undef,   undef,   undef, '1.2-5', '1.2-6', '1.2-7', ],
        [      undef,   undef,   undef,   undef,   undef,   undef,   undef, '1.3-7', '1.3-8' ],
    ] };

    # What do you do with them?

=head1 as_perl

    my $game_info = $sgf->as_perl(1);
    my $root_node = $sgf->as_perl('1.1-root');
    my $move5_v2  = $sgf->as_perl('1.2-5');

    my $s = $game_info->{game_properties}{SZ};
    print "The board size is: ${s}x${s}!\n"; 

    print my $c (@{ $move5_v2->{comments} }) {
        print "Comment from 1.2-5: $c\n";
    }

    # Tada!!

    # Oh, as_perl takes an optional second argument.
    
    $sgf->as_perl("doesn't exist node"); # will die on the spot...
    $sgf->as_perl("isn't there", 1)      # will not
        or print "yikes!!: " . $sgf->errstr;

=head1 as_text 

    # This is pretty much just an example

    use strict;
    use Games::Go::SGF2misc;

    my $sgf = new Games::Go::SGF2misc; 
       $sgf->parse("sgf/jettero-sixrusses-2004-03-18.sgf");

    my $nl  = $sgf->nodelist;
    my $end = $nl->{1}[0][ $#{$nl->{1}[0]} ];
              # 1st game  1st variation    last node
     
    my $caps = $sgf->as_perl( $end )->{captures};

    print $sgf->as_text($end), "Captures:  Black-$caps->{B} / White-$caps->{W}\n";

    # Result:
    #   X O . . . O . . . . . . . . . O O O .
    #   . O O . O O X . X X O O X O O O X X X
    #   X O . . . O X . X O . O O . O X . . .
    #   O O . O O X O O O O O . . O X . X . .
    #   O O O O O X X X X O . O O X . . . . .
    #   O X X X O X . X . X O O X . X . . . .
    #   X X X O O . X . X . X X . . . . . . .
    #   X . . X O . . X . . . . . . . . X . .
    #   X . X X O O O X . . . . . . . . . . .
    #   X . X . X X O X O O O X . . . . . . .
    #   . X . . . X O O X X X X X . . . X . .
    #   . . . . X O O . O O O X . . . X X X X
    #   X X X . X X O O . O X X X . . X O O X
    #   X O O X . . X . O O O X X . X O O . O
    #   O O . O X X X X X O X X O X X X O . .
    #   . . O O O X . . X O O X O X O X O X .
    #   . . . . O O X X O O . O O O O O O X .
    #   . . . . . O X O . O . . . . O X X O O
    #   . . . . . O X O O . . . . . . . . . .
    #   Captures:  Black-11 / White-16

=head1 as_html 

    # This function works very much like the as_text function above, but
    # instead prints out an html table full of images.

    open OUT, ">example.html" or die $!;
    print OUT, $sgf->as_html( $end, "/image/dir/" );

    # The only real difference is the image-dir argument (which defaults to
    # "./img").  This is the URL image dir (<img # src="$image_dir/moku.gif">), 
    # not the pysical image dir.  There is a directory of images included
    # with SGF2misc.  They are from this page:

    # http://www.britgo.org/gopcres/gopcres1.html#ag-gifs

    # They are Andrew Grant's GIF images.  I did NOT seek permission to
    # re-distribute them.  Perhaps I have no right to do so.  I really
    # don't know how to get ahold of him.  

    # If anyone knows how who to ask, please tell me.  If anyone knows it's
    # a problem, please tell me.

    # 3/22/04, Orien Vandenbergh made bc.gif, wc.gif, bq.gif and wq.gif for me.

    # NOTE: On marks, this as_html only shows circles, triangles, squares,
    # and numbers where there are stones.  It does not show letters at all.
    # This is only because I don't have images for _everything_. :)

=head1 as_image

    # This uses the fantastic ::SGF2misc::GD package by Orien Vandenbergh
    # that comes with this package.  It is a separate package, and as you
    # will see, the interfaces aren't totally compatable -- however, it
    # _is_ intended to be used with this package.

    # You must install GD-2.15 (or so) in order to use it!!  You will also
    # need the bleeding edge versions of libpng and libgd.  At the time of
    # this writing, I used libgd-2.0.22 and got GD-2.12 to install and
    # function normally.

    # Here's a calling example:
    $sgf->as_image($node, {filename=>"html/$x->[$i].png", gobanColor=>[255, 255, 255]});

    # ::SGF2misc::GD takes hash-like arguments.  So, so does as_image().
    # filename=>"" and gobanColor=>[] are additions of mine, as they're
    # actually used on separate calls in the ::SGF2misc::GD package.
    # All other arguments are passed to the new() member function if
    # ::SGF2misc::GD.  Please read the Games::Go::SGF2misc::GD manpage; it
    # contains much more information.

    # Some SGF writers will automatically add a CR (circle) property to
    # the current move, which as_image will render as expected.  However
    # other SGF writers do not.  To have as_image automatically render
    # circles on the current moves add the hash argument auto_mark=>1.
    # With auto_mark enabled, any nodes which already have _any_ form of
    # markup will not receive _any_ circles on the current moves.  This
    # is to help ensure consistancy with SGFs where the markup has other
    # (more important?) uses.*This could have undesirable results for
    # nodes in which there are multiple stones placed.

    # Also, in the interests of making this as clumsy as possible, if the
    # filename is a dash followed by a type extension,

        my $image = $sgf->as_image($l, {filename=>"-.png"});

    # then the image will be returned as a string rather than written to a
    # file.  

    # Optionally, you can also install the ::SGF2misc::SVG package written
    # by Orien.  To specify use of the SVG module call as_image with the 
    # argument 'use' => 'Games::Go::SGF2misc::SVG'.  Functionally the two 
    # rendering modules are equivalent, except SVG doesn't yet support
    # returning the image as a string.  Aesthetically, however, ::SVG
    # seems to render a cleaner image (at all resolutions), and does so
    # significantly faster.

=head1 as_freezerbag
  
   # What?  Yeah, this is a special way to save the parsed sgf data such
   # that you can interact with it without having to re-parse it.  It
   # seemed like it should be faster than re-parsing the SGF every time,
   # but it isn't much of a speedup at all.  Unless I can speed it up, or
   # someone asks me to leave it in, it could get axed.  I may leave it in
   # anyway, because it's cute.

   # Try it yourself:

    $sgf->as_freezerbag( "freezer.pl",  # an export filename

        # Some code to put in it.  This argument is optional.
            q/my @nodes = @{ $sgf->nodelist->{1}[0] };

            for my $n (@nodes) {
                my $board = $sgf->as_text($n) or die $sgf->errstr; 
                print "\e[1;1f$board";
            }/,

       # The location and/or switches for your perl
       # I needed blib/lib (since I don't always install to test this
       # stuff).  This argument is optional.

       "/usr/bin/perl -I blib/lib",
    );

=head1 Board Postion Character Map

    # This is how an empty 3x3 board would be stored:

    $board = [
        [ ' ', ' ', ' ' ],
        [ ' ', ' ', ' ' ],
        [ ' ', ' ', ' ' ],
    ];

    # ' ' - an empty board position
    # 'W' - a white stone
    # 'B' - a black stone

    # Marks are not placed on the board!
    # You'll just have to fetch the marks array from the $node.

=head1 Miscellaneous

=head2 is_node

    print "There is a node called 1.1-root!\n" if $sgf->is_node('1.1-root');

=head1 BUGS

    Besides the lack of documentation?  Well, I'm sure there's a bunch.
    If you spot any bugs, please tell me.

=head1 ENV VARIABLES

=head2 DEBUG

   Try setting $ENV{DEBUG}=1, $ENV{DEBUG}=2, or $ENV{DEBUG}=3 to see the internals.

   Also, from your bash prompt you can 'DEBUG=1 perl ./myprog.pl' to
   enable debugging dynamically.

   DEBUG of 31 or over will show the lexical trace.  That's kinda fun.

=head1 AUTHOR

    Please contact me with ANY suggestions, no matter how pedantic.

    Jettero Heller <jettero@cpan.org>

    Some changes and patches provided by:

    Orien Vandenbergh <orien@icecode.com>

=head1 COPYRIGHT

    GPL!  I included a gpl.txt for your reading enjoyment.

    Though, additionally, I will say that I'll be tickled if you were to
    include this package in any commercial endeavor.  Also, any thoughts to
    the effect that using this module will somehow make your commercial
    package GPL should be washed away.

    I hereby release you from any such silly conditions.

    This package and any modifications you make to it must remain GPL.  Any
    programs you (or your company) write shall remain yours (and under
    whatever copyright you choose) even if you use this package's intended
    and/or exported interfaces in them.

=cut
