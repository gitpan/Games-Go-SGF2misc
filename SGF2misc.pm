# vi:fdm=marker fdl=0
# $Id: SGF2misc.pm,v 1.46 2004/03/23 14:29:59 jettero Exp $ 

package Games::Go::SGF2misc;

use strict;
no warnings;

use Carp;
use Parse::Lex;

# This is actually my major version, followed by my current CVS revision.
our $VERSION = q($Revision: 1.46 $); $VERSION =~ s/[^\.\d]//g; $VERSION =~ s/^1\./0.6./;

1;

# main calls
# new {{{
sub new {
    my $this = shift;
       $this = bless {}, $this;

    if( $ENV{DEBUG} > 0 ) {
        use Number::Format;
        use Devel::Size qw(total_size);

        $this->{frm} = new Number::Format;
    }

    return $this;
}
# }}}
# parse {{{
sub parse {
    my $this = shift;
    my $file = shift;

    my %did = ();
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

    for my $k (keys %$this) {
        delete $this->{$k} unless {frm=>1}->{$k};
    }
    $global::lex_error = undef;

    if( -f $file ) {
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

        local $/;  # Enable local "slurp" ... ie, by unsetting $/ for this local scope, it will not end lines on \n
        open SGFIN, $file or die "couldn't open $file: $!";

        Parse::Lex->trace if $ENV{DEBUG} > 30;

        my $lex = new Parse::Lex(@rules); $^W = 0; no warnings;
           $lex->from(\*SGFIN);

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

        close SGFIN;

        print STDERR "SGF Parsed!  Calling internal _parse() routine\n" if $ENV{DEBUG} > 0;
        print STDERR "\$this size (before _parse)= ", $this->{frm}->format_bytes(total_size( $this )), "\n" if $ENV{DEBUG} > 0;

        my $r = $this->_parse(0, $this->{parse});

        print STDERR "\$this size (after _parse)= ", $this->{frm}->format_bytes(total_size( $this )), "\n" if $ENV{DEBUG} > 0;

        print STDERR "rebuilding {refdb} (for ref2id/id2ref)\n" if $ENV{DEBUG} > 0;
        $this->{nodelist} = { map {$this->_ref2id($_) => $this->_node_list([], $_)} @{$this->{gametree}} };
        print STDERR "\$this size (after _node_list())= ", $this->{frm}->format_bytes(total_size( $this )), "\n" if $ENV{DEBUG} > 0;

        return $r;
    }

    $this->{error} = "Parse Error reading $file: unknown";
    return 0;
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
        my $a = -1;

        $a = int(hex(unpack("H*", $_[0]))) - 97 if $_[0] =~ m/[a-z]/;
        $a = int(hex(unpack("H*", $_[0]))) - 65 if $_[0] =~ m/[A-Z]/;

        die "unexpected error reading column identifier" unless $a > -1;

        return $a;
    };

    if( not $co or ($co eq "tt" and $ff == 3) ) {
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
# node_list {{{
sub node_list {
    my $this  = shift;

    return $this->{nodelist};
}
# }}}
# as_perl {{{
sub as_perl {
    my $this = shift;
    my $node = shift;

    if( $node ) {
        if( my $ref = $this->_id2ref( $node ) ) {
            return $ref;
        } else {
            $this->{error} = "Error: no such node($node).";
            return 0;
        }
    }

    return (wantarray ? @{$this->{gametree}} : $this->{gametree});
}
# }}}
# as_text {{{
sub as_text {
    my $this = shift;
    my $node = shift;

    if( $node =~ m/\d+\-(\d+|root)/ ) {
       $node = $this->as_perl( $node ) or return 0;
    } elsif( not ref($node) eq "HASH" ) {
        $this->{error} = "The as_functions() take only take IDs or node references as arguments, ie, not \"$node\"";
        return 0;
    }

    my $board = $node->{board};

    my $b = "";
    for my $i (0..$#{ $board }) {
        for my $j (0..$#{ $board->[$i] }) {
            $b .= " " . { ' '=>'.', 'W'=>'O', 'B'=>'X' }->{$board->[$i][$j]};
        }
        $b .= "\n";
    }

    return $b;
}
# }}}
# as_html {{{
sub as_html {
    my $this = shift;
    my $node = shift;
    my $dir  = shift;
       $dir  = "./img" unless $dir;

    if( $node =~ m/\d+\-(\d+|root)/ ) {
       $node = $this->as_perl( $node ) or return 0;
    } elsif( not ref($node) eq "HASH" ) {
        $this->{error} = "The as_functions() take only take IDs or node references as arguments";
        return 0;
    }

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
        $marks{"$m->[1] $m->[2]"} = $m->[0];
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

    my $b = "<table cellpadding=0 cellspacing=0>\n";
    for my $i (0..$#{ $board }) {
        $b .= "<tr>";
        for my $j (0..$#{ $board->[$i] }) {

            my $c = { 
                'B' => "b.gif",
                'W' => "w.gif",
            }->{$board->[$i][$j]};

            $c = $crazy_moku_alg->($i, $j) unless $c;
            $c = $mark_alg->($marks{"$i $j"}, $c);

            $c  = "$dir/$c";
            $b .= "<td><img src=\"$c\" width=19 height=19></td> ";
        }
        $b .= "</tr>\n";
    }

    return "$b</table>";
}
# }}}
# as_image {{{
sub as_image {
    my $this = shift;
    my $node = shift;
    my $argu = shift;
    my %opts = (imagesize=>256, antialias=>0);

    if( $node =~ m/\d+\-(\d+|root)/ ) {
       $node = $this->as_perl( $node ) or return 0;
    } elsif( not ref($node) eq "HASH" ) {
        croak "The as_functions() take only take IDs or node references as arguments";
    }

    my $board = $node->{board};
    my $size  = @{$board->[0]}; # inaccurate?

    if( ref($argu) eq "HASH" ) {
        @opts{keys %$argu} = (values %$argu);
    } else {
        croak "as_image() takes a hashref argument... e.g., {imagesize=>256, etc=>1} or nothing at all.";
    }

    $opts{boardsize} = $size;
    $opts{filename}  = "$node->{variation}-$node->{move_no}.png" unless $opts{filename};

    my $name = "Games::Go::SGF2misc::GD (aka as_image)";

    eval "use GD;"; 
    if( $@ ) {
        croak "You need to install GD/GD::Image (version 2.15+ or so) to use $name";
    } elsif( $GD::VERSION < 2.0 ) {
        croak "You need GD/GD::Image version 2.15+ or so to use $name";
    }

    eval "use Games::Go::SGF2misc::GD;";
    if( $@ ) {
        if( $@ =~ m/Bareword/ ) {
            croak "You need GD/GD::Image version 2.15+ or so to use $name";
        } else { croak $@ }
    }

    use Games::Go::SGF2misc::GD;
    my $image = new Games::Go::SGF2misc::GD(%opts);

    $image->gobanColor(@{ $opts{gobanColor} }) if $opts{gobanColor};
    $image->drawGoban();

    for my $i (0..$#{ $board }) {
        for my $j (0..$#{ $board->[$i] }) {
            if( $board->[$i][$j] =~ m/([WB])/ ) {
                if( $ENV{DEBUG} > 0 ) {
                    print STDERR "placeStone($1, [$i, $j])\n";
                }
                $image->placeStone(lc($1), [reverse( $i, $j )]);  # SGFs are $y, $x, my matrix is $x, $y ...
            }
        }
    }

    # draw moves
    # draw marks

    $image->save($opts{filename});
}
# }}}

# internals
# _node_list {{{
sub _node_list {
    my $this = shift;
    my $list = shift;
    my $cur  = shift;

    # $this->{nodelist} = { map {$this->_ref2id($_) => $this->_node_list([], $_)} @{$this->{gametree}} };

    for my $kid (@{ $cur->{kids} }) {
        my $id = $this->_ref2id( $kid );
        my ($var, $mov) = split /\-/, $id;

        if( $var > @{ $list } ) {
            my $b = [];
            push @$list, $b;
            for (1..$mov) {
                push @$b, undef;
            }
        }

        push @{ $list->[$var-1] }, $id;

        $this->_node_list($list, $kid);
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

    my $gm_pr_reg = qr{^(?:GM|SZ|CA|AP|RU|KM|HA|FF|PW|PB|RE)$};

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
            $this->{error} = "Parse Error: Need FF[3] or FF[4] property in the first node of the game... not found.";
            return 0;
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

            $gnode = { variation=>$gref->{variations}, parent => ($gnode ? $gnode : $parent ? $parent : $gref), kids=>[] };
            push @{ $gnode->{parent}{kids} }, $gnode;

            $gnode->{board} = $this->_copy_board_matrix( $gnode->{parent}{board} ) if $gnode->{parent}{board};
            $gnode->{board} = $this->_new_board_matrix( $gref ) unless $gnode->{board};

            $gnode->{captures} = { B=>0, W=>0 };
            if( ref($gnode->{parent}) and ref(my $pc = $gnode->{parent}{captures}) ) {
                $gnode->{captures}{B} += $pc->{B};
                $gnode->{captures}{W} += $pc->{W};
            }

            for my $p (@$pnode) {
                if( $p->{P} =~ m/^([BW])$/) {
                    my $c = $1;
                    my @c = $this->sgfco2numco($gref, $p->{V});

                    print STDERR "\t\tmove: $c($p->{V})\n" if $ENV{DEBUG} >= 4;

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
                    push @{ $gnode->{marks} }, [ $2, $this->sgfco2numco($gref, $1), $1 ];

                } elsif( not $p->{P} =~ m/$gm_pr_reg/ ) {
                    push @{ $gnode->{other}{$p->{P}} }, $p->{V};
                }
            }

            $gnode->{move_no} = 
                  (ref($gnode->{moves}) ? int(@{ $gnode->{moves} }) : 0)
                + (ref($gnode->{parent}) and defined $gnode->{parent}{move_no} ? $gnode->{parent}{move_no} : 0);
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
# _id2ref {{{
sub _id2ref {
    my $this = shift;
    my $id   = shift;

    return $this->{refdb}{$id} if defined $this->{refdb}{$id};
    return undef;
}
# }}}
# _ref2id {{{
sub _ref2id {
    my $this = shift;
    my $ref  = shift;

    unless( defined $this->{refdb}{$ref} ) {
        my $id;
        my $c = 2;
        if( defined($ref->{variation}) and defined($ref->{move_no}) ) {
            $id = $ref->{variation} . "-" . ($ref->{move_no} ? $ref->{move_no} : "root");
            my $cur = $id;
            while( defined $this->{refdb}{$cur} ) {
                $cur = $id . "-" . $c++;
            }
            $id = $cur;
        } else {
            $id = "game #" . (++$this->{games});
        }

        print STDERR "$ref 2 id: $id\n" if $ENV{DEBUG} >= 10;

        $this->{refdb}{$ref} = $id;
        $this->{refdb}{$id} = $ref;

        if( $ENV{DEBUG} > 20 ) {
            print STDERR "\$this\->\{refdb\}\{\$ref($ref)\} = $this->{refdb}{$ref} ",
                        "/ \$this\-\>\{refdb\}\{\$id($id)\} = $this->{refdb}{$id}\n";
        }
    }

    return $this->{refdb}{$ref};
}
# }}}
# _new_board_matrix {{{
sub _new_board_matrix {
    my $this = shift;
    my $gref = shift;

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

    return $board;
}
# }}}
# _copy_board_matrix {{{
sub _copy_board_matrix {
    my $this = shift;
    my $tocp = shift;

    my $board = [];

    for my $i (0..$#{ $tocp }) {
        my $row = [];
        for my $j (0..$#{ $tocp->[$i] }) {
            push @$row, $tocp->[$i][$j];
        }
        push @$board, $row;
    }

    my $max_i = $#{ $tocp };
    for my $i (0 .. $max_i) {
        if( $max_i != $#{ $tocp->[$i] } ) {
            if( $ENV{DEBUG} > 0 ) {
                use Data::Dumper;
                $Data::Dumper::Indent = 1;
                print STDERR Dumper( $board ), "\n";
            }
            die "FATAL: problem copying board";
        }
    }

    return $board;
}
# }}}

# _check_for_captures {{{
sub _check_for_captures {
    my ($this, $SZ, $node, @p) = @_;
    my $board = $node->{board};
    my $caps  = $node->{captures};

    my $tc = $board->[$p[0]][$p[1]];

    croak "crazy unexpected error: checking for caps, and current pos doesn't have a stone.  Two times double odd, and fatal" 
        unless $tc =~ m/^[WB]$/;

    my $oc = ($tc eq "W" ? "B" : "W");

    # 1. Find groups for all adjacent stones.  

    my %checked = ();
    my @groups  = ();
    for my $p ( [$p[0]-1, $p[1]+0], [$p[0]+1, $p[1]+0], [$p[0]+0, $p[1]-1], [$p[0]+0, $p[1]+1] ) {
        my @g = $this->_find_group( \%checked, $SZ, $oc, $board, @$p );

        push @groups, [ @g ] if @g;
    }

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
}
# }}}
# _count_liberties {{{
sub _count_liberties {
    my ($this, $SZ, $board, @group) = @_;

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

    return $count;
}
# }}}
# _find_group {{{
sub _find_group {
    my ($this, $checked, $SZ, $oc, $board, @p) = @_;

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

=head1 as_perl

    my @gametree = $sgf->as_perl; # smart enough to return an arrayref or array as appropriate
    my $gametree = $sgf->as_perl; # the arrayref is probably a better choice, since it's not a copy.

    # This will probably be the most useful as_ function (if you're doing
    # your own custom thing).  It returns all the nodes, beautifully
    # parsed, with the board positions as a big matrix, kibitzen as an
    # array of strings, the list of moves, the list of captures, and the
    # rules, etc.
    # 
    # Could you ask for anything more?  
    # 
    # Less memory usage perhaps?  Because of the huge amount of
    # semi-duplicate data in the tree, these take up (very roughly) around
    # a meg of ram per 300 move game.

=head2 The Layout (Pretty Much)

    # The game tree is actually an array(ref) of games.

    $gametree = [ $game1, $game2, $game3, $game4 ];

    # The games are hashes (refs) of game data with the Collections of
    # Nodes stuffed into a kids=>[] array(ref).

    $game = { variations=>6, 
        game_properties={
          'AP' => 'CGoban:2', 'FF' => 3, 'PB' => 'Myself', 'GM' => 1, 'KM' => '0', 'SZ' => 5,
          'RU' => 'Japanese', 'CA' => 'UTF-8', 'PW' => 'Me'
        },
        kids => [ $node1, $node2, $node3 ],
    };

    # And, lastly, the nodes are hashes with their own kids array.
    $node = {
        parent=>$game, # this is actually a pointer (ref) to the hashref
                       # who's kids=>[] array points to this node.

        'variation' => 1,

        'other' => { 'ST' => '2' },  # cgoban 1 and 2 kick this out.  *shrug*
                                     # the other properties are just values
                                     # SGF2misc doesn't handle

        'move_no' => 1, # The root node is move #0. 
                        # It is technically possible to have more than one
                        # move in a given node.  If so, the move counter is
                        # bumped up to count the last move of the node.

        'kids' => [ $node1, $node2, $etc ],

        # This should be pretty clear.  SGF2misc leaves the sgf
        # co-ordinates intact, but also presents some numerical ones for
        # your enjoyment.  CR is a circle, btw.  The numerical co-ordinates
        # are for the board matrix and start in the upper left corner
        # (unlike the letter-number co-ordinates we use IRL).

        'marks' => [ [ 'CR', 1, 3, 'bd' ] ],
        'moves' => [ [ 'B', 1, 3, 'bd' ] ],

        # This is from the root node of a handicap game... They're the handicap stones.
        'edits' => [ [ 'B', 15, 3, 'pd' ], [ 'B', 3, 15, 'dp' ] ],

        # The board is a matrix of descriptive characters.  This is a 3x3
        # board, and clearly, the edits from above do not fit on it.
        'board' => [ [' ', ' ', ' '], [' ', ' ', ' '], [' ', ' ', ' '] ], 

        # This speaks for itself I hope.
        'captures' => { B=>3, W=>4 },

        # I generally like to say something friendly at the start of all my games, you?
        'comments' => [ 'jettero [15k]: hi ' ],
    }

=head2 Lemme See It

    # Rather than explain the layout in detail, it's better to simply see
    # it.  WARNING:  This can be gigantically huge!  In memory, a complete
    # game is probably around a meg.  When you print them though, the
    # indenting get's out of control!

    use Data::Dumper;
    $Data::Dumper::Maxdepth = 10; # set this to 0 to see the whole show...
    $Data::Dumper::Indent   = 1;  
    $Data::Dumper::Purity   = 1 if $Data::Dumper::Maxdepth == 0;

    # Purity makes some of the crazy-refs show up at the end instead
    # crazy refs?  EG: 'parent' =>
    # $VAR1->[0]{'kids'}[0]{'kids'}[0]{'kids'}[0]{'kids'}[0]{'kids'}[0]{'kids'}[0]{'kids'}[0],

    open OUT, ">darray.pl" or die $!;
    print OUT Dumper( $gametree );
    clsoe OUT;

    # One last thing.  You can check the memory usage by using the DEBUG
    # environment variable (see below).  Anything over a 1 will show how
    # big the structures get at both parse phasen.

=head1 node_list

    my $node_list = $sgf->node_list;  

    # Parsing the game tree from as_perl() could be pretty tedius.
    # Fortunately, you don't have to.  This returns a special list
    # of node-id's (human readable $variation-$move_no node descriptors).
    # example:

    $node_list = { 'game #1' => [
        [ '1-root', '1-1', '1-2', '1-3', '1-4', '1-5', ],
        [    undef, undef, undef, undef, undef, '2-5', '2-6', '2-7', ],
        [    undef, undef, undef, undef, undef, undef, undef, '3-7', '3-8' ],
    ] };

    # As confusing as you found the as_perl() above, you'll probably like this.
    # as_perl() understands these node identifiers!

    my $game_info = $sgf->as_perl('game #1');
    my $root_node = $sgf->as_perl('1-root');
    my $move5_v2  = $sgf->as_perl('2-5');

    my $s = $game_info->{game_properties}{SZ};
    print "The board size is: ${s}x${s}!\n"; 

    print my $c (@{ $move5_v2->{comments} }) {
        print "Comment from 2-5: $c\n";
    }

    # Tada!!

=head1 as_text 

    # This is pretty much just an example

    use strict;
    use Games::Go::SGF2misc;

    my $sgf = new Games::Go::SGF2misc; 
       $sgf->parse("sgf/jettero-sixrusses-2004-03-18.sgf");

    my $nl  = $sgf->node_list;
    my $end = $nl->{'game #1'}[0][ $#{$nl->{'game #1'}[0]} ];
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
    # "./img").  There is a directory of images included with SGF2misc.
    # They are from this page:

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
    # that comes with this package.

    # You must install GD-2.15 (or so) in order to use it!!
    # You will need the latest bleeding edge versions of libpng and libgd
    # though.  At the time of this writing, I used libgd-2.0.12 and got
    # GD-2.15 to install and function normally.

    # Rather than explain myself like a normal human, I'll provide a
    # lengthy example:

    my $sgf = new Games::Go::SGF2misc;
       $sgf->parse($ARGV[0]) or warn "could not parse: " . $sgf->errstr;


    # But I'll explain a copule things.  This fetches the nodelist for the
    # first variation in the SGF.  This is not necessarilly the longest
    # variation, but it is the first variation.

    my $a = $sgf->node_list->{"game #1"}->[0];

    for my $i (0..$#{$a}) {
        open OUT, ">html/$a->[$i].html" or die $!;

        if( $i-1 >= 0 ) {
            print OUT "<li><a href=\"", $a->[$i-1], ".html\">previous</a>";
        } else { print OUT "<li> previous" }

        if( $i+1 <= $#{$a} ) {
            print OUT "<li><a href=\"", $a->[$i+1], ".html\">next</a>"
        } else { print OUT "<li> next" }


        # ::SGF2misc::GD takes hash-like arguments.  So, so does
        # as_image().  It must write to a file, and doesn't return anything
        # useful.

        # filename=>"" and gobanColor=>[] are additions of mine, as they're
        # actually used on separate calls in the ::SGF2misc::GD package.
        # All other arguments are passed to the new() member function.

        # For further information, please read the Games::Go::SGF2misc::GD
        # manpage.

        $sgf->as_image($a->[$i], {filename=>"html/$a->[$i].png", gobanColor=>[255, 255, 255]});

        print OUT "<P>";
        print OUT "<img src=\"$a->[$i].png\">";

        close OUT;
    }

=head1 parse_hash

    my $hash = $sgf->parse_hash;  

    # You'll find this highly useless.  It returns the parse tree as a perl
    # hash.  Check out as_perl() instead.

=head1 Board Postion Character Map

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

    Jettero Heller <japh@voltar-confed.org>

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
