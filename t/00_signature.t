#!/usr/bin/perl
# vi:syntax=perl:
# $File: //member/autrijus/Module-Signature/t/0-signature.t $ $Author: jettero $
# $Revision: 1.1 $ $Change: 7212 $ $DateTime: 2003/07/28 14:21:21 $

use strict;
use Test::More tests => 1;

if ( -f "skip_sig.check" or -f "../skip_sig.check" ) {
    ok 1;
    exit 0;
}

SKIP: {
    if (!eval { require Module::Signature; 1 }) {
	skip("Next time around, consider install Module::Signature, ".
	     "so you can verify the integrity of this distribution.", 1);
    }
    elsif (!eval { require Socket; Socket::inet_aton('pgp.mit.edu') }) {
	skip("Cannot connect to the keyserver", 1);
    }
    else {
	ok(Module::Signature::verify() == Module::Signature::SIGNATURE_OK()
	    => "Valid signature" );
    }
}

exit 0;