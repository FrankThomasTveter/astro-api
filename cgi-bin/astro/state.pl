#!/usr/bin/perl -w
#
use lib '/astro/ubuntu/astro/src/astro/perl/blib/lib';
use Astro::Api qw(:all);
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
#
my $ref=CGI->new();
#
#while( my( $key, $value ) = each %$ref ){
#    print "     $key: $value\n";
#}
my $href=$$ref{'param'};
my $rr= ref $href;
####print "Reference: $rr\n";
my ($xml, $irc) = Astro::Api::state($href);

