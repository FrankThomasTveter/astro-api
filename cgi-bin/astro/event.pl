#!/usr/bin/perl -w
#
#use lib '/astro/ubuntu/astro/src/astro/perl/blib/lib';
use Astro::Api qw(:all);
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
#
### test data for running as script...:
### $ENV{QUERY_STRING} = 'eventStart=2008-06-23T23:00:00Z;eventSearch=1;event1Id=600;event2Id=620;eventVal1=60.;eventVal2=0.;eventVal3=0.';
### $ENV{REQUEST_METHOD} = 'GET';
### $ENV{GATEWAY_INTERFACE} = 'CGI/1.1';
#
my $ref=CGI->new();
#
#while( my( $key, $value ) = each %$ref ){
#    print " ref($key)->'$value'\n";
#}
my $href=$$ref{'param'};
my $rr= ref $href;
####print "Reference: $rr\n";
my ($xml, $irc) = Astro::Api::event($href);
if ($xml) {
    print "Content-type: text/xml\r\n\r\n";
    print $xml;
} else {
    print "Error while running.\n";
}

