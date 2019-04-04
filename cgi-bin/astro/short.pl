#!/usr/bin/perl
#
use Astro::Api qw(:all);
use strict;
use Apache2::Request;
use Apache2::RequestRec ();
use Apache2::Const -compile => qw(:http);;

# get the Apache2::RequestRec object
# see https://perl.apache.org/docs/2.0/user/coding/coding.html#Getting_the_C__r__Object
my $r = shift;

my $req = Apache2::Request->new();

my $lat = $req->param('lat');
my $lon = $req->param('lon');
my $height = $req->param('height');
my $days = $req->param('days');
my $date = $req->param('date');

return $r->status(Apache2::Const::HTTP_BAD_REQUEST)
    unless defined($lat) && defined($lon) && $date;

# my $offset = $req->param('offset');
my ($offset) = $req->param('offset') =~ /^([ +-]?\d\d:\d\d)$/a
    or return $r->status(Apache2::Const::HTTP_BAD_REQUEST);
$offset =~ s/^( |)(?=\d)/+/; # + is escaped as space or missing, put it back

my @lines = &Astro::Api::short($lat,$lon,$height,$days,$date,$offset);

# print output
$r->content_type('text/xml');

foreach my $line (@lines) {
    print "$line\n";
}

return Apache2::Const::OK;
