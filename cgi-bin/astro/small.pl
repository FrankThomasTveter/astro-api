#!/usr/bin/perl
#
use Astro::Api qw(:all);
use strict;
use Apache2::Request;
my $req = Apache2::Request->new();

my $lat = $req->param('lat');
my $lon = $req->param('lon');
my $height = $req->param('height');
my $days = $req->param('days');
my $date = $req->param('date');
# my $offset = $req->param('offset');
my ($offset) = $req->param('offset') =~ /^([ +-]?\d\d:\d\d)$/a
    or die "Error in offset: " . $req->param('offset');
$offset =~ s/^( |)(?=\d)/+/; # + is escaped as space or missing, put it back

my @lines = &Astro::Api::small($lat,$lon,$height,$days,$date,$offset);

# Print HTTP headers

print "Content-Type: text/xml;\n";
print "\n";

foreach my $line (@lines) {
    print "$line\n";
}
