#!/usr/bin/perl
#
use Astro::Api qw(:all);
use strict;
use Apache2::Request;

BEGIN { $INC{"warnings.pm"}="blah" } 

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

print "Content-Type: text/json;\n";
print "\n";

my $dataXML="";
foreach my $line (@lines) {
    $dataXML=$dataXML . "$line\n";
}
local $SIG{__WARN__} = sub { };
require XML::XML2JSON;
my $XML2JSON = XML::XML2JSON->new();
my $jsonString = $XML2JSON->convert($dataXML);

# finally print json
print $jsonString . "\n";
#
