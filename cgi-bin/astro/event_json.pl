#!/usr/bin/perl
#
use Astro::Api qw(:all);
use strict;

my @lines = &Astro::Api::event();

# Print HTTP headers

print "Content-Type: text/json;\n";
print "\n";

my $dataXML="";
foreach my $line (@lines) {
    if ( $line =~ m/^\s*</ && $line !~ m/^\s*<\?/) {
	#print "$line\n";
	$dataXML=$dataXML . "$line\n";
    } else {
	#print "Junk: $line\n";
    }
}

#print "Data:'$dataXML'\n";

local $SIG{__WARN__} = sub { };
require XML::XML2JSON;
my $XML2JSON = XML::XML2JSON->new();
my $jsonString = $XML2JSON->convert($dataXML);

# finally print json
print $jsonString . "\n";
#
