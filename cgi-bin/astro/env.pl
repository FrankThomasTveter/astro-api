#!/usr/bin/perl
print "Content-type: text/html\n\n";
foreach my $keys (sort keys %ENV) {
    print "$keys = $ENV{$keys}<br/>\n";
} 
