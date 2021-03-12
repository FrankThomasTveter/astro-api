#!/usr/bin/perl
#
use Astro::Api qw(:all);
use strict;
#
my @lines = &Astro::Api::short();
foreach my $line (@lines) {
    print "$line\n";
}
#
