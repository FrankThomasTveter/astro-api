#!/usr/bin/perl
#
#
use 5.006;
use Astro::Astro;
use strict;
use warnings;
use Carp qw(croak);

my @lines=Astro::Astro::pm_event(shift);
foreach my $line (@lines) {
    print "$line\n";
}

#
#
