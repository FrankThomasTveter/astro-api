#! /usr/bin/perl
#
use strict;
use Net::FTP;    
#
# run program
my $ cmd = "../asc2eph.bin";
unlink("JPLEPH");
die "\n ** Unable to initiate $cmd **\n" unless (-x $cmd);
system "cat header.405 ascp*.405 | $cmd" || die "Unable to run $cmd **\n";
#
