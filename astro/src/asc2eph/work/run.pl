#! /usr/bin/perl
#
use strict;
use Net::FTP;    
#
# run program
my $ cmd = "../asc2eph.bin";
unlink("JPLEPH");
die "\n ** Unable to initiate $cmd **\n" unless (-x $cmd);
####system "cat header.406 ascp2000.406 ascp2100.406 | $cmd" || die "Unable to run $cmd **\n";
system "cat header.405 ascp2000.405 ascp2020.405 ascp2040.405 ascp2060.405 ascp2080.405  ascp2100.405  ascp2120.405 ascp2140.405  ascp2160.405  ascp2180.405  ascp2200.405 | $cmd" || die "Unable to run $cmd **\n";

#open(FH,"|$cmd");
#print FH  <<EOF || die "\n ** Unable to run $cmd **\n";
#header.406
#ascp2000.406 
#EOF
#close(FH) || die "\n ** Unable to terminate $cmd **\n";
