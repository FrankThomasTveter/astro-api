#! /usr/bin/perl
#
use strict;
use Net::FTP;    
#
# run program
my $ cmd = "../testeph.bin";
die "\n ** Unable to initiate $cmd **\n" unless (-x $cmd);
system "cat testpo.405 | $cmd" || die "Unable to run $cmd **\n";

#open(FH,"|$cmd");
#print FH  <<EOF || die "\n ** Unable to run $cmd **\n";
#header.406
#ascp2000.406 
#EOF
#close(FH) || die "\n ** Unable to terminate $cmd **\n";
