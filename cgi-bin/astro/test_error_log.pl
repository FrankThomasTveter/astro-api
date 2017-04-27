#!/usr/bin/perl -w
#
# Test error log length limitations
# Result: no limitation
# 
#

use strict;
use CGI;


my $query = CGI->new;
print $query->header;

print "<html><body>You have reached "
    ."cgi-bin/admin/test_error_log.pl"
    ."</body></html>\n";
print STDERR "This is an error message from $0\n";
print STDERR "
[Above should be a blank line] here is a multiline error message
here is the third and final line of it\n";
print STDERR 'abcde' x 200;   #1,000 characters
print STDERR "\nAbove is 1,000 characters of output of 'abcde'\n";
print STDERR 'vwxyz' x 2000;  #10,000 characters
print STDERR "\nAbove is 10,000 characters of output of 'vwxyz'\n";
