#!/usr/bin/perl -w
use lib '/home/www/astro/src/astro/Metno-Astro-AlmanacAlgorithm/blib/lib';
use Metno::Astro::Sunrise qw(:all);
use strict;
use CGI;
use CGI::Carp 'fatalsToBrowser';
#system "touch /home/www/asdf";
print "Content-type: text/html\r\n\r\n";
print "Hello there!<br />\nJust testing .<br />\n";

for (my $i=0; $i<10; $i++)
{
print $i."<br />\n";
}

my $ref=CGI->new();
#
print "BASIC LOOP:<BR>\n";
while( my( $key, $value ) = each %$ref ){
    print "     $key: $value<BR>\n";
}

my $href=$$ref{'param'};
my $rr= ref $href;

print "PARAM LOOP:<BR>\n";
while( my( $key, $value ) = each %$href ){
    my $val = @$value[0];
    print "     $key: $value:$val<BR>\n";
}
