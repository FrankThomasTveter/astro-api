#!/usr/bin/perl -w
system "touch /home/www/asdf";
print "Content-type: text/html\r\n\r\n";
print "Hello there!<br />\nJust testing .<br />\n";

for ($i=0; $i<10; $i++)
{
print $i."<br />\n";
}
