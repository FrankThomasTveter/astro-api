#!/usr/bin/perl
#
use Astro::Api qw(:all);
use strict;
#
### test data for running as script...:
### $ENV{QUERY_STRING} = 'eventStart=2008-06-23T23:00:00Z;eventSearch=1;event1Id=600;event2Id=620;eventVal1=60.;eventVal2=0.;eventVal3=0.';
#
&Astro::Api::event();
#
