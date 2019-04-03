# -*- coding: utf-8; -*-
# $Id: Astro::URL.pm,v 1.1 2012-12-11 09:42:21 franktt Exp $

package Astro::Api;

use strict;
use warnings;

use Astro::Astro qw(:all);

require Exporter;
our @ISA = ("Exporter");
our @EXPORT = qw(state short event xs_state xs_short xs_event xs_DTGToJD xs_JDToDTG astroEvent);    
our %EXPORT_TAGS = ( all => [qw(state short event xs_state xs_short xs_event xs_DTGToJD xs_JDToDTG astroEvent)]);
our $license_url;

sub config_value {
    return @_;
}

sub state {
    # reads $ENV{QUERY_STRING} directly
    return pm_state($ENV{QUERY_STRING});
}

sub short {
    return pm_short(@_);
}

sub event {
    # reads $ENV{QUERY_STRING} directly
    return pm_event($ENV{QUERY_STRING});
}

# sub used to test library...

sub astroEvent {
    ############################################
    ###### retrieve iput
    ############################################
    #
    my $tStart2000 = shift;                      # start time (in jd2000 = julianDate - 2451544.5)
    my $searchCode = shift;                      # search code; -1:previous, +1:next, 0: both, +2:until tend2000
    # Do not provide $tend2000 if $searchCode = -1, 0 or 1 !!!!!!
    my $tend2000 = ($searchCode==2 ? shift : 0); # report all events end time (in jd2000)
    my $eventId = shift;                         # requested event id (SEE TABLE BELOW)
    my $eventValr=shift;                         # array reference
    my @eventVal=@$eventValr;                    # event input data (SEE TABLE BELOW)
    my $secdec =  shift;                         # number of second decimals used in output report string 
    #
    ############################################
    ##### call WRAPPER subroutine
    ############################################
    #
    ###    @eventVal=(@eventVal,0); ### @eventVal can not be empty

    my ($irc,$nrep,$repJD,$repId,$repVal,$rep250) = 
        pm_astroEvent($tStart2000,$searchCode,$tend2000,$eventId,\@eventVal,$secdec,0);

    ############################################
    ##### return output
    ############################################
    return ($irc,$nrep,$repJD,$repId,$repVal,$rep250);
}

1;
