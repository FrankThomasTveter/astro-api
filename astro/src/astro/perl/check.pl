#!/usr/bin/perl
#my $url = 'http://www.met.no/';
# Just an example: the URL for the most recent /Fresh Air/ show

use AstroApi qw(:all);
use DateTime;
use LWP::Simple;
use strict;
use POSIX;


# loop over time interval
my $start = DateTime->new(
    day   => 1,
    month => 5,
    year  => 2012,
    );

my $stop = DateTime->new(
    day   => 2,
    month => 5,
    year  => 2012,
    );


while ( $start->add(days => 1) <= $stop ) {
    my $date=$start->ymd('-');
    for (my $lon=-180; $lon<=180; $lon++) {	# loop over longitude
	for (my $lat=-90; $lat<=90; $lat++) {    # loop over latitude
	    printf "Date: %s", $start->ymd('-');
	    print "   Latitude: $lat    Longitude: $lon\n";
	    my $error=0;
	    # make url
###	    my $url = "http://api.met.no/weatherapi/sunrise/1.0/?lat=$lat;lon=$lon;date=$date";
	    my $url = "http://dev-vm089/weatherapi/sunrise/1.0/?lat=$lat;lon=$lon;date=$date";
	    #
	    print "URL: $url";
	    #
	    my $content = get $url;
	    #
	    if (defined $content) {
		print " (got content)\n";
	    } else {
		print " (content missing)\n";
		die "Couldn't get $url";
	    }
	    #
	    # print "$content\n";
	    #
	    # Sleep for 25 milliseconds
	    select(undef, undef, undef, 0.025);
	    

	    my $lat_web;
	    if ($content =~ m/latitude="([\d\.-]*)"/) {
		$lat_web=$1;
	    }
	    my $lon_web;
	    if ($content =~ m/longitude="([\d\.-]*)"/) {
		$lon_web=$1;
	    }
	    my @date_web;
	    if ($content =~ m/date="(\d\d\d\d)-(\d\d)-(\d\d)"/) {
		@date_web=($1,$2,$3,$4,$5,0);
	    }
	    my $sun_nr_web="undefined";
	    my $sun_ns_web="undefined";
	    my $sun_rise_web;
	    my $sun_set_web;
	    if ($content =~ m/<sun([^>]*)>/) {
		my $line=$1;
		if ($line =~ m/never_rise="(\w*)"/) {
		    $sun_nr_web=$1;
		    $sun_ns_web="false";
		}
		if ($line =~ m/never_set="(\w*)"/) {
		    $sun_nr_web="false";
		    $sun_ns_web=$1;
		}
		if ($line =~ m/set="(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z"/) {
		    $sun_set_web = DateTime->new(
			year   => $1,
			month => $2,
			day  => $3,
			hour  => $4,
			minute  => $5,
			second  => floor(0.5+$6)
			);
		    $sun_ns_web="false";
		}
		if ($line =~ m/rise="(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z"/) {
		    $sun_rise_web = DateTime->new(
			year   => $1,
			month => $2,
			day  => $3,
			hour  => $4,
			minute  => $5,
			second  => floor(0.5+$6)
			);
		    $sun_nr_web="false";
		}
		if ("$sun_nr_web"eq"undefined") {
		    print "Undefined Sun-never-rises flag. '$line' '$sun_rise_web->ymd $sun_rise_web->hms' '$sun_set_web->ymd $sun_set_web->hms'\n";
		    $error=1;
		}
	    }
	    if ("$sun_nr_web"eq"undefined") {
		print "Undefined Sun-never-rises flag.\n$content\n";
		$error=1;
	    }
	    my $moon_nr_web="undefined";
	    my $moon_ns_web="undefined";
	    my $moon_rise_web;
	    my $moon_set_web;
	    if ($content =~ m/<moon([^>]*)>/) {
		my $line=$1;
		if ($line =~ m/never_rise="(\w*)"/) {
		    $moon_nr_web=$1;
		    $moon_ns_web="false";
		}
		if ($line =~ m/never_set="(\w*)"/) {
		    $moon_nr_web="false";
		    $moon_ns_web=$1;
		}
		if ($line =~ m/set="(\d\d\d\d)-(\d\d)-(\d\d)_(\d\d)T(\d\d):(\d\d)Z"/) {
		    $moon_set_web = DateTime->new(
			year   => $1,
			month => $2,
			day  => $3,
			hour  => $4,
			minute  => $5,
			second  => floor(0.5+$6)
			);
		}
		if ($line =~ m/rise="(\d\d\d\d)-(\d\d)-(\d\d)_(\d\d)T(\d\d):(\d\d)Z"/) {
		    $moon_rise_web = DateTime->new(
			year   => $1,
			month => $2,
			day  => $3,
			hour  => $4,
			minute  => $5,
			second  => floor(0.5+$6)
			);
		}
	    }
	    #
	    # get sun astro event
	    #
	    my $tz=-floor(($lon/15.0));
	    my $current= DateTime->new(
		year   => $start->year(),
		month => $start->month(),
		day  => $start->day(),
		hour  => $start->hour(),
		minute  => $start->minute(),
		second  => $start->second()
		);
	    $current->add(hours => $tz);
	    my $j2000=DTGToJ2000($current->year(),$current->month(),$current->day(),
				 $current->hour(),$current->minute(),$current->second());
	    my $sun_rise;
	    if ($lat && $lon) {
		my ($eventId);
		my ($irc,$nrep,$rep2000,$repId,$repVal,$rep250);
		$repId=600;  # find next sun rise
		$nrep=0;		
		($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(
		   $j2000,+1,$repId,[$lat,$lon,0.],0,0); # get next event
		if ($irc) {
		    die "Error return from astroEvent.";
		}
		if ($nrep==1) {
		    my @ret=J2000ToDTG($$rep2000[0]);
		    $sun_rise = DateTime->new(
			year   => $ret[0],
			month => $ret[1],
			day  => $ret[2],
			hour  => $ret[3],
			minute  => $ret[4],
			second  => floor(0.5+$ret[5])
			);
		} 
	    }
	    my $sun_isup="undefined";
	    if ($lat && $lon) {
		my ($eventId);
		my ($irc,$nrep,$rep2000,$repId,$repVal,$rep250);
		$repId=120;  # sun initial state
		$nrep=0;		
		($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(
		    $j2000,+1,$repId,[$lat,$lon,0.],0,0); # get next event
		if ($irc) {
		    die "Error return from astroEvent.";
		}
		if ($nrep==2) {
		    if ($$repVal[0]>0) {
			$sun_isup="true";
		    } else {
			$sun_isup="false";
		    }
		} else {
		    die "Invalid number of reports: $nrep";
		}
	    }
	    my $sun_set;
	    if ($lat && $lon) {
		my ($eventId);
		my ($irc,$nrep,$rep2000,$repId,$repVal,$rep250);
		$repId=610;  # find next sun set
		$nrep=0;		
		($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(
		    $j2000,+1,$repId,[$lat,$lon,0.],0,0); # get next event
		if ($irc) {
		    die "Error return from astroEvent.";
		}
		if ($nrep==1) {
		    my @ret=J2000ToDTG($$rep2000[0]);
		    $sun_set = DateTime->new(
			year   => $ret[0],
			month => $ret[1],
			day  => $ret[2],
			hour  => $ret[3],
			minute  => $ret[4],
			second  => floor(0.5+$ret[5])
			);
		} 
	    }
	    #
	    # get moon astro event
	    #
	    my $moon_rise;
	    if ($lat && $lon) {
		my ($eventId);
		my ($irc,$nrep,$rep2000,$repId,$repVal,$rep250);
		$repId=800;  # find next moon rise
		$nrep=0;		
		($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(
		    $j2000,+1,$repId,[$lat,$lon,0.],0,0); # get next event
		if ($irc) {
		    die "Error return from astroEvent.";
		}
		if ($nrep==1) {
		    my @ret=J2000ToDTG($$rep2000[0]);
		    $moon_rise = DateTime->new(
			year   => $ret[0],
			month => $ret[1],
			day  => $ret[2],
			hour  => $ret[3],
			minute  => $ret[4],
			second  => floor(0.5+$ret[5])
			);
		} 
	    }
	    my $moon_isup="undefined";
	    if ($lat && $lon) {
		my ($eventId);
		my ($irc,$nrep,$rep2000,$repId,$repVal,$rep250);
		$repId=100;  # moon initial state
		$nrep=0;		
		($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(
		    $j2000,+1,$repId,[$lat,$lon,0.],0,0); # get next event
		if ($irc) {
		    die "Error return from astroEvent.";
		}
		if ($nrep==3) {
		    if ($$repVal[0]>0) {
			$moon_isup="true";
		    } else {
			$moon_isup="false";
		    }
		} else {
		    die "Invalid number of reports: $nrep";
		}
	    }
	    my $moon_set;
	    if ($lat && $lon) {
		my ($eventId);
		my ($irc,$nrep,$rep2000,$repId,$repVal,$rep250);
		$repId=810;  # find next moon set
		$nrep=0;		
		($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(
		    $j2000,+1,$repId,[$lat,$lon,0.],0,0); # get next event
		if ($irc) {
		    die "Error return from astroEvent.";
		}
		if ($nrep==1) {
		    my @ret=J2000ToDTG($$rep2000[0]);
		    $moon_set = DateTime->new(
			year   => $ret[0],
			month => $ret[1],
			day  => $ret[2],
			hour  => $ret[3],
			minute  => $ret[4],
			second  => floor($ret[5])
			);
		}
	    }
	    #
	    # check "sun rise time" consistency
	    #
	    if ((! $sun_rise && $sun_rise_web) || 
		( $sun_rise && ! $sun_rise_web) ) {
		print " NOT OK - Sun rise";
	    } elsif ($sun_rise && $sun_rise_web) {
		my $dt1=$sun_rise->epoch();
		my $dt2=$sun_rise_web->epoch();
		my $ddt=abs($dt1-$dt2);
		if ($ddt > 120) {
		    print " NOT OK - Sun rise ($ddt)";
		    $error=1;
		} else {
		    print "     OK - Sun rise ($ddt)";
		}
	    } else {
		print "     OK - Sun rise";
	    }
	    if ($sun_rise) {print " Astro:'$sun_rise'";}
	    if ($sun_rise_web) {print " Web:'$sun_rise_web'"};
	    print "\n";
	    #
	    # check "sun never rises" consistency
	    #
	    if ( ($sun_rise || ("$sun_isup" eq "true" && ! $sun_set)) && ("$sun_nr_web"eq"true")) {
		print " NOT OK - SNR consistency";
		$error=1;		# snr mismatch
	    } else {                    # match
		print "     OK - SNR consistency";
	    }
	    if ($sun_rise) {print ", rise='$sun_rise'";}
	    if ($sun_set) {print " set='$sun_set'";}
	    print " sunisup=$sun_isup  web-sun-never-rises='$sun_nr_web'\n";
	    #
	    # check "sun never sets" consistency
	    #
	    if ( ($sun_set || (! "$sun_isup"eq"true" && ! $sun_rise)) && ("$sun_ns_web"eq"true")) {
		print " NOT OK - SNS consistency";
		$error=1;		# snr mismatch
	    } else {                    # match
		print "     OK - SNS consistency";
	    }
	    if ($sun_rise) {print ", rise='$sun_rise'";}
	    if ($sun_set) {print " set='$sun_set'";}
	    print " sunisup=$sun_isup  web-sun-never-sets='$sun_ns_web'\n";
	    #
	    # check "sun set time" consistency
	    #
	    if ((! $sun_set && $sun_set_web) || 
		( $sun_set && ! $sun_set_web) ) {
		print " NOT OK - Sun set";
	    } elsif ($sun_set && $sun_set_web) {
		my $dt1=$sun_set->epoch();
		my $dt2=$sun_set_web->epoch();
		my $ddt=abs($dt1-$dt2);
		if ($ddt > 120) {
		    print " NOT OK - Sun set ($ddt)";
		    $error=1;
		} else {
		    print "     OK - Sun set ($ddt)";
		}
	    } else {
		print "     OK - Sun set";
	    }
	    if ($sun_set) {print " Astro:'$sun_set'";}
	    if ($sun_set_web) {print " Web:'$sun_set_web'"};
	    print "\n";
	    #
	    # check "moon rise time" consistency
	    #
	    if ((! $moon_rise && $moon_rise_web) || 
		( $moon_rise && ! $moon_rise_web) ) {
		print " NOT OK - Moon rise";
	    } elsif ($moon_rise && $moon_rise_web) {
		my $dt1=$moon_rise->epoch();
		my $dt2=$moon_rise_web->epoch();
		my $ddt=abs($dt1-$dt2);
		if ($ddt > 10*60) {
		    print " NOT OK - Moon rise ($ddt)";
		    $error=1;
		} else {
		    print "     OK - Moon rise ($ddt)";
		}
	    } else {
		print "     OK - Moon rise";
	    }
	    if ($moon_rise) {print " Astro:'$moon_rise'";}
	    if ($moon_rise_web) {print " Web:'$moon_rise_web'"};
	    print "\n";
	    #
	    # check "moon never rises" consistency
	    #
	    if ( ($moon_rise || ("$moon_isup"eq"true" && ! $moon_set)) && ("$moon_nr_web"eq"true")) {
		print " NOT OK - MNR consistency:";
		$error=1;		# snr mismatch
	    } else {                    # match
		print "     OK - MNR consistency";
	    }
	    if ($moon_rise) {print ", rise='$moon_rise'";}
	    if ($moon_set) {print " set='$moon_set'";}
	    print " moonisup=$moon_isup  web-moon-never-rises='$moon_nr_web'\n";
	    #
	    # check "moon never sets" consistency
	    #
	    if ( ($moon_set || (! "$moon_isup"eq"true" && ! $moon_rise)) && ("$moon_ns_web"eq"true")) {
		print " NOT OK - MNS consistency:";
		$error=1;		# snr mismatch
	    } else {                    # match
		print "     OK - MNS consistency";
	    }
	    if ($moon_rise) {print ", rise='$moon_rise'";}
	    if ($moon_set) {print " set='$moon_set'";}
	    print " moonisup=$moon_isup  web-moon-never-sets='$moon_ns_web'\n";
	    #
	    # check "moon set time" consistency
	    #
	    if ((! $moon_set && $moon_set_web) || 
		( $moon_set && ! $moon_set_web) ) {
		print " NOT OK - Moon set";
	    } elsif ($moon_set && $moon_set_web) {
		my $dt1=$moon_set->epoch();
		my $dt2=$moon_set_web->epoch();
		my $ddt=abs($dt1-$dt2);
		if ($ddt > 10*60) {
		    print " NOT OK - Moon set ($ddt)";
		    $error=1;
		} else {
		    print "     OK - Moon set ($ddt)";
		}
	    } else {
		print "     OK - Moon set";
	    }
	    if ($moon_set) {print " Astro:'$moon_set'";}
	    if ($moon_set_web) {print " Web:'$moon_set_web'"};
	    print "\n";
	    if ($error) { print "$content\n";}
	}
	die "Debug";
    }
}
