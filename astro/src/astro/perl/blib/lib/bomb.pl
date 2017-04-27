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

my $sun_set_dt=0;
my $sun_rise_dt=0;
my $moon_set_dt=0;
my $moon_rise_dt=0;
my $sun_altitude_dt=0;

while ( $start->add(days => 1) <= $stop ) {
    my $date=$start->ymd('-');
    for (my $lon=-1; $lon<=1; $lon++) {	# loop over longitude
	for (my $lat=-90; $lat<=90; $lat++) {    # loop over latitude
	    printf "Date: %s", $start->ymd('-');
	    print "   Latitude: $lat    Longitude: $lon\n";
	    my $error=0;
	    # make url
###	    my $url = "http://api.met.no/weatherapi/sunrise/1.0/?lat=$lat;lon=$lon;date=$date";
	    my $test_url = "http://dev-vm089/weatherapi/sunrise/1.0/?lat=$lat;lon=$lon;date=$date";
	    my $oper_url = "http://api.met.no/weatherapi/sunrise/1.0/?lat=$lat;lon=$lon;date=$date";
###	    my $url = "http://dev-vm089/weatherapi/sunrise/1.0/?eventId=750;eventStart=$date"."T12:00:00Z;eventSearch=0;eventVal1=$lat;eventVal2=$lon;eventVal3=0.0;eventVal4=180.0";
###	    my $url = "http://dev-vm089/weatherapi/sunrise/1.0/?eventId=110;eventStart=$date"."T12:00:00Z;eventSearch=0;eventVal1=$lat;eventVal2=$lon;eventVal3=0.0;";
	    #
	    print "TEST URL: $test_url";
	    #
	    my $test_content = get $test_url;
	    #
            my $test_sun_nr="undefined";
            my $test_sun_ns="undefined";
            my $test_sun_rise;
            my $test_sun_set;
            my $test_sun_altitude;
            my $test_moon_nr="undefined";
            my $test_moon_ns="undefined";
            my $test_moon_rise;
            my $test_moon_set;
	    if (defined $test_content) {
		if ($test_content =~ m/<sun([^>]*)>/) {
		    my $line=$1;
		    if ($line =~ m/never_rise="(\w*)"/) {
			$test_sun_nr=$1;
			$test_sun_ns="false";
		    }
		    if ($line =~ m/never_set="(\w*)"/) {
			$test_sun_nr="false";
			$test_sun_ns=$1;
		    }
		    if ($line =~ m/set="(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z"/) {
			$test_sun_set = DateTime->new(
			    year   => $1,
			    month => $2,
			    day  => $3,
			    hour  => $4,
			    minute  => $5,
			    second  => floor(0.5+$6)
			    );
			$test_sun_ns="false";
		    }
		    if ($line =~ m/rise="(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z"/) {
			$test_sun_rise = DateTime->new(
			    year   => $1,
			    month => $2,
			    day  => $3,
			    hour  => $4,
			    minute  => $5,
			    second  => floor(0.5+$6)
			    );
			$test_sun_nr="false";
		    }
		    if ("$test_sun_nr"eq"undefined") {
			print "Undefined Sun-never-rises flag. '$line' '$test_sun_rise->ymd $test_sun_rise->hms' '$test_sun_set->ymd $test_sun_set->hms'\n";
			$error=1;
		    }
		}
		if ($test_content =~ m/<noon([^>]*)>/) {
		    my $line=$1;
		    if ($line =~ m/altitude="(\w*)"/) {
			$test_sun_altitude=$1;
		    }
		}
		if ($test_content =~ m/<moon([^>]*)>/) {
		    my $line=$1;
		    if ($line =~ m/never_rise="(\w*)"/) {
			$test_moon_nr=$1;
			$test_moon_ns="false";
		    }
		    if ($line =~ m/never_set="(\w*)"/) {
			$test_moon_nr="false";
			$test_moon_ns=$1;
		    }
		    if ($line =~ m/set="(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z"/) {
			$test_moon_set = DateTime->new(
			    year   => $1,
			    month => $2,
			    day  => $3,
			    hour  => $4,
			    minute  => $5,
			    second  => floor(0.5+$6)
			    );
			$test_moon_ns="false";
		    }
		    if ($line =~ m/rise="(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z"/) {
			$test_moon_rise = DateTime->new(
			    year   => $1,
			    month => $2,
			    day  => $3,
			    hour  => $4,
			    minute  => $5,
			    second  => floor(0.5+$6)
			    );
			$test_moon_nr="false";
		    }
		    if ("$test_moon_nr"eq"undefined") {
			print "Undefined Moon-never-rises flag. '$line' '$test_moon_rise->ymd $test_moon_rise->hms' '$test_moon_set->ymd $test_moon_set->hms'\n";
			$error=1;
		    }
		}
	    } else {
		print " (content missing)\n";
		die "Couldn't get $test_url";
	    }
	    print "          $oper_url";
	    #
	    my $oper_content = $test_content; #get $oper_url;
	    #
	    my $oper_sun_nr="undefined";
	    my $oper_sun_ns="undefined";
	    my $oper_sun_rise;
	    my $oper_sun_set;
            my $oper_sun_altitude;
	    my $oper_moon_nr="undefined";
	    my $oper_moon_ns="undefined";
	    my $oper_moon_rise;
	    my $oper_moon_set;
	    if (defined $oper_content) {
		print " (got content)\n";
		if ($oper_content =~ m/<sun([^>]*)>/) {
		    my $line=$1;
		    if ($line =~ m/never_rise="(\w*)"/) {
			$oper_sun_nr=$1;
			$oper_sun_ns="false";
		    }
		    if ($line =~ m/never_set="(\w*)"/) {
			$oper_sun_nr="false";
			$oper_sun_ns=$1;
		    }
		    if ($line =~ m/set="(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z"/) {
			$oper_sun_set = DateTime->new(
			    year   => $1,
			    month => $2,
			    day  => $3,
			    hour  => $4,
			    minute  => $5,
			    second  => floor(0.5+$6)
			    );
			$oper_sun_ns="false";
		    }
		    if ($line =~ m/rise="(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z"/) {
			$oper_sun_rise = DateTime->new(
			    year   => $1,
			    month => $2,
			    day  => $3,
			    hour  => $4,
			    minute  => $5,
			    second  => floor(0.5+$6)
			    );
			$oper_sun_nr="false";
		    }
		    if ("$oper_sun_nr"eq"undefined") {
			print "Undefined Sun-never-rises flag. '$line' '$oper_sun_rise->ymd $oper_sun_rise->hms' '$oper_sun_set->ymd $oper_sun_set->hms'\n";
			$error=1;
		    }
		}
		if ($oper_content =~ m/<noon([^>]*)>/) {
		    my $line=$1;
		    if ($line =~ m/altitude="(\w*)"/) {
			$oper_sun_altitude=$1;
		    }
		}
		if ($oper_content =~ m/<moon([^>]*)>/) {
		    my $line=$1;
		    if ($line =~ m/never_rise="(\w*)"/) {
			$oper_moon_nr=$1;
			$oper_moon_ns="false";
		    }
		    if ($line =~ m/never_set="(\w*)"/) {
			$oper_moon_nr="false";
			$oper_moon_ns=$1;
		    }
		    if ($line =~ m/set="(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z"/) {
			$oper_moon_set = DateTime->new(
			    year   => $1,
			    month => $2,
			    day  => $3,
			    hour  => $4,
			    minute  => $5,
			    second  => floor(0.5+$6)
			    );
			$oper_moon_ns="false";
		    }
		    if ($line =~ m/rise="(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z"/) {
			$oper_moon_rise = DateTime->new(
			    year   => $1,
			    month => $2,
			    day  => $3,
			    hour  => $4,
			    minute  => $5,
			    second  => floor(0.5+$6)
			    );
			$oper_moon_nr="false";
		    }
		    if ("$oper_moon_nr"eq"undefined") {
			print "Undefined Moon-never-rises flag. '$line' '$oper_moon_rise->ymd $oper_moon_rise->hms' '$oper_moon_set->ymd $oper_moon_set->hms'\n";
			$error=1;
		    }
		} else {
		    print " (content missing)\n";
		    die "Couldn't get $oper_url";
		}
	    }
	    #
	    # print "$content\n";
	    #
	    # Sleep for 10 milliseconds
#	    select(undef, undef, undef, 0.010);
	    #
	    # compare contents...
	    #
            if ((! $oper_sun_rise && $test_sun_rise) || 
                ( $oper_sun_rise && ! $test_sun_rise) ) {
                print " NOT OK - Sun rise mismatch ('";
		if ($oper_sun_rise) {print "$oper_sun_rise";}
                print "'<>'";
		if ($test_sun_rise) {print "$test_sun_rise";}
		print "')\n";
		$error=1;
            } elsif ($oper_sun_rise && $test_sun_rise) {
                my $dt1=$oper_sun_rise->epoch();
                my $dt2=$test_sun_rise->epoch();
                my $ddt=abs($dt1-$dt2);
                if ($ddt > 10*60) {
                    print " NOT OK - Sun rise ($ddt)\n";
                    $error=1;
                } else {
                    print "     OK - Sun rise ($ddt)\n";
                }
		if ($sun_rise_dt < $ddt) {
		    $sun_rise_dt=$ddt;
		}
            } else {
                print "     OK - Sun rise\n";
            }
            if ((! $oper_sun_set && $test_sun_set) || 
                ( $oper_sun_set && ! $test_sun_set) ) {
                print " NOT OK - Sun set mismatch ('";
		if ($oper_sun_set) {print "$oper_sun_set";}
                print "'<>'";
		if ($test_sun_set) {print "$test_sun_set";}
		print "')\n";
		$error=1;
            } elsif ($oper_sun_set && $test_sun_set) {
                my $dt1=$oper_sun_set->epoch();
                my $dt2=$test_sun_set->epoch();
                my $ddt=abs($dt1-$dt2);
                if ($ddt > 10*60) {
                    print " NOT OK - Sun set ($ddt)\n";
                    $error=1;
                } else {
                    print "     OK - Sun set ($ddt)\n";
                }
		if ($sun_set_dt < $ddt) {
		    $sun_set_dt=$ddt;
		}
            } else {
                print "     OK - Sun set\n";
            }
            if ((! $oper_moon_rise && $test_moon_rise) || 
                ( $oper_moon_rise && ! $test_moon_rise) ) {
                print " NOT OK - Moon rise mismatch ('";
		if ($oper_moon_rise) {print "$oper_moon_rise";}
                print "'<>'";
		if ($test_moon_rise) {print "$test_moon_rise";}
		print "')\n";
		$error=1;
            } elsif ($oper_moon_rise && $test_moon_rise) {
                my $dt1=$oper_moon_rise->epoch();
                my $dt2=$test_moon_rise->epoch();
                my $ddt=abs($dt1-$dt2);
                if ($ddt > 10*60) {
                    print " NOT OK - Moon rise ($ddt)\n";
                    $error=1;
                } else {
                    print "     OK - Moon rise ($ddt)\n";
                }
		if ($moon_rise_dt < $ddt) {
		    $moon_rise_dt=$ddt;
		}
		$error=1;
            } else {
                print "     OK - Moon rise\n";
            }
            if ((! $oper_moon_set && $test_moon_set) || 
                ( $oper_moon_set && ! $test_moon_set) ) {
                print " NOT OK - Moon set mismatch ('";
		if ($oper_moon_set) {print "$oper_moon_set";}
                print "'<>'";
		if ($test_moon_set) {print "$test_moon_set";}
		print "')\n";
		$error=1;
            } elsif ($oper_moon_set && $test_moon_set) {
                my $dt1=$oper_moon_set->epoch();
                my $dt2=$test_moon_set->epoch();
                my $ddt=abs($dt1-$dt2);
                if ($ddt > 10*60) {
                    print " NOT OK - Moon set ($ddt)\n";
                    $error=1;
                } else {
                    print "     OK - Moon set ($ddt)\n";
                }
		if ($moon_set_dt < $ddt) {
		    $moon_set_dt=$ddt;
		}
		$error=1;
            } else {
                print "     OK - Moon set\n";
            }
            if ((! $oper_sun_altitude && $test_sun_altitude) || 
                ( $oper_sun_altitude && ! $test_sun_altitude) ) {
                print " NOT OK - Sun altitude mismatch ('";
		if ($oper_sun_altitude) {print "$oper_sun_altitude";}
                print "'<>'";
		if ($test_sun_altitude) {print "$test_sun_altitude";}
		print "')\n";
		$error=1;
            } elsif ($oper_sun_altitude && $test_sun_altitude) {
                my $dt1=$oper_sun_altitude;
                my $dt2=$test_sun_altitude;
                my $ddt=abs($dt1-$dt2);
                if ($ddt > 1.0) {
                    print " NOT OK - Sun altitude ($ddt)\n";
                    $error=1;
                } else {
                    print "     OK - Sun altitude ($ddt)\n";
                }
		if ($sun_altitude_dt < $ddt) {
		    $sun_altitude_dt=$ddt;
		}
		$error=1;
            } else {
                print "     OK - Sun altitude\n";
            }
	    # write debug output if any errors were detected...
            if ($error) {
		print "=====================MISMATCH===================\n";
		print "$test_content\n";
		print "$oper_content\n";
	    }
 
	    
	}

    }
}
print "Max error: $sun_rise_dt $sun_set_dt $moon_rise_dt $moon_set_dt $sun_altitude_dt\n";
die "Debug";
