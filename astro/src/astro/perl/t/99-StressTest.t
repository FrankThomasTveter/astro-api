use Test::More;
use Time::Local qw(timegm_nocheck);
use Metno::Astro::AlmanacAlgorithm qw(:all);
use strict;
use warnings;

use constant DAY => 24 * 60 * 60;
use constant DDAY => 16 * 60 * 60;

my @places = ([59.91339, 10.7195], # Oslo
        [63.42722, 10.39847], # Trondheim
        [69.66233, 18.94189], # Tromsoe
        [66.5075, 12.03083], # Traena
        [51.47722, 0.0]); # Greenwich
my @years = (2000..2010);
my $testYear = 2008;
my @longitude = (-36..36); 
my $lonMult = 5;
my @latitude = (-11..11); # problems close to poles
my $latMult = 8;

plan tests => (scalar @places * scalar @years) + (scalar @longitude * scalar @latitude);

# test places for all days during all years
for (my $p = 0; $p < @places; $p++) {
    YEAR: foreach my $year (@years) {
        ok(testYear(@{$places[$p]}, $year, 1), "place $p, year $year");
    }
}

foreach my $lat (@latitude) {
    my $lati = $lat *$latMult;
    foreach my $lon (@longitude) {
        my $long = $lon * $lonMult;
        ok(testYear($lati, $long, $testYear, 30), "$lati,$long in $testYear");
    }
}

# test worldwide for all days 


sub testYear {
    my ($lat, $lon, $year, $dayOffset) = @_;
    $dayOffset = 1 unless $dayOffset;
    # start at noon at that place
    my $current = timegm_nocheck(0, 0, 12+($lon/180*12), 1, 0, $year);
    for (my $d = 0; $d < 365; $d+=$dayOffset) {
        my $eT;
        my ($rise, $set, $transit, $vis);
        eval {
            my $jD = epoch2julianDay($current);
            ($rise, $set, $transit, $vis) = riseSetTransit($lat, $lon, $jD, SUN);
            $eT = julianDay2epoch($transit);
        }; if ($@) {
            print STDERR "ERROR: $@\n";
            print STDERR "year: $year day: $d\n";
            return 0;
        }
        if (($current-DDAY()) < $eT and $eT < ($current+DDAY())) {
            $current += ($dayOffset * DAY);
        } else {
            print STDERR scalar gmtime($current), ": ", 12+(($lon/180)*12), "  $lon", "\n";
            print STDERR scalar gmtime($eT), "\n";
            return 0
        }
    }
    return 1;
}

