use Test::More tests => 17;
use Time::Local qw(timegm);
BEGIN { use_ok( 'Metno::Astro::AlmanacAlgorithm', ':all'); }

my %julianDays = (
        2451545.125 => [2000, 1, 1, 15],
        2451179.5 => [1999, 1, 1, 0],
        2446822.5 => [1987, 1, 27, 0],
        2446966.0 => [1987, 6, 19, 12],
        2447187.5 => [1988, 1, 27, 0],
        2447332.625 => [1988, 6, 20, 3],
        2436116.375 => [1957, 10, 4, 21],
        2454466.846527 => [2008, 1, 1, 8, 19]
        );

foreach my $jd (keys %julianDays) {
    my @time = @{ $julianDays{$jd} };
    my $epoch = timegm($time[5] || 0, $time[4] || 0, $time[3], $time[2], $time[1] - 1, $time[0]);
    my $jdEpoch = julianDay2epoch($jd);
    is($jdEpoch, $epoch, "jd2epoch($jd)". scalar gmtime($jdEpoch));
    my $epochJd = epoch2julianDay($epoch);
    ok(abs($epochJd - $jd) < 0.0001, "epoch2jd($jd)");
}
