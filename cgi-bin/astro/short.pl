#!/usr/bin/perl
#
use Astro::Api qw(:all);
use strict;

use XML::LibXSLT;
use XML::LibXML;
use DateTime;
use Apache2::Request;

# sudo apt-get install libdatetime-perl libapache2-request-perl

my $maxdays = 15;
my $xsl= '/var/www/cgi-bin/astro/short.xsl';

my $req = Apache2::Request->new();

# calculate time params

my ($offset) = $req->param('offset') =~ /^([ +-]?\d\d:\d\d)$/a
    or die "Error in offset format: " . $req->param('offset');
$offset =~ s/^( |)(?=\d)/+/; # + is escaped as space or missing, put it back
my $days = $req->param('days')||1;

my $from = DateTime->new(
    year => $1,
    month => $2,
    day => $3,
    time_zone  => $offset,
) if $req->param('date') =~ /(\d\d\d\d)-(\d\d)-(\d\d)/a; # disable unicode numbers
die 'Illegal date format: ' . $req->param('date') unless $from;
$from->set_time_zone('UTC');
my $to = $from->clone->add( days => $days);

# generate astro query

my @events = (0, 100, 110, 210, 220, 230, 240, 600, 610, 620, 630, 800, 810, 820, 830, 900, 910, 920, 940);
my $query = sprintf "eventStart=%sZ&eventStop=%sZ&eventSearch=2&eventVal1=%f&eventVal2=%f&eventVal3=%f",
    $from->iso8601, $to->iso8601, $req->param('lat'), $req->param('lon'), $req->param('alt')||0;
$query .= "&eventVal4=1"; # get moonposition for each day + 1 extra (bug?)
$query .= "&event${_}Id=".$events[$_] for 1..$#events;
#print STDERR "(Sunrise) Fetching $query\n";

$ENV{'QUERY_STRING'} = $query;

# fetch XML

my @lines = &Astro::Api::event();

# Print HTTP headers (should be done in this wrapper)

print shift(@lines) . "\n";
print shift(@lines) . "\n";

# transform XML and output result

my $parser = XML::LibXML->new(no_cdata=>1);
my $source = $parser->parse_string(join "\n", @lines);
my $style_doc = $parser->parse_file($xsl);
my $stylesheet = XML::LibXSLT->new->parse_stylesheet($style_doc);

my $temp = $stylesheet->transform($source);
my $results = postproc( $temp, $offset );
print $stylesheet->output_as_bytes($results);

#
#my @lines = &Astro::Api::event();
#foreach my $line (@lines) {
#    print "$line\n";
#}
#

sub postproc { # sort XML by date and localize times
    my ($dom, $offset) = @_;
    my ($topnode) = $dom->findnodes('/astrodata/location');
    my %dates;
    for my $node ( $topnode->childNodes ) {
        next unless $node->nodeType == XML_ELEMENT_NODE;
        my @keys = qw( year month day hour minute second );
        my $utctime = $node->getAttribute('time');
        my %bits;
        @bits{@keys} = $utctime =~ /^(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z/ or next; # should not happen
        # workaround since DateTime can't parse T24:00 from astro
        my $hours = delete $bits{'hour'};
        my $localtime = DateTime->new( %bits, time_zone  => 'UTC' );
        $localtime->add( hours => $hours ); # add hours back on
        $localtime->set_time_zone($offset);
        # cut out node from doc and file by date
        $topnode->removeChild( $node )->setAttribute('time', $localtime . $offset);
        my $date = $localtime->ymd;
        if (exists $dates{$date} ) {
            push @{ $dates{$date} }, $node;
        } else {
            $dates{$date} = [ $node ];
        }
    }
    # add nodes grouped by date element
    for my $date (sort keys %dates) {
        my $datenode = $topnode->addNewChild('', 'time');
        $datenode->setAttribute('date', $date);
        $datenode->appendChild($_) for @{ $dates{$date} };
    }
    return $dom;
}
