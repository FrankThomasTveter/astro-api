# -*- coding: utf-8; -*-
# $Id: Astro::URL.pm,v 1.1 2012-12-11 09:42:21 franktt Exp $

package Astro::Api;

use strict;
use warnings;

use Astro::Astro qw(:all);

use DateTime;
use XML::LibXML;
use Time::HiRes qw/gettimeofday tv_interval/;

use constant SILENT         => 0;
use constant NEVER_SET      => 10;
use constant NEVER_RISE     => 20;
use constant ERROR          => 30;

require Exporter;
our @ISA = ("Exporter");
our @EXPORT = qw(state event);    
our %EXPORT_TAGS = (
                     all => [qw(state event)],
                   );
our $license_url;

our $dbg=0;
our $dbgmsg = "";

our $default = "Unique";


sub config_value {
    return @_;
}


sub state {
    my ($params) = @_;
    my $start = DateTime->now;
    my $dtgnow = $start->strftime('%Y-%m-%dT%H:%M:%SZ');
    my %info;
    $license_url = config_value('license_url');
    $dbg  = defined $params->{'debug'};
    # foreach my $key (keys %$params) {
    # 	print "$key -> ";
    # 	foreach my $value ( @{$params->{$key}}) {
    # 	    print " $value";
    # 	}
    # 	print "\n";
    # }
    # my $keystring=join('|', keys %$params);
    # print "Found keys:".$keystring ."\n";
    # call syntax  productroot %]/1.0/?lat=60.0;lon=10.0;hgt=0.0;dtg=2008-06-23t20:00:00Z;dtg=2008-06-23t21:00:00Z;dtg=2008-06-23t22:00:00Z;
    my $lat=60.0;
    my $lon=10.0;
    my $hgt=0.0;
    my @dtg=();
    my $seqFound=0;         # only default so far
    foreach my $par (keys %$params) { # loop over parameters and create input
	if ($par =~ m/^lat$/ ) {
	    $lat=getValue($params->{$par});
	} elsif ($par =~ m/^lon$/ ) {
	    $lon=getValue($params->{$par});
	} elsif ($par =~ m/^hgt$/ ) {
	    $hgt=getValue($params->{$par});
	} elsif ($par =~ m/^dtg/ ) {
	    push(@dtg,getArray($params->{$par}));
	    # print "Sunrise.pm Found time: ".getArray($params->{$par})."\n";
	} elsif ($par =~ m/^debug$/ ) {
	} else {
	    throw(param_name => $par, message => 'Not valid astroState key');
	}
    }
    # loop over input parameter sets and create reports

    return pm_astroState($lat,$lon,$hgt,@dtg);
}
sub event {
    my ($params) = @_;
    my $start = DateTime->now;
    my $dtgnow = $start->strftime('%Y-%m-%dT%H:%M:%SZ');
    my %info;
    $license_url = config_value('license_url');
    $dbg  = defined $params->{'debug'};
    my $keystring=join('|', keys %$params);
    # call syntax  productroot %]/1.0/?eventStart=2008-06-23t23:00:00Z;eventSearch=1;event1Id=600;event2Id=620;eventVal1=60.;eventVal2=0.;eventVal3=0.
    my %callSeq=(); # the call sequence 
    my $seqFound=0;         # only default so far
    #####print "Found par:".$keystring ."\n";
    foreach my $par (keys %$params) { # loop over parameters and create input
	if ($par =~ m/^event(\d*)Start$/ ) {
	    $callSeq{$1?$1:$default}{"eventStart"}=getValue($params->{$par});
	} elsif ($par =~ m/^event(\d*)Search$/ ) {
	    $callSeq{$1?$1:$default}{"eventSearch"}=getValue($params->{$par});
	} elsif ($par =~ m/^event(\d*)Stop$/ ) {
	    $callSeq{$1?$1:$default}{"eventStop"}=getValue($params->{$par});
	} elsif ($par =~ m/^event(\d*)Id$/ ) {
	    $callSeq{$1?$1:$default}{"eventId"}=getValue($params->{$par});
	} elsif ($par =~ m/^event(\d*)Val(\d*)$/ ) {
	    $callSeq{$1?$1:$default}{"eventVal"}{$2}=getValue($params->{$par});
	} elsif ($par =~ m/^debug$/ ) {
	} else {
	    ############### report error in input parameter names ($par)
	    ###die "$par: Not a valid astroEvent key\n";
	    throw(param_name => $par, message => 'Not valid astroEvent key');
	}
	if ($par =~ m/^event(\d+)(\w+)$/) {$seqFound=1;} # we have at least one sequence, do not execute default sequence
    }
    # loop over input parameter sets and create reports
#	my $ret="";
#	foreach my $seq (keys %callSeq) {
#	    $ret="$ret|$seq";
#	}
#	throw(param_name => "test", message => $ret);
    foreach my $seq (keys %callSeq) {
	if ($seqFound && "$seq" eq "$default") {next;} # do not execute default sequence
	my $sseq="";
	if ("$seq" ne "$default") {$sseq=$seq;}
	my $eventStart=$dtgnow;
	my $eventSearch=1;
	my $eventStop="";
	my $eventId="";
	my %eventVal;
	if ("$seq" ne "$default") { # set default values
	    #####print "Setting default values. \"$seq\" \"$default\"\n";
	    if (defined $callSeq{$default}{"eventStart"}) {$eventStart=$callSeq{$default}{"eventStart"};}
	    if (defined $callSeq{$default}{"eventSearch"}) {$eventSearch=$callSeq{$default}{"eventSearch"};}
	    if (defined $callSeq{$default}{"eventStop"}) {$eventStop=$callSeq{$default}{"eventStop"};}
	    if (defined $callSeq{$default}{"eventId"}) {$eventId=$callSeq{$default}{"eventId"};}
	    foreach my $valno (sort keys %{$callSeq{$default}{"eventVal"}}) {
		$eventVal{$valno}=$callSeq{$default}{"eventVal"}{$valno};
	    }
	} else {
	    #####print "Not setting default values \"$seq\" \"$default\".\n";
	}
	if (defined $callSeq{$seq}{"eventStart"}) {$eventStart=$callSeq{$seq}{"eventStart"};}
	if (defined $callSeq{$seq}{"eventSearch"}) {$eventSearch=$callSeq{$seq}{"eventSearch"};}
	if (defined $callSeq{$seq}{"eventStop"}) {$eventStop=$callSeq{$seq}{"eventStop"};}
	if (defined $callSeq{$seq}{"eventId"}) {$eventId=$callSeq{$seq}{"eventId"};}
	foreach my $valno (sort keys %{$callSeq{$seq}{"eventVal"}}) {
	    $eventVal{$valno}=$callSeq{$seq}{"eventVal"}{$valno};
	}
	if ("$eventId" eq "") {
	    throw(param_name => "event".$sseq."Id", message => "Missing.");
	}
	if ("$eventStart" eq "") {
	    throw(param_name => "event".$sseq."Start", message => "Missing.");
	}
	if ("$eventSearch" eq "") {
	    throw(param_name => "event".$sseq."Search", message => "Missing.");
	}
	if ( "$eventSearch" eq "2" && "$eventStop" eq "") {
	    throw(param_name => "event".$sseq."Stop", message => "Missing (eventSearch=$eventSearch).");
	} 
	# make times
	my $eventStartJD="";
	my $eventStopJD="";
	if ($eventStart =~ m/^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2}\.?\d*)Z/) {
	    $eventStartJD=xs_DTGToJD($1,$2,$3,$4,$5,$6);
	    #print "EventStart: $1-$2-$3 $4:$5:$6 -> JD $eventStartJD\n"
	} else {
	    ############### report error in $eventStart
	    throw(param_name => "event".$sseq."Start", message => "Unable to extract date from '$eventStart'.");
	}
	if ("$eventSearch" ne "" && $eventSearch==2 && $eventStop =~ m/^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2}\.?\d*)Z/) {
	    $eventStopJD=xs_DTGToJD($1,$2,$3,$4,$5,$6);
	} elsif ("$eventSearch" ne "" && ($eventSearch==-2 || $eventSearch==-1 || $eventSearch==0 || $eventSearch==+1)) {
	} else {
	    ############### report error in $eventStop $eventSearch
	    my $msg="Mismatch";
	    if ("$eventStart"ne"") {$msg=$msg." Start time:$eventStart";}
	    if ("$eventStop"ne"") {$msg=$msg." Stop time:$eventStop";}
	    if ("$eventSearch"ne"") {$msg=$msg." Search code:$eventSearch";}
	    throw(param_name =>  "event".$sseq."Search", message => $msg);
	}
	# make eventVal array, based on ascending order of input eventVal
	my @eventVal=();
	foreach my $valno (sort keys %eventVal) {
	    if ("$eventVal{$valno}"ne"") { push (@eventVal, $eventVal{$valno}); }
	}
	# call astroEvent
	my $irc=0;
	my $nrep=0;
	my $repJD;
	my $repId;
	my $repVal;
	my $rep250;
#	    my $msg="$eventStartJD,$eventSearch,$eventStopJD,$eventId,@eventVal,$#eventVal";
#	    throw(message => "astroEvent call: $msg\n");
	my $start_time = [gettimeofday];
	eval {
	    if ("$eventSearch" eq "2") {      # use $eventStop
		($irc,$nrep,$repJD,$repId,$repVal,$rep250) = pm_astroEvent($eventStartJD,$eventSearch,$eventStopJD,$eventId,\@eventVal,0,0);
	    } else {
		$eventStop="";
		($irc,$nrep,$repJD,$repId,$repVal,$rep250) = pm_astroEvent($eventStartJD,$eventSearch,$eventId,\@eventVal,0,0);
	    }
	};
	my $msg=$@ ;
	if ( $msg ) { # astroEvent croaked...
	    $irc=-1;
	}
	$info{$seq}{'Cost'}=sprintf("%.1fms", 1000*tv_interval($start_time));
	my $total_time = tv_interval($start_time);
	# store output
	$info{'astroEvent'}=1; # the info hash contains results from direct calls to astroEvent
	if ("$eventStart" ne "") {$info{$seq}{'eventStart'}=$eventStart;}
	if ("$eventSearch" ne "") {$info{$seq}{'eventSearch'}=$eventSearch;}
	if ("$eventStop" ne "") {$info{$seq}{'eventStop'}=$eventStop;}
	if ("$eventId" ne "") {$info{$seq}{'eventId'}=$eventId;}
	my $eventValSize=scalar(@eventVal);
	for (my $ii=0; $ii < $eventValSize; $ii++) {
	    $info{$seq}{'eventVal'}{$ii}=$eventVal[$ii];
	}
	if ($irc == 0) {
	    $info{$seq}{'nrep'}=$nrep; # number of reports
	    for (my $ii=0; $ii < $nrep; $ii++) {
		$info{$seq}{'repJD'}{$ii}=$$repJD[$ii];
		$info{$seq}{'repId'}{$ii}=$$repId[$ii];
		$info{$seq}{'repVal'}{$ii}=$$repVal[$ii];
		$info{$seq}{'rep250'}{$ii}=$$rep250[$ii];
	    }
	} else {
	    $info{$seq}{'nrep'}=1; # number of reports
	    $info{$seq}{'repJD'}{0}=$eventStartJD;
	    $info{$seq}{'repId'}{0}="999";
	    $info{$seq}{'repVal'}{0}="0.0";
	    $info{$seq}{'rep250'}{0}="Error:" . $msg;
	    ############### report error in irc
	    ############### throw(param_name => 'astroEvent',message => 'Error return from astroEvent. $irc');
	}
    }
    my $expire = DateTime->now;
    $expire->add( minutes => 60*2 );
    $expire->add( minutes => int( rand(180) ) );
    
    my $exp = $expire->strftime('%a, %d %b %Y %T GMT');
    
    # and in the end, return XML:
    my $xml = create_xml(\%info);
    
    return ($xml, { 'status'  => 'OK',
		    'expires' => $exp,
		    'type'    => 'text/xml',}
	);
}

sub getValue {
    my ($adr,@jnk)=@_;
    my $refadr = ref $adr;
    if ($refadr eq "ARRAY") {
	$adr=@$adr[0];
    }
    return $adr;
}

sub getArray {
    my ($adr,@jnk)=@_;
    my $refadr = ref $adr;
    if ($refadr eq "ARRAY") {
	return @$adr;
    }
    return $adr;
}

sub extract_date {
    my ($tst) = @_;
    
    my ($date) = ( $tst =~ m{(\d{4}-\d{2}-\d{2})T} );
    
    return $date;
}

# This isn't weatherdata!
# So I call it astrodata.. Create element so that we easily can put in
# moon phase later on.
sub create_xml {
    my ($info) = @_;

    my $doc = XML::LibXML::Document->new('1.0', 'utf-8');
    $doc->setEncoding('utf-8');
    
    my $root = $doc->createElement('astrodata');
    $doc->setDocumentElement($root);

    $root->setAttribute('xmlns:xsi','http://www.w3.org/2001/XMLSchema-instance');
    $root->setAttribute('xmlns','http://astro.met.no');
    $root->setAttribute('xsi:schemaLocation','http://astro.met.no/astro event.xsd');

    my $meta = $doc->createElement('meta');
    $meta->setAttribute('licenseurl', $license_url);
    $root->appendChild($meta);
    if (defined $info->{'astroEvent'}) { # the info hash contains results from direct calls to astroEvent
	foreach my $seq (sort keys %$info) {
	    if ($seq eq 'astroEvent') {next;}
	    my $event=$doc->createElement('Event');
	    my $seqno=$event->setAttribute('Seq', $seq);
	    if (defined $info->{$seq}{'eventId'}) {$event->setAttribute('Id', $info->{$seq}{'eventId'});}
	    if (defined $info->{$seq}{'eventStart'}) {$event->setAttribute('Start', $info->{$seq}{'eventStart'});}
	    if (defined $info->{$seq}{'eventSearch'}) {$event->setAttribute('Search', $info->{$seq}{'eventSearch'});}
	    if (defined $info->{$seq}{'eventStop'}) {$event->setAttribute('Stop', $info->{$seq}{'eventStop'});}
	    for (my $ii=1; $ii <= 6; $ii++) {
		my $valstr="Val$ii";
		if (defined $info->{$seq}{'eventVal'}{$ii-1}) {$event->setAttribute($valstr, $info->{$seq}{'eventVal'}{$ii-1});}
	    }
	    my $nrep=$info->{$seq}{'nrep'};
	    my $repno=$event->setAttribute('reports', $nrep);
	    if ($dbg && defined $info->{$seq}{'Cost'}) {$event->setAttribute('cost', $info->{$seq}{'Cost'});}
	    for (my $ii=0; $ii < $nrep; $ii++) {
		my $report=$doc->createElement('Report');
		$report->setAttribute('no',$ii+1);
		my ($yy,$mm,$dd,$hh,$mi,$sec)=xs_JDToDTG($info->{$seq}{'repJD'}{$ii});
		my $date=sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",$yy,$mm,$dd,$hh,$mi,$sec);
		$report->setAttribute('time',$date);
		$report->setAttribute('repId',$info->{$seq}{'repId'}{$ii});
		if ("$info->{$seq}{'repVal'}{$ii}" ne "-99999") {
		    $report->setAttribute('repVal',$info->{$seq}{'repVal'}{$ii});
		}
		$report->setAttribute('hint',$info->{$seq}{'rep250'}{$ii});
		$event->appendChild($report);
	    }
	    $root->appendChild($event);
	}
    }    
    my $xml_content = $doc->toString(2);
    return $xml_content;
}

# Versions, past, present and future.  Date is which time that version
# stops working
sub versions {
    return {
	'0.8' => '2008-10-01',
	'0.9' => '2009-06-24',
	'1.0' => 'CURRENT',
    };
}

# Any schema related to this product?
sub schema {
  my ($self) = @_;

  return ('astrodata', 1.2);
}

sub throw {
    my (%msg) =@_;
    my $s="";
    while( my( $key, $value ) = each %msg ){
	$s=sprintf "$s|$key:$value";
    }
    print STDERR "$s\n";
    die $s;
}

1;
