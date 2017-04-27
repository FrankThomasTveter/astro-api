#!/usr/bin/perl

$HoH={};
$HoH{flintstones}{wife} = "wilma";
$HoH{jetsons}{'his boy'} =~ s/(\w)/\u$1/;
for $family ( keys %HoH ) {
    print "$family: ";
    for $role ( keys %{ $HoH{$family} } ) {
	print "$role=$HoH{$family}{$role} ";
    }
    print "\n";
}

