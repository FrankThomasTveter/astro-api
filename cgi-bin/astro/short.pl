#!/usr/bin/perl
#
use Astro::Api qw(:all);
use strict;

use XML::LibXSLT;
use XML::LibXML;

my $xsl= << EOF;
<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
                xmlns:astro="http://astro.met.no" exclude-result-prefixes="astro">
  <xsl:output method="xml" indent="yes" encoding="utf-8"/>

  <xsl:template match="/*">
    <astrodata xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
               xsi:noNamespaceSchemaLocation="http://schema.api.met.no/schemas/astrodata-2.0.xsd">
      <meta licenseurl="https://api.met.no/license_data.html"/>
        <location latitude="{astro:Event[1]/@Val1}"
                  longitude="{/*/astro:Event[1]/@Val2}"
                  height="{/*/astro:Event[1]/@Val3}">
          <xsl:apply-templates select="/*/astro:Event[@Id='100']"/>
          <xsl:apply-templates select="/*/astro:Event[@Id='110']"/>
          <xsl:apply-templates select="/*/astro:Event/astro:Report"><!-- this will match above once more, but since is empty will result in null string -->
            <xsl:sort select="@time"/>
          </xsl:apply-templates>
        </location>
    </astrodata>
  </xsl:template>

  <!-- sun stuff -->
  <xsl:template match="/*/astro:Event[@Id='600']/astro:Report">
    <sunrise time="{@time}" desc="{substring(@hint,21)}"/>
  </xsl:template>

  <xsl:template match="/*/astro:Event[@Id='610']/astro:Report">
    <sunset time="{@time}" desc="{substring(@hint,21)}"/>
  </xsl:template>

  <xsl:template match="/*/astro:Event[@Id='620']/astro:Report">
    <solarnoon time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
  </xsl:template>

  <xsl:template match="/*/astro:Event[@Id='630']/astro:Report">
    <solarmidnight time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
  </xsl:template>

  <!-- moon stuff -->
  <xsl:template match="/*/astro:Event[@Id='100']">
    <moonphase time="{astro:Report[@repId='102']/@time}"
               value="{astro:Report[@repId='102']/@repVal}"
               desc="{substring(astro:Report[@repId='102']/@hint,21)}"/>
    <moonshadow time="{astro:Report[@repId='103']/@time}"
                elevation="{astro:Report[@repId='104']/@repVal}"
                azimuth="{astro:Report[@repId='103']/@repVal}"
                desc="{substring(astro:Report[@repId='103']/@hint,21)}"/>
  </xsl:template>

  <xsl:template match="/*/astro:Event[@Id='110']">
    <moonposition time="{child::astro:Report[@repId='110']/@time}"
                  elevation="{child::astro:Report[@repId='110']/@repVal}"
                  azimuth="{child::astro:Report[@repId='111']/@repVal}"
                  range="{child::astro:Report[@repId='112']/@repVal}"
                  desc="{substring(child::astro:Report[@repId='110']/@hint,21)}"/>
  </xsl:template>

  <xsl:template match="/*/astro:Event[@Id='800']/astro:Report">
    <moonrise time="{@time}" desc="{substring(@hint,21)}"/>
  </xsl:template>

  <xsl:template match="/*/astro:Event[@Id='810']/astro:Report">
    <moonset time="{@time}" desc="{substring(@hint,21)}"/>
  </xsl:template>

  <xsl:template match="/*/astro:Event[@Id='820']/astro:Report">
    <high_moon time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
  </xsl:template>

  <xsl:template match="/*/astro:Event[@Id='830']/astro:Report">
    <low_moon time="{@time}" elevation="{@repVal}" desc="{substring(@hint,21)}"/>
  </xsl:template>

  <!-- polar stuff -->
  <xsl:template match="/*/astro:Event[@Id='900']/astro:Report">
    <polardaystart time="{@time}" desc="{substring(@hint,21)}"/>
  </xsl:template>

  <xsl:template match="/*/astro:Event[@Id='910']/astro:Report">
    <polardayend time="{@time}" desc="{substring(@hint,21)}"/>
  </xsl:template>

  <xsl:template match="/*/astro:Event[@Id='920']/astro:Report">
    <polarnightstart time="{@time}" desc="{substring(@hint,21)}"/>
  </xsl:template>

  <xsl:template match="/*/astro:Event[@Id='930']/astro:Report">
    <polarnightend time="{@time}" desc="{substring(@hint,21)}"/>
  </xsl:template>

</xsl:stylesheet>
EOF


my @lines = &Astro::Api::event();

print shift(@lines) . "\n";
print shift(@lines) . "\n";


my $source = XML::LibXML->load_xml(string => join "\n", @lines);

my $style_doc = XML::LibXML->load_xml(string=>$xsl,no_cdata=>1);
my $stylesheet = XML::LibXSLT->new->parse_stylesheet($style_doc);

my $results = $stylesheet->transform($source);
print $stylesheet->output_as_bytes($results);

#
my @lines = &Astro::Api::event();
foreach my $line (@lines) {
    print "$line\n";
}
#
