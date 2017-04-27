use Test::More tests => 128;
use Time::Local qw(timegm timelocal);
use Metno::Astro::AlmanacAlgorithm qw(:all);
use strict;
use warnings;
use POSIX;
my ($eventId);
my ($irc,$nrep,$rep2000,$repId,$repVal,$rep250);
$nrep=0;
#     
#     ASTRONOMICAL EVENT IDENTIFICATION TABLE
#     
#     EVENTID  = 100  'REPORT LOCAL INITIAL MOON STATE'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 100 -> if (repval >= 1) "moon is above horison" (repval <=-1) "moon is below"
#     o  repId = 101 -> if (repval >= 1) "lunar polar day" (repval =0) "no lunar polar effect" (repval<=-1) "lunar polar night"
#     o  repId = 102 -> moon phase
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,100,[60.,0.,0.],0,0);
  checkOk(100,DTGToJ2000(2008,6,26,12,0,0),1,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(101,DTGToJ2000(2008,6,26,12,0,0),0,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 110 : 'REPORT LOCAL TC EF MOON POSITION AT TIME INCREMENT'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     i  eventVal(4) = time increment (days)
#     o  repId = 110 -> repval = moon elevation (deg)
#     o  repId = 111 -> repval = moon azimuth (deg)
#     o  repId = 112 -> repval = moon range (km)
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,110,[60.,0.,0.],0,0);
  checkOk(110,DTGToJ2000(2008,6,26,12,0,0),2.89278883426464,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(111,DTGToJ2000(2008,6,26,12,0,0),274.218604477482,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(112,DTGToJ2000(2008,6,26,12,0,0),377483.009250029,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 120 : 'REPORT LOCAL INITIAL SUN STATE'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 120 -> if (repval >= 1) "sun is above horison" (repval <=-1) "sun is below"
#     o  repId = 121 -> if (repval >= 1) "polar day" (repval =0) "no polar effect" (repval<=-1) "polar night"
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,120,[60.,0.,0.],0,0);
  checkOk(120,DTGToJ2000(2008,6,26,12,0,0),1,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(121,DTGToJ2000(2008,6,26,12,0,0),0,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 130 : 'REPORT LOCAL TC EF SUN POSITION AT TIME INCREMENT'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     i  eventVal(4) = time increment (days)
#     o  repId = 130 -> repval = sun elevation (deg)
#     o  repId = 131 -> repval = sun azimuth (deg)
#     o  repId = 132 -> repval = sun range (km)
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+2,DTGToJ2000(2008,6,26,12,0,0),130,[60.,0.,0.,0.],0,0);
  checkOk(130,DTGToJ2000(2008,6,26,12,0,0),53.3311810460119 ,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(131,DTGToJ2000(2008,6,26,12,0,0),178.877118963031,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(132,DTGToJ2000(2008,6,26,12,0,0),152070921.0121,$nrep,$rep2000,$repId,$repVal,$rep250);
#      
#     EVENTID  = 150 : 'DETECT WINTER SOLSTICE'
#     o  repId = 150 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,150,[],0,0);
  checkOk(150,DTGToJ2000(2008,12,21,12,3,44.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#
#     EVENTID  = 160 : 'DETECT VERNAL EQUINOX'
#     o  repId = 160 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,160,[],0,0);
  checkOk(160,DTGToJ2000(2009,3,20,11,43,32.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#
#     EVENTID  = 170 : 'DETECT SUMMER SOLSTICE'
#     o  repId = 170 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,170,[],0,0);
  checkOk(170,DTGToJ2000(2009,6,21,5,45,29.6),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#
#     EVENTID  = 180 : 'DETECT AUTUMNAL EQUINOX'
#     o  repId = 180 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,180,[],0,0);
  checkOk(180,DTGToJ2000(2008,9,22,15,44,23),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#
#     EVENTID  = 190 : 'DETECT EARTH IN PERIHELION'
#     o  repId = 190 -> repval = sun range (km)
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,190,[],0,0);
  checkOk(190,DTGToJ2000(2009,1,4,15,37,50.3),147095552.987911,$nrep,$rep2000,$repId,$repVal,$rep250);
#
#     EVENTID  = 200 : 'DETECT EARTH IN APHELION'
#     o  repId = 200 -> repval = sun range (km)
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,200,[],0,0);
  checkOk(200,DTGToJ2000(2008,7,4,7,48,34.9),152104161.159031,$nrep,$rep2000,$repId,$repVal,$rep250);
#      
#     EVENTID  = 210 : 'DETECT NEW MOON (PHASE=0/100)'
#     o  repId = 210 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,210,[],0,0);
  checkOk(210,DTGToJ2000(2008,7,3,2,18,34.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#      
#     EVENTID  = 220 : 'DETECT FIRST QUARTER MOON (PHASE=25)'
#     o  repId = 220 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,220,[],0,0);
  checkOk(220,DTGToJ2000(2008,7,10,4,15,59.1),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#      
#     EVENTID  = 230 : 'DETECT FULL MOON (PHASE=50)'
#     o  repId = 230 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,230,[],0,0);
  checkOk(230,DTGToJ2000(2008,7,18,7,59,3.7),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#      
#     EVENTID  = 240 : 'DETECT LAST QUARTER MOON (PHASE=75)'
#     o  repId = 240 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,240,[],0,0);
  checkOk(240,DTGToJ2000(2008,6,26,12,26,0.2),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#      
#     EVENTID  = 250 : 'DETECT MOON PHASE (0 TO 100)'
#     i  eventVal(1) = target moon phase
#     o  repId = 250 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,250,[69.],0,0);
  checkOk(250,DTGToJ2000(2008,7,24,2,26,36.8),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#      
#     EVENTID  = 260 : 'DETECT MOON ILLUMINATION MINIMUM'
#     o  repId = 260 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,260,[],0,0);
  checkOk(260,DTGToJ2000(2008,7,3,2,43,19.4),0.0755179148890783,$nrep,$rep2000,$repId,$repVal,$rep250);
#      
#     EVENTID  = 270 : 'DETECT MOON ILLUMINATION MAXIMUM'
#     o  repId = 270 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,270,[],0,0);
  checkOk(270,DTGToJ2000(2008,7,18,8,22,16),99.9696791753844,$nrep,$rep2000,$repId,$repVal,$rep250);
#      
#     EVENTID  = 280 : 'DETECT MOON ILLUMINATION (0 TO 100)'
#     i  eventVal(1) = target moon illumination
#     o  repId = 280 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,280,[71],0,0);
  checkOk(280,DTGToJ2000(2008,7,12,10,1,14.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#      
#
#     EVENTID  = 300 : 'DETECT MERCURY INFERIOR CONJUNCTION'
#     o  repId = 300 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,300,[],0,0);
  checkOk(300,DTGToJ2000(2008,10,6,20,52,54),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 310 : 'DETECT MERCURY SUPERIOR CONJUNCTION'
#     o  repId = 310 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,310,[],0,0);
  checkOk(310,DTGToJ2000(2008,7,29,20,4,21.2),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 320 : 'DETECT MERCURY GREATEST WESTERN ELONGATION'
#     o  repId = 320 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,320,[],0,0);
  checkOk(320,DTGToJ2000(2008,7,1,17,54,1.7),21.7841658600798,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 330 : 'DETECT MERCURY GREATEST EASTERN ELONGATION'
#     o  repId = 330 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,330,[],0,0);
  checkOk(330,DTGToJ2000(2008,9,11,4,31,46.6),-26.8714049661546,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
      
#     EVENTID  = 340 : 'DETECT VENUS INFERIOR CONJUNCTION'
#     o  repId = 340 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,340,[],0,0);
  checkOk(340,DTGToJ2000(2009,3,27,19,23,25.9),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 350 : 'DETECT VENUS GREATEST WESTERN ELONGATION'
#     o  repId = 350 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,350,[],0,0);
  checkOk(350,DTGToJ2000(2009,6,5,20,50,33.5),45.8520793304237,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 360 : 'DETECT VENUS SUPERIOR CONJUNCTION'
#     o  repId = 360 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,360,[],0,0);
  checkOk(360,DTGToJ2000(2010,1,11,21,5,48.9),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 370 : 'DETECT VENUS GREATEST EASTERN ELONGATION'
#     o  repId = 370 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,370,[],0,0);
  checkOk(370,DTGToJ2000(2009,1,14,21,23,37.6),-47.1223938794795,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
      
#     EVENTID  = 380 : 'DETECT MARS CONJUNCTION'
#     o  repId = 380 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,380,[],0,0);
  checkOk(380,DTGToJ2000(2008,12,5,22,3,38.8),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 390 : 'DETECT MARS WESTERN QUADRATURE'
#     o  repId = 390 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,390,[],0,0);
  checkOk(390,DTGToJ2000(2009,10,29,7,56,6.8),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 400 : 'DETECT MARS OPPOSITION'
#     o  repId = 400 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,400,[],0,0);
  checkOk(400,DTGToJ2000(2010,1,29,19,43,3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 410 : 'DETECT MARS EASTERN QUADRATURE'
#     o  repId = 410 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,410,[],0,0);
  checkOk(410,DTGToJ2000(2010,5,4,13,9,1.9),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
      
#     EVENTID  = 420 : 'DETECT JUPITER CONJUNCTION'
#     o  repId = 420 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,420,[],0,0);
  checkOk(420,DTGToJ2000(2009,1,24,5,44,13.4),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 430 : 'DETECT JUPITER WESTERN QUADRATURE'
#     o  repId = 430 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,430,[],0,0);
  checkOk(430,DTGToJ2000(2009,5,16,8,45,21.8),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 440 : 'DETECT JUPITER OPPOSITION'
#     o  repId = 440 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,440,[],0,0);
  checkOk(440,DTGToJ2000(2008,7,9,7,39,8.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 450 : 'DETECT JUPITER EASTERN QUADRATURE'
#     o  repId = 450 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,450,[],0,0);
  checkOk(450,DTGToJ2000(2008,10,6,16,51,59.2),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
      
#     EVENTID  = 460 : 'DETECT SATURN CONJUNCTION'
#     o  repId = 460 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,460,[],0,0);
  checkOk(460,DTGToJ2000(2008,9,4,1,59,39.6),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 470 : 'DETECT SATURN WESTERN QUADRATURE'
#     o  repId = 470 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,470,[],0,0);
  checkOk(470,DTGToJ2000(2008,12,13,2,19,51.1),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 480 : 'DETECT SATURN OPPOSITION'
#     o  repId = 480 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,480,[],0,0);
  checkOk(480,DTGToJ2000(2009,3,8,19,53,8.9),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 490 : 'DETECT SATURN EASTERN QUADRATURE'
#     o  repId = 490 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,490,[],0,0);
  checkOk(490,DTGToJ2000(2009,6,5,19,10,32.8),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
      
#     EVENTID  = 500 : 'DETECT MERCURY TRANSIT (ANYWHERE ON EARTH)'
#     o  repId = 500 -> transit starts
#     o  repId = 501 -> transit ends
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,500,[],0,0);
  checkOk(500,DTGToJ2000(2016,5,9,11,15,20.7),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(501,DTGToJ2000(2016,5,9,18,39,25),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 520 : 'DETECT VENUS TRANSIT (ANYWHERE ON EARTH)'
#     o  repId = 520 -> transit starts
#     o  repId = 521 -> transit ends
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,520,[],0,0);
  checkOk(520,DTGToJ2000(2012,6,5,22,8,39.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(521,DTGToJ2000(2012,06,06,04,50,33),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
      
#     EVENTID  = 550 : 'DETECT LUNAR ECLIPSE (MINOCC MAXOCC)'
#     i  eventVal(1) = minimum occultation (0 to 100)
#     i  eventVal(2) = maximum occultation (0 to 100)
#     o  repId = 550 -> penumbra contact starts (P1)
#     o  repId = 551 -> umbra contact starts (U1)
#     o  repId = 552 -> total eclipse starts (U2)
#     o  repId = 553 -> repval = maximum occultation
#     o  repId = 554 -> total eclipse stops (U3)
#     o  repId = 555 -> umbra contact stops (U4)
#     o  repId = 556 -> penumbra contact stops (P2)
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,550,[90.,100.],0,0);
  checkOk(550,DTGToJ2000(2010,12,21,5,31,3.2 ),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(551,DTGToJ2000(2010,12,21,6,33,10.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(552,DTGToJ2000(2010,12,21,7,41,31.7),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(553,DTGToJ2000(2010,12,21,8,16,58.3),100,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(554,DTGToJ2000(2010,12,21,8,52,26.4),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(555,DTGToJ2000(2010,12,21,10,0,48.7),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(556,DTGToJ2000(2010,12,21,11,2,47.6),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 560 : 'DETECT LUNAR ECLIPSE -LUNECL[0]'
#     o  repId = 560 -> penumbra contact starts (P1)
#     o  repId = 561 -> umbra contact starts (U1)
#     o  repId = 562 -> total eclipse starts (U2)
#     o  repId = 563 -> repval = maximum occultation
#     o  repId = 564 -> total eclipse stops (U3)
#     o  repId = 565 -> umbra contact stops (U4)
#     o  repId = 566 -> penumbra contact stops (P2)
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,560,[],0,0);
  checkOk(560,DTGToJ2000(2008,8,16,18,26,40.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(561,DTGToJ2000(2008,8,16,19,36,41.8),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#  checkOk(562,DTGToJ2000(2010,12,21,7,41,31.7),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(563,DTGToJ2000(2008,8,16,21,10,6.2),83.1900809483519,$nrep,$rep2000,$repId,$repVal,$rep250);
#  checkOk(564,DTGToJ2000(2010,12,21,8,52,26.4),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(565,DTGToJ2000(2008,8,16,22,43,35),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(566,DTGToJ2000(2008,8,16,23,53,28.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
      
#     EVENTID  = 600 : 'DETECT LOCAL DIURNAL SUN RISE'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 600 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,600,[60.,0.,0.],0,0);
  checkOk(600,DTGToJ2000(2008,6,27,2,38,53.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 610 : 'DETECT LOCAL DIURNAL SUN SET'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 610 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,610,[60.,0.,0.],0,0);
  checkOk(610,DTGToJ2000(2008,6,26,21,27,16.6),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 620 : 'DETECT LOCAL DIURNAL MAXIMUM SOLAR ELEVATION'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 620 -> repval = maximum solar elevation (deg)
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,620,[60.,0.,0.],0,0);
  checkOk(620,DTGToJ2000(2008,6,26,12,2,35.8),53.3346700013709,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 630 : 'DETECT LOCAL DIURNAL MINIMUM SOLAR ELEVATION'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 630 -> repval = minimum solar elevation (deg)
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,12,26,12,0,0),+1,630,[60.,0.,0.],0,0);
  checkOk(630,DTGToJ2000(2008,12,26,23,59,59.9),-53.3222778232689,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 640 : 'DETECT LOCAL DIURNAL CIVIL TWILIGHT START'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 640 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,12,26,12,0,0),+1,640,[60.,0.,0.],0,0);
  checkOk(640,DTGToJ2000(2008,12,26,14,58,0.9),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 650 : 'DETECT LOCAL DIURNAL CIVIL TWILIGHT STOP'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 650 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,12,26,12,0,0),+1,650,[60.,0.,0.],0,0);
  checkOk(650,DTGToJ2000(2008,12,27,9,3,37.1),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 660 : 'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT START'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 660 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,12,26,12,0,0),+1,660,[60.,0.,0.],0,0);
  checkOk(660,DTGToJ2000(2008,12,26,15,58,17.8),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 670 : 'DETECT LOCAL DIURNAL NAUTICAL TWILIGHT STOP'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 670 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,12,26,12,0,0),+1,670,[60.,0.,0.],0,0);
  checkOk(670,DTGToJ2000(2008,12,27,8,3,27.1),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 680 : 'DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT START'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 680 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,12,26,12,0,0),+1,680,[60.,0.,0.],0,0);
  checkOk(680,DTGToJ2000(2008,12,26,16,54,44.),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 690 : 'DETECT LOCAL DIURNAL ASTRONOMICAL TWILIGHT STOP'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 690 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,12,26,12,0,0),+1,690,[60.,0.,0.],0,0);
  checkOk(690,DTGToJ2000(2008,12,27,7,7,4),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 700 : 'DETECT LOCAL DIURNAL NIGHT START'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 700 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,12,26,12,0,0),+1,700,[60.,0.,0.],0,0);
  checkOk(700,DTGToJ2000(2008,12,26,17,46,4.9),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 710 : 'DETECT LOCAL DIURNAL NIGHT STOP'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 710 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,12,26,12,0,0),+1,710,[60.,0.,0.],0,0);
  checkOk(710,DTGToJ2000(2008,12,27,6,15,46),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 750 : 'DETECT LOCAL DIURNAL SUN AZIMUTH (0=NORTH, 90=EAST)'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     i  eventVal(4) = sun azimuth (deg)
#     o  repId = 750 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,750,[60.,0.,0.,180.],0,0);
  checkOk(750,DTGToJ2000(2008,6,26,12,2,55.2),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 760 : 'DETECT LOCAL DIURNAL APPARENT SOLAR TIME'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     i  eventVal(4) = apparent solar time (0 to 24)
#     o  repId = 760 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,760,[60.,0.,0.,9],0,0);
  checkOk(760,DTGToJ2000(2008,6,27,9,3,5.9),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 770 : 'DETECT LOCAL DIURNAL APPARENT LUNAR TIME'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     i  eventVal(4) = apparent lunar time (0 to 24)
#     o  repId = 770 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,770,[60.,0.,0.,9],0,0);
  checkOk(770,DTGToJ2000(2008,6,27,3,22,11.7),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#     
#     EVENTID  = 800 : 'DETECT LOCAL DIURNAL MOON RISE'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 800 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,800,[60.,0.,0.],0,0);
  checkOk(800,DTGToJ2000(2008,6,26,23,16,44),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     EVENTID  = 810 : 'DETECT LOCAL DIURNAL MOON SET'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 810 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,810,[60.,0.,0.],0,0);
  checkOk(810,DTGToJ2000(2008,6,26,12,32,0.4),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     EVENTID  = 820 : 'DETECT LOCAL DIURNAL MAXIMUM MOON ELEVATION'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 820 -> repVal = maximum moon elevation
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,820,[60.,0.,0.],0,0);
  checkOk(820,DTGToJ2000(2008,6,27,6,34,47.7),39.67748,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     EVENTID  = 830 : 'DETECT LOCAL DIURNAL MINIMUM MOON ELEVATION'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 830 -> repVal = minimum moon elevation
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,830,[60.,0.,0.],0,0);
  checkOk(830,DTGToJ2000(2008,6,26,17,56,36.0),-23.63331,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     EVENTID  = 840 : 'DETECT LOCAL DIURNAL MOON AZIMUTH (0=NORTH, 90=EAST)'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     i  eventVal(4) = moon azimuth (deg)
#     o  repId = 840 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,840,[60.,0.,0.,9],0,0);
  checkOk(840,DTGToJ2000(2008,6,26,18,39,3.7),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
      
#     EVENTID  = 900 : 'DETECT LOCAL POLAR SUN DAY START'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 900 -> event found
#     o  repId = 901 -> previous sun rise
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,900,[80.,0.,0.],0,0);
  checkOk(900,DTGToJ2000(2009,4,13,10,29,55.4),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(901,DTGToJ2000(2009,4,13,0,40,27.7),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     EVENTID  = 910 : 'DETECT LOCAL POLAR SUN DAY STOP'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 910 -> event found
#     o  repId = 911 -> next sun set
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,910,[80.,0.,0.],0,0);
  checkOk(910,DTGToJ2000(2008,8,29,8,17,10.1),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(911,DTGToJ2000(2008,8,29,23,11,49.9),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     EVENTID  = 920 : 'DETECT LOCAL POLAR SUN NIGHT START'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 920 -> event found
#     o  repId = 921 -> previous sun set
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,920,[80.,0.,0.],0,0);
  checkOk(920,DTGToJ2000(2008,10,21,5,12,35.5),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(921,DTGToJ2000(2008,10,20,12,36,14.7),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     EVENTID  = 930 : 'DETECT LOCAL POLAR SUN NIGHT STOP'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 930 -> event found
#     o  repId = 931 -> next sun rise
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,930,[80.,0.,0.],0,0);
  checkOk(930,DTGToJ2000(2009,2,20,8,0,51.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(931,DTGToJ2000(2009,2,20,11,48,55.7),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     
#     EVENTID  = 940 : 'DETECT LOCAL POLAR LUNAR DAY START'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 940 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,940,[80.,0.,0.],0,0);
  checkOk(940,DTGToJ2000(2008,6,27,3,38,1.5),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     EVENTID  = 950 : 'DETECT LOCAL POLAR LUNAR DAY STOP'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 950 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,950,[80.,0.,0.],0,0);
  checkOk(950,DTGToJ2000(2008,7,6,15,28,47.5),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     EVENTID  = 960 : 'DETECT LOCAL POLAR LUNAR NIGHT START'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 960 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,960,[80.,0.,0.],0,0);
  checkOk(960,DTGToJ2000(2008,7,10,1,9,30.4),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     EVENTID  = 970 : 'DETECT LOCAL POLAR LUNAR NIGHT STOP'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 970 -> event found
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,970,[80.,0.,0.],0,0);
  checkOk(970,DTGToJ2000(2008,7,21,0,48,39.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     
#     EVENTID  = 980 : 'DETECT LOCAL SOLAR ECLIPSE (MINOCC MAXOCC)'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     i  eventVal(4) = minimum occultation (0 to 100)
#     i  eventVal(5) = maximum occultation (0 to 100)
#     o  repId = 980 -> partial solar eclipse starts
#     o  repId = 981 -> total solar eclipse starts
#     o  repId = 982 -> repVal = maximum occultation
#     o  repId = 983 -> total solar eclipse stops
#     o  repId = 984 -> partial solar eclipse stops
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2011,1,1,12,0,0),+1,980,[59.9134,10.7195,0.,50,100],0,0);
  checkOk(980,DTGToJ2000(2011,1,4,7,19,29.9 ),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# checkOk(981,DTGToJ2000(2008,7,21,0,48,39.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(982,DTGToJ2000(2011,1,4,8,33,48.8),79.355487511363009,$nrep,$rep2000,$repId,$repVal,$rep250);
# checkOk(983,DTGToJ2000(2008,7,21,0,48,39.3),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(984,DTGToJ2000(2011,1,4,9,54,59.8),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     EVENTID  = 990 : 'DETECT LOCAL SOLAR ECLIPSE'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     o  repId = 990 -> partial solar eclipse starts
#     o  repId = 991 -> total solar eclipse starts
#     o  repId = 992 -> repVal = maximum occultation
#     o  repId = 993 -> total solar eclipse stops
#     o  repId = 994 -> partial solar eclipse stops
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+1,990,[60.,0.,0.],0,0);
  checkOk(990,DTGToJ2000(2008,8,1,8,25,43.1),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
#  checkOk(991,DTGToJ2000(2008,8,1,8,25,43.1),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(992,DTGToJ2000(2008,8,1,9,23,50.7),36.0269459411464,$nrep,$rep2000,$repId,$repVal,$rep250);
#  checkOk(993,DTGToJ2000(2008,8,1,8,25,43.1),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(994,DTGToJ2000(2008,8,1,10,21,28.7),-99999,$nrep,$rep2000,$repId,$repVal,$rep250);
# 
#     
#     EVENTID  = 1000 : 'REPORT LOCAL TC EF SOLAR SYSTEM POSITIONS AT TIME INCREMENT'
#     i  eventVal(1) = latitude of observer (deg)
#     i  eventVal(2) = longtitude of observer (deg)
#     i  eventVal(3) = height of observer (deg)
#     i  eventVal(4) = time increment (days)
#     o  repId = 1000 -> repval = sun elevation (deg)
#     o  repId = 1001 -> repval = sun azimuth (deg)
#     o  repId = 1002 -> repval = sun range (km)
#     o  repId = 1010 -> repval = mercury elevation (deg)
#     o  repId = 1011 -> repval = mercury azimuth (deg)
#     o  repId = 1012 -> repval = mercury range (km)
#     o  repId = 1020 -> repval = venus elevation (deg)
#     o  repId = 1021 -> repval = venus azimuth (deg)
#     o  repId = 1022 -> repval = venus range (km)
#     o  repId = 1030 -> repval = moon elevation (deg)
#     o  repId = 1031 -> repval = moon azimuth (deg)
#     o  repId = 1032 -> repval = moon range (km)
#     o  repId = 1040 -> repval = mars elevation (deg)
#     o  repId = 1041 -> repval = mars azimuth (deg)
#     o  repId = 1042 -> repval = mars range (km)
#     o  repId = 1050 -> repval = jupiter elevation (deg)
#     o  repId = 1051 -> repval = jupiter azimuth (deg)
#     o  repId = 1052 -> repval = jupiter range (km)
#     o  repId = 1060 -> repval = saturn elevation (deg)
#     o  repId = 1061 -> repval = saturn azimuth (deg)
#     o  repId = 1062 -> repval = saturn range (km)
#     o  repId = 1070 -> repval = uranus elevation (deg)
#     o  repId = 1071 -> repval = uranus azimuth (deg)
#     o  repId = 1072 -> repval = uranus range (km)
#     o  repId = 1080 -> repval = neptun elevation (deg)
#     o  repId = 1081 -> repval = neptun azimuth (deg)
#     o  repId = 1082 -> repval = neptun range (km)
#     o  repId = 1090 -> repval = pluto elevation (deg)
#     o  repId = 1091 -> repval = pluto azimuth (deg)
#     o  repId = 1092 -> repval = pluto range (km)
#
 ($irc,$nrep,$rep2000,$repId,$repVal,$rep250) = astroEvent(DTGToJ2000(2008,6,26,12,0,0),+2,DTGToJ2000(2008,6,26,12,0,0),1000,[60.,0.,0.,0.],0,0);
  checkOk(1000,DTGToJ2000(2008,6,26,12,0,0),53.3311810460119,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1001,DTGToJ2000(2008,6,26,12,0,0),178.877118963031,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1002,DTGToJ2000(2008,6,26,12,0,0),152070921.0121,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1010,DTGToJ2000(2008,6,26,12,0,0),45.981131441789,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1011,DTGToJ2000(2008,6,26,12,0,0),209.223100698053,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1012,DTGToJ2000(2008,6,26,12,0,0),110201881.821871,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1020,DTGToJ2000(2008,6,26,12,0,0),53.5294991412329,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1021,DTGToJ2000(2008,6,26,12,0,0),170.825750328584,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1022,DTGToJ2000(2008,6,26,12,0,0),258326149.791338,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1030,DTGToJ2000(2008,6,26,12,0,0),2.89278883426464,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1031,DTGToJ2000(2008,6,26,12,0,0),274.218604477482,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1032,DTGToJ2000(2008,6,26,12,0,0),377483.009250029,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1040,DTGToJ2000(2008,6,26,12,0,0),29.0789894672295,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1041,DTGToJ2000(2008,6,26,12,0,0),115.027347166824,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1042,DTGToJ2000(2008,6,26,12,0,0),311377368.308441,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1050,DTGToJ2000(2008,6,26,12,0,0),-50.654677414589,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1051,DTGToJ2000(2008,6,26,12,0,0),336.723073926115,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1052,DTGToJ2000(2008,6,26,12,0,0),626809486.868862,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1060,DTGToJ2000(2008,6,26,12,0,0),23.9766404384595,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1061,DTGToJ2000(2008,6,26,12,0,0),109.334697443275,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1062,DTGToJ2000(2008,6,26,12,0,0),1465689785.00427,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1070,DTGToJ2000(2008,6,26,12,0,0),-8.83677228064275,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1071,DTGToJ2000(2008,6,26,12,0,0),278.115060949008,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1072,DTGToJ2000(2008,6,26,12,0,0),2970021558.29707,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1080,DTGToJ2000(2008,6,26,12,0,0),-30.640657480186,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1081,DTGToJ2000(2008,6,26,12,0,0),298.041411552869,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1082,DTGToJ2000(2008,6,26,12,0,0),4392301384.12429,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1090,DTGToJ2000(2008,6,26,12,0,0),-46.880667022267,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1091,DTGToJ2000(2008,6,26,12,0,0),7.44343775887004,$nrep,$rep2000,$repId,$repVal,$rep250);
  checkOk(1092,DTGToJ2000(2008,6,26,12,0,0),4559147979.85225,$nrep,$rep2000,$repId,$repVal,$rep250);
# 


# check time and value for given eventId (only works for reports valid for the same time).
sub checkOk {
    my ($id,$j2000,$val,$nreport,$report2000,$reportId,$reportVal,$report250)=@_;
    # Note that the following are array-references: $report2000,$reportId,$reportVal,$report250
    my $found=0;
    my $first=1;
    my $closestdt=0;
    my $closestii=0;
    my $ii;
    my $dt;
    for ($ii=0;$ii < $nreport; $ii++) {
	if ($$reportId[$ii] == $id) {    # $reportId is an array reference for @$reportId, use $$reportId[$ii] to get array element
	    $dt=abs($$report2000[$ii] - $j2000);
	    if ($first) {
		$first=0;
		$closestdt=$dt;
		$closestii=$ii;
	    } elsif ($dt < $closestdt) {
		$closestdt=$dt;
		$closestii=$ii;
	    }
            if ($dt < 5/86400.) {
                ok(abs($$reportVal[$ii] - $val) < (0.005 + abs($$reportVal[$ii])*0.000001), "EventId: $$reportId[$ii] *** Value: $$reportVal[$ii] ($val) $$report250[$ii]");
                $found=1;
            }
	}     
    }
    if (! $found && ! $first) {
	my ($yyE,$mmE,$ddE,$hhE,$miE,$secE) = J2000ToDTG($$report2000[$closestii]);
	my ($yyR,$mmR,$ddR,$hhR,$miR,$secR) = J2000ToDTG($j2000);
	$secE=floor($secE*10)/10;
	ok(abs($$report2000[$closestii] - $j2000) < 5.0/86400.0, "EventId: $$reportId[$closestii] *** Time: $yyE/$mmE/$ddE $hhE:$miE:$secE ($yyR/$mmR/$ddR $hhR:$miR:$secR) $$report250[$closestii]");
    } elsif ($first) {
	ok(0, "EventId: $id *** No reports found.");
    }
}
