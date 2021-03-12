#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "math.h"
#include "ppport.h"

#include "astroEvent.h"

#define true 1
#define false 0

MODULE = Astro		PACKAGE = Astro::Astro

void
xs_jplephOpen(char* path)
  PREINIT:
    int status;
    char *crc250;
    int slen;
    int clen;
  CODE:
        slen = strlen(path);
        crc250 = calloc(sizeof(char), 251);
        jplephopen_(path, crc250, &status, &slen, &clen);
        if (status != 0){croak("Error %d jplephOpen: '%s' '%s'", status, path,crc250);}
	free (crc250);

void
xs_jplephClose()
  CODE:
        jplephclose_();        


void
xs_JDToDTG(double JD, OUTLIST int year, OUTLIST int month, OUTLIST int mday, OUTLIST int hour, OUTLIST int min, OUTLIST double secs)
  CODE:
        jd2date_(&JD, &year, &month, &mday, &hour, &min, &secs);

void
xs_DTGToJD(int year, int month, int mday, int hour, int min, double secs, OUTLIST double JD)
  CODE:
        date2jd_(&JD, &year, &month, &mday, &hour, &min, &secs );

void
xs_short(char *s)
  PREINIT:
      char *s1000;
      int maxline = 5000;
      int nline;
      int *lenline;
      char *line250;
      int lenr=250;
      int ii;
  PPCODE:
    s1000 = calloc(sizeof(char), 1000);
    strcpy(s1000,s);
    lenline = malloc(sizeof(int)*maxline);
    line250 = calloc(sizeof(char), lenr*maxline);
    short_(s1000,&maxline,&nline,lenline,line250,1000,250);
    /* make return stack */
    EXTEND(SP, nline);
    for (ii=0; ii < nline ; ii++ ) {
      /*printf("  nline= %i %i\n", ii,lenline[ii]);*/
      if (lenline[ii]==0) {
       PUSHs(sv_2mortal(newSVpv("",0)));
      } else {
       PUSHs(sv_2mortal(newSVpv(&line250[ii*250],lenline[ii])));
      };
    };
    free(s1000);
    free(lenline);
    free(line250);

void
xs_small(char *lat, char *lon, char *hgt, char *day, char *dat, char *off)
  PREINIT:
      char *lat10;
      char *lon10;
      char *hgt10;
      char *day10;
      char *dat10;
      char *off6;
      int maxline = 1500;
      int nline;
      int *lenline;
      char *line250;
      int lenr=250;
      int ii;
  PPCODE:
    /*printf("  xs_input= %s %s %s %s %s %s\n", lat,lon,hgt,day,dat,off);*/
    lat10 = calloc(sizeof(char), 10);
    lon10 = calloc(sizeof(char), 10);
    hgt10 = calloc(sizeof(char), 10);
    day10 = calloc(sizeof(char), 10);
    dat10 = calloc(sizeof(char), 10);
    off6 = calloc(sizeof(char), 6);
    /*printf("  xs_between= %s %s %s %s %s %s\n", lat,lon,hgt,day,dat,off);*/
    strncpy(lat10,lat,10);
    strncpy(lon10,lon,10);
    strncpy(hgt10,hgt,10);
    strncpy(day10,day,10);
    strncpy(dat10,dat,10);
    strncpy(off6,off,6);
    /*printf("  xs_alloc= %s %s %s %s %s %s\n", lat,lon,hgt,day,dat,off);*/
    lenline = malloc(sizeof(int)*maxline);
    line250 = calloc(sizeof(char), lenr*maxline);
    //printf("  xs_start= %s %s %s %s %s %s\n", lat10,lon10,hgt10,day10,dat10,off6);
    small_(lat10,lon10,hgt10,day10,dat10,off6,&maxline,&nline,lenline,line250,10,10,10,10,10,6,250);
    /* make return stack */
    EXTEND(SP, nline);
    for (ii=0; ii < nline ; ii++ ) {
      /*printf("  nline= %i %i\n", ii,lenline[ii]);*/
      if (lenline[ii]==0) {
       PUSHs(sv_2mortal(newSVpv("",0)));
      } else {
       PUSHs(sv_2mortal(newSVpv(&line250[ii*250],lenline[ii])));
      };
    };
    free(lat10);
    free(lon10);
    free(hgt10);
    free(dat10);
    free(day10);
    free(off6);
    free(lenline);
    free(line250);

void
xs_event(char *s)
  PREINIT:
      char *s1000;
      int maxline = 5000;
      int nline;
      int *lenline;
      char *line250;
      int lenr=250;
      int ii;
  PPCODE:
    s1000 = calloc(sizeof(char), 1000);
    strcpy(s1000,s);
    lenline = malloc(sizeof(int)*maxline);
    line250 = calloc(sizeof(char), lenr*maxline);
    event_(s1000,&maxline,&nline,lenline,line250,1000,250);
    /* make return stack */
    EXTEND(SP, nline);
    for (ii=0; ii < nline ; ii++ ) {
      /*printf("  nline= %i %i\n", ii,lenline[ii]);*/
      if (lenline[ii]==0) {
       PUSHs(sv_2mortal(newSVpv("",0)));
      } else {
       PUSHs(sv_2mortal(newSVpv(&line250[ii*250],lenline[ii])));
      };
    };
    free(s1000);
    free(lenline);
    free(line250);

void
xs_state(char *s)
  PREINIT:
      char *s1000;
      int maxline = 1500;
      int nline;
      int *lenline;
      char *line250;
      int lenr=250;
      int ii;
  PPCODE:
    s1000 = calloc(sizeof(char), 1000);
    strcpy(s1000,s);
    lenline = malloc(sizeof(int)*maxline);
    line250 = calloc(sizeof(char), lenr*maxline);
    state_(s1000,&maxline,&nline,lenline,line250,1000,250);
    /* make return stack */
    EXTEND(SP, nline);
    for (ii=0; ii < nline ; ii++ ) {
      if (lenline[ii]==0) {
       PUSHs(sv_2mortal(newSVpv("",0)));
      } else {
       PUSHs(sv_2mortal(newSVpv(&line250[ii*250],lenline[ii])));
      };
    };
    free(s1000);
    free(lenline);
    free(line250);

void
xs_astroEvent(double tstartJD, int searchCode, double tendJD, int eventId, int neventVal, AV* eventValin, int secdec, int irc)
  PREINIT:
      double* eventVal;
      int maxrep = 1000;
      int nrep;
      double* repJD;
      int* repId;
      double* repVal;
      char *rep250;
      char *crc250;
      int ii;
      int lenr=250;
  PPCODE:
    /* allocate and assign input array */
    neventVal=av_len(eventValin)+1;
    if (neventVal<=0) { /* make sure at least one element exists */
     eventVal = malloc(sizeof(double));
     neventVal=0;
     eventVal[0]=0.;
    } else {
     eventVal = malloc(sizeof(double)*neventVal);
     for (ii=0; ii < neventVal; ii++ ) {
       SV** elem = av_fetch(eventValin, ii, 0);
       if  (elem != NULL) eventVal[ ii ] = SvNV(*elem);
     }
    }
    /* allocate output arrays */
    repJD = malloc(sizeof(double)*maxrep);
    repId = malloc(sizeof(int)*maxrep);
    repVal = malloc(sizeof(double)*maxrep);
    rep250 = calloc(sizeof(char), lenr*maxrep);
    crc250 = calloc(sizeof(char), lenr);
    nrep=0;
    /* call fortran subroutine */
/*printf("  EV(0)= %d %f", neventVal,eventVal[0]);*/
    astroevent_(&tstartJD,&searchCode,&tendJD,&eventId,&neventVal,eventVal,&maxrep,&nrep,repJD,repId,repVal,rep250,&secdec,crc250,&irc,250,250);
    if (irc != 0 ) {
      free(eventVal);
      free(repJD);
      free(repId);
      free(repVal);
      free(rep250);
//      free(crc250);
      croak("xs_astroEvent Error return from astroevent (irc=%d, nrep=%d eventId=%d '%s')", irc, nrep, eventId,crc250); 
    }
    /* make return stack */
    if (irc == 0) {
       EXTEND(SP, 2+4*nrep);
       PUSHs(sv_2mortal(newSViv(irc)));
       PUSHs(sv_2mortal(newSViv(nrep)));
       for (ii=0; ii < nrep ; ii++ ) {
          PUSHs(sv_2mortal(newSVnv(repJD[ii])));
       }
       for (ii=0; ii < nrep ; ii++ ) {
          PUSHs(sv_2mortal(newSViv(repId[ii])));
       }
       for (ii=0; ii < nrep ; ii++ ) {
          PUSHs(sv_2mortal(newSVnv(repVal[ii])));
       }
       for (ii=0; ii < nrep ; ii++ ) {
          PUSHs(sv_2mortal(newSVpv(&rep250[ii*250],250)));
       }
    };
    free(eventVal);
    free(repJD);
    free(repId);
    free(repVal);
    free(rep250);
    free(crc250);
