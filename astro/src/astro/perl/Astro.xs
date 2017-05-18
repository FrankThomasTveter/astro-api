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
xs_event()
  PREINIT:
      int maxline = 1000;
      int nline;
      int *lenline;
      char *line250;
      int lenr=250;
      int ii;
  PPCODE:
    lenline = malloc(sizeof(int)*maxline);
    line250 = calloc(sizeof(char), lenr*maxline);
    event_(&maxline,&nline,lenline,line250,250);
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
    free(lenline);
    free(line250);

void
xs_state()
  PREINIT:
      int maxline = 1000;
      int nline;
      int *lenline;
      char *line250;
      int lenr=250;
      int ii;
  PPCODE:
    lenline = malloc(sizeof(int)*maxline);
    line250 = calloc(sizeof(char), lenr*maxline);
    state_(&maxline,&nline,lenline,line250,250);
    /* make return stack */
    EXTEND(SP, nline);
    for (ii=0; ii < nline ; ii++ ) {
      if (lenline[ii]==0) {
       PUSHs(sv_2mortal(newSVpv("",0)));
      } else {
       PUSHs(sv_2mortal(newSVpv(&line250[ii*250],lenline[ii])));
      };
    };
    free(lenline);
    free(line250);

void
xs_astroEvent(double tstartJD, int searchCode, double tendJD, int eventId, int neventVal, AV* eventValin, int secdec, int irc)
  PREINIT:
      double* eventVal;
      int maxrep = 100;
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
