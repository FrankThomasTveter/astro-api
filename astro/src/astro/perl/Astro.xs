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
  CODE:
        event_();

void
xs_state()
  CODE:
        state_();

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

void
xs_astroState(double lat, double lon, double hgt,...)
  PREINIT:
      char *crc250;
      int irc;
      int njd;
      int i;
      char *jd;
      char *b30;
      char *jd30;
      int lenc=250;
      int lenj=30;
      STRLEN lens;
  PPCODE:
    crc250 = calloc(sizeof(char), lenc);
    njd=items-3;
    /*printf("XXXXXXXXXXXXXXXX njd: %d items=%d\n",njd,items);*/
    jd30 = malloc(sizeof(char)*njd*lenj);
    b30 = malloc(sizeof(char)*lenj);
    for (i=1; i <= njd; i++) {
            jd= (char *)SvPV(ST(i+2),lens);
	    memset(b30,' ',lenj);
            strncpy(b30,jd,fmin(lens,lenj));
            memcpy(&jd30[(i-1)*30],b30,lenj);
	    /*printf("XXXXXXXXXXXXXXXX astroState: %d %s\n",i,b30); */
    }
    /* call fortran subroutine */
    astrostate_(&njd,jd30,&lat,&lon,&hgt,crc250,&irc,lenj,lenc);
    if (irc != 0) {
       free(jd30);
       croak("xs_astroState Error return from state (irc=%d '%s')", irc, crc250); 
    }
    /* make return stack */
    if (irc == 0) {
       EXTEND(SP, 2);
       PUSHs(sv_2mortal(newSViv(irc)));
       PUSHs(sv_2mortal(newSVpv(crc250,strlen(crc250))));
    }; 
    free(b30);
    free(jd30);
    free(crc250);
