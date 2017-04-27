#ifndef ASTRO_H_INCLUDED
#define ASTRO_H_INCLUDED
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/* init */

  void jplephopen_(char* jpl1024, char* crc250, int* irc, int* strlen, int* crclen);

void jplephclose_();

void jd2date_(double* JD, int* year, int* month, int* mday, int* hour, int* min, double* secs);
void date2jd_(double* JD, int* year, int* month, int* mday, int* hour, int* min, double* secs);
  
void astroevent_( double *tstart2000, // start time (in jd2000)
                  int *searchCode,    // search code (-1:prev,0:both,+1:next,+2:report all events in period)
		  double *tend2000,   // report all events end time (in jd2000)
		  int *eventId,       // requested event id (SEE TABLE BELOW)
		  int *neventVal,     // number of event input data (SEE TABLE BELOW)
		  double *eventVal,   // event input data (SEE TABLE BELOW)
		  int *maxrep,        // maximum number of output reports
		  int *nrep,          // number of output reports
		  double *rep2000,    // time of output report (in jd2000)
		  int *repId,         // output report identification (SEE TABLE BELOW)
		  double *repVal,     // output report value (SEE TABLE BELOW)
		  char *rep250,       // output report string (redundant description)
		  int *secdec,        // number of second decimals used in output report string 
		  char *crc250,       // error return string
		  int *irc,          // error return code
		  int lenr,
		  int lenc);

  void astrostate_(
		   int *njd,           // number of dtg
		   char* dtg,          // dtg-array
		   double *lat,        // latitude
		   double *lon,        // longitude
		   double *hgt,        // height of observer
		   char *crc250,       // error return string
		   int *irc,           // error return code
		   int lend,
		   int lenc);

#ifdef __cplusplus
}
#endif

#endif
