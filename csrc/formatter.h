// formatter.h

#ifndef __FORMATTER__
#define __FORMATTER__

void prtevt_( char* buf, int* nup, int* idprup, 
              double* xwgtup, double* scalup, 
              double* aqedup, double* aqcdup ); 

void prtptl_( char* buf, int* idup, int* istup, 
              int mothup[2], int icolup[2], 
              double pup[5], double* vtimup, double* spinup );

#endif // __FORMATTER__
