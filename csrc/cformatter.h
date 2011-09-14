#ifndef __CFORMATTER__
#define __CFORMATTER__

char* formatEventInfo( int nup, int idprup, 
                       double xwgtup, double scalup, 
                       double aqedup, double aqcdup ); 


char* formatParticleInfo( int idup, int istup, 
                          int mothup[2], int icolup[2], 
                          double pup[5], double vtimup, double spinup ); 

#endif //__CFORMATTER__


