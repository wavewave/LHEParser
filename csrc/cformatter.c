#include "formatter.h"

#include <string.h>
#include <stdlib.h>

char* formatEventInfo( int nup, int idprup, 
                       double xwgtup, double scalup, 
                       double aqedup, double aqcdup ) 
{
  char buf[200] ;
  prtevt_ (buf,&nup,&idprup,&xwgtup,&scalup,&aqedup,&aqcdup); 
  
  char* dest;
  dest = (char*) malloc(strlen(buf)+1);
  strcpy(dest,buf);
  return dest;  
}

char* formatParticleInfo( int idup, int istup, 
                          int mothup[2], int icolup[2], 
                          double pup[5], double vtimup, double spinup ) 
{
  char buf[200] ;
  prtptl_ (buf,&idup,&istup,mothup,icolup,pup,&vtimup,&spinup);

  char* dest;
  dest = (char*) malloc(strlen(buf)+1);
  strcpy(dest,buf);
  return dest;  
}

