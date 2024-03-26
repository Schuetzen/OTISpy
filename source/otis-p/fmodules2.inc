*****************************************************************************
*     fmodules2.inc: include file to set the maximum dimensions for STARPAC
*
*****************************************************************************
      INTEGER LDSTAK,MAXOBS,MAXPAR
      PARAMETER (MAXOBS = 200)
      PARAMETER (MAXPAR = 10)
      PARAMETER (LDSTAK = 27 + MAX(MAXOBS+MAXPAR,30+MAXPAR)
     &                       + MAX(10*MAXOBS,94+MAXOBS*(3+MAXPAR)
     &                       + (3*MAXPAR**2+37*MAXPAR)/2))
