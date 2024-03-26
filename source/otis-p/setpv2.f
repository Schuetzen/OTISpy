************************************************************************
*
*     Subroutine SETPV2             Called by: OTISRUN
*
*     Set PV equal to the steady-state concentration at each distance
*     that corresponds an observation.
*
************************************************************************
      SUBROUTINE SETPV2(N,PV,CONC,INDEXPV,WTPV)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
      INCLUDE 'fmodules2.inc'
*
*     subroutine arguments
*
      INTEGER*4 N,INDEXPV(*)
      DOUBLE PRECISION PV(*),WTPV(*),CONC(MAXSEG,*)
*
*     local variables
*
      INTEGER*4 I,J
*
*     set PV equal to the steady-state concentration at each distance
*
      DO 10 J=1,N
         I = INDEXPV(J)
         PV(J) = CONC(I,1) + WTPV(J) * (CONC(I+1,1) - CONC(I,1))
 10   CONTINUE

      RETURN
      END
