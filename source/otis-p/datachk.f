************************************************************************
*
*     Subroutine DATACHK             Called by: REACHEST
*
*     check the observed data to make sure it is valid.  The observed
*     data must adhere to the following rules:
*
*     1) the first observation must be at least one time step past the
*        simulation start time.
*     2) the observations must be entered in order of ascending time
*     3) the time between successive observations must be > time step
*
************************************************************************
      SUBROUTINE DATACHK(XM,I,TSTEP,TSTART)
*
*     subroutine arguments
*
      INTEGER*4 I
      DOUBLE PRECISION TSTEP,TSTART,XM(*)

      IF (I .EQ. 1) THEN
         IF (XM(1) .LE. TSTART+TSTEP) CALL ERROR5(1,XM(1),TSTART+TSTEP)
      ELSE
         IF (XM(I) .LT. XM(I-1)) CALL ERROR5(2,XM(I-1),XM(I))
         IF (XM(I) - XM(I-1) .LT. TSTEP) CALL ERROR5(3,XM(I-1),XM(I))
      ENDIF
      RETURN
      END
