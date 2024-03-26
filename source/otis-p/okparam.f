************************************************************************
*
*     Function OKPARAM              Called by: OTISRUN
*
************************************************************************
      LOGICAL FUNCTION OKPARAM(PAR,PV,N)
*
*     function arguments
*
      INTEGER N
      DOUBLE PRECISION PAR(*),PV(*)
*
*     local variables
*
      INTEGER I
      DOUBLE PRECISION PARMIN
      PARAMETER (PARMIN=1.D-70)
*
*     check the parameters to make sure realistic values are used.
*     If an unrealistic parameter set is proposed, discourage the 
*     optimizer by setting the predicted values to -9999999.99
*     & return without running OTIS.
*
*     The minimum for the Dispersion Coefficient (PAR(1)) is set to 0.01
*     so that lower values that may cause instability are avoided.
*
      IF (PAR(1) .LT. 0.01D0 .OR. PAR(2) .LT. PARMIN .OR.
     &    PAR(3) .LT. PARMIN .OR. PAR(4) .LT. 0.D0 .OR.
     &    PAR(5) .LT. 0.D0 .OR. PAR(6) .LT. 0.D0 .OR.
     &    PAR(7) .LT. 0.D0 .OR. PAR(8) .LT. 0.D0 .OR.
     &    PAR(9) .LT. 0.D0 .OR. PAR(10) .LT. 0.D0) THEN
         DO 10 I=1,N
            PV(I) = -9999999.99D0
 10      CONTINUE
         OKPARAM = .FALSE.
      ELSE
         OKPARAM = .TRUE.
      ENDIF

      RETURN
      END



