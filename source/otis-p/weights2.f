************************************************************************
*
*     Subroutine WEIGHTS2            Called by: REACHEST
*
*     For the case of steady-state parameter estimation, determine the
*     weights used to interpolate linearly between segment centroids.
*     The linear interpolation formula used is of the form:
*
*     Conc@XM = Conc@I + WTPV * [ Conc@I+1 - Conc@I ]
*
*          where
*                 WTPV = [ XM - DIST(I) ] / [ DIST(I+1) - DIST(I) ]
*
*     Variables
*     ---------
*     XM(J)     distance at which results are needed to set PV
*     WTPV(J)   interpolation weight corresponding to XM(J)
*     INDEXPV   index corresponding to XM(J) denoting segment I
*     DIST(I)   distance at the center of segment I
*
************************************************************************
      SUBROUTINE WEIGHTS2(IMAX,INDEXPV,WTPV,XM,DIST,IOBS)

      INCLUDE 'fmodules.inc'
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 IMAX,IOBS,INDEXPV(*)
      DOUBLE PRECISION WTPV(*),XM(*),DIST(*)
*
*     local variable
*
      INTEGER*4 J
*
*     format statement
*
 100  FORMAT(//,10X,'WARNING: Distance Specified (',F14.5,
     &       ')',/,10X,'is less than distance of the first segment.',
     &       //,10X,'Distance set to ',F14.5,/)
*
*     Error Checks
*
*     1) Test to make sure that the specified distance does not exceed
*     the distance at the end of the stream.
*     2) Make sure the specified distance is not in the upstream half of
*     the first segment. If it is, set it equal to the distance at the
*     center of the first segment.
*
      IF (XM(IOBS) .GT. DIST(IMAX))
     &   CALL ERROR5(4,XM(IOBS),DIST(IMAX))
      IF (XM(IOBS) .LT. DIST(1)) THEN
         WRITE(LDECHO,100) XM(IOBS),DIST(1)
         XM(IOBS) = DIST(1)
      ENDIF
*
*     Sequentially search through the vector of distances until the
*     specified distance falls between two of the vector elements (i.e.
*     the specified distance is between the centers of two adjacent
*     segments).  Note that the test interval is inclusive [.GE. DIST(J)
*     and .LE. DIST(J+1)] to avoid searching past the last segment. Set
*     INDEXPV equal to the number of the upstream segment and compute
*     the interpolation weight, WTPV.
*
      J = 1

 10   IF ((XM(IOBS).GE.DIST(J)).AND.(XM(IOBS).LE.DIST(J+1))) THEN
         INDEXPV(IOBS) = J
         WTPV(IOBS) = (XM(IOBS) - DIST(J)) / (DIST(J+1) - DIST(J))
         J = 1
      ELSE
         J = J + 1
         GOTO 10
      ENDIF

      RETURN
      END
