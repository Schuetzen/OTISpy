*FITPT1
      SUBROUTINE FITPT1(N, M, XM, Y, PV, SDPV, RES, SDRES, WT, IXM,
     +   NNZW, WEIGHT, IPTOUT)
C
C     LATEST REVISION  -  03/15/90  (JRD)
C
C     THIS SUBOUTINE PRINTS THE DATA SUMMARY FOR THE NONLINEAR
C     LEAST SQUARES SUBROUTINES.
C
C     WRITTEN BY  -  JANET R. DONALDSON
C                    STATISTICAL ENGINEERING DIVISION
C                    NATIONAL BUREAU OF STANDARDS, BOULDER, COLORADO
C
C     CREATION DATE  -  APRIL 2, 1981
C
C
C  VARIABLE DECLARATIONS
C
C  SCALAR ARGUMENTS
      INTEGER
     +   IPTOUT,IXM,M,N,NNZW
      LOGICAL
     +   WEIGHT
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   PV(N),RES(N),SDPV(N),SDRES(N),WT(N),XM(IXM,M),Y(N)
C
C  SCALARS IN COMMON
      INTEGER
     +   IERR
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   FPLM
      INTEGER
     +   I,IPRT,JCOL1,JCOLM,K,NMAX
C
C  EXTERNAL FUNCTIONS
      DOUBLE PRECISION
     +   D1MACH
      EXTERNAL D1MACH
C
C  EXTERNAL SUBROUTINES
      EXTERNAL IPRINT,OBSSUM
C
C  INTRINSIC FUNCTIONS
      INTRINSIC MIN
C
C  COMMON BLOCKS
      COMMON /ERRCHK/IERR
C
C     VARIABLE DEFINITIONS (ALPHABETICALLY)
C
C     DOUBLE PRECISION FPLM
C        THE FLOATING POINT LARGEST MAGNITUDE.
C     INTEGER I
C        AN INDEXING VARIABLE.
C     INTEGER IERR
C        THE INTEGER VALUE DESIGNATING WHETHER ANY ERRORS HAVE
C        BEEN DETECTED.
C        IF IERR .EQ. 0, NO ERRORS WERE DETECTED
C        IF IERR .NE. 0, ERRORS HAVE BEEN DETECTED.
C     INTEGER IPTOUT
C        THE VARIABLE USED TO CONTROL PRINTED OUTPUT.
C     INTEGER IXM
C        THE FIRST DIMENSION OF THE INDEPENDENT VARIABLE ARRAY.
C     INTEGER JCOLM
C        THE LAST COLUMN OF THE INDEPENDENT VARIABLE TO BE PRINTED.
C     INTEGER JCOL1
C        THE FIRST COLUMN OF THE INDEPENDENT VARIABLE TO BE PRINTED.
C     INTEGER K
C        AN INDEX VARIABLE.
C     INTEGER M
C        THE NUMBER OF INDEPENDENT VARIABLES.
C     INTEGER N
C        THE NUMBER OF OBSERVATIONS.
C     INTEGER NMAX
C        THE MAXIMUM NUMBER OF ROWS TO BE PRINTED.
C     INTEGER NNZW
C        THE NUMBER OF NON ZERO WEIGHTS.
C     DOUBLE PRECISION PV(N)
C        THE PREDICTED VALUE BASED ON THE CURRENT PARAMETER ESTIMATES
C     DOUBLE PRECISION RES(N)
C        THE RESIDUALS FROM THE FIT.
C     DOUBLE PRECISION SDPV(N)
C        THE STANDARD DEVIATION OF THE PREDICTED VALUE.
C     DOUBLE PRECISION SDRES(N)
C        THE STANDARD DEVIATIONS OF THE RESIDUALS.
C     LOGICAL WEIGHT
C        THE VARIABLE USED TO INDICATE WHETHER WEIGHTED ANALYSIS IS TO
C        BE PERFORMED (TRUE) OR NOT (FALSE).
C     DOUBLE PRECISION WT(N)
C        THE USER SUPPLIED WEIGHTS.
C     DOUBLE PRECISION XM(IXM,M)
C        THE ARRAY IN WHICH ONE ROW OF THE INDEPENDENT VARIABLE ARRAY
C        IS STORED.
C     DOUBLE PRECISION Y(N)
C        THE ARRAY OF THE DEPENDENT VARIABLE.
C
      FPLM = D1MACH(2)
C
      CALL IPRINT(IPRT)
C
      WRITE (IPRT,1100)
C
      IF (WEIGHT) THEN
         WRITE (IPRT,1010)
      ELSE
         WRITE (IPRT,1000)
      END IF
      WRITE (IPRT, 1110)
C
C     TEST WHETHER COLUMN VECTOR XM(*, 1) = VECTOR 1.0D0
C
      DO 10 I=1,N
         IF (XM(I,1).NE.1.0D0) GO TO 20
   10 CONTINUE
      GO TO 30
C
C     NOT A UNIT VECTOR
C
   20 JCOL1 = 1
      JCOLM = MIN(M,3)
      GO TO 40
C
C     UNIT VECTOR
C
   30 JCOLM = MIN(M,4)
      JCOL1 = MIN(2,JCOLM)
   40 K = JCOLM - JCOL1 + 1
C
      NMAX = N
      IF ((IPTOUT.EQ.1) .AND. (N.GE.45)) NMAX = MIN(N,40)
C
C     PRINT OBSERVATION SUMMARY
C
      CALL OBSSUM(N, M, XM, Y, PV, SDPV, RES, SDRES, WT, IXM,
     +   WEIGHT, K, 1, NMAX, JCOL1, JCOLM)
C
      IF (NMAX.GE.N) GO TO 200
C
      DO 195 I = 1, 3
C
         GO TO (160, 170, 180), K
  160    WRITE (IPRT,1120)
         GO TO 190
  170    WRITE (IPRT,1130)
         GO TO 190
  180    WRITE (IPRT,1140)
C
  190    CONTINUE
         WRITE (IPRT, 1150)
         IF (WEIGHT) WRITE (IPRT, 1160)
C
  195 CONTINUE
C
C     PRINT LAST LINE OF OUTPUT
C
      CALL OBSSUM(N, M, XM, Y, PV, SDPV, RES, SDRES, WT, IXM,
     +   WEIGHT, K, N, N, JCOL1, JCOLM)
C
  200 CONTINUE
C
      IF ((NNZW.LT.N) .AND. (IERR.EQ.0)) WRITE (IPRT, 1060)
      IF ((NNZW.LT.N) .AND. (IERR.EQ.4)) WRITE (IPRT, 1070)
      IF ((NNZW.EQ.N) .AND. (IERR.EQ.4)) WRITE (IPRT, 1080)
      IF ((IERR.GT.0) .AND. (IERR.NE.4)) WRITE (IPRT, 1090)
C
      RETURN
C
C     FORMAT STATEMENTS
C
 1000 FORMAT (/53X, 9HDEPENDENT, 7X, 9HPREDICTED, 5X, 12H STD DEV OF ,
     +   24X, 4HSTD /
     +   2X, 3HROW, 13X, 16HPREDICTOR VALUES, 20X, 8HVARIABLE, 8X,
     +   6H VALUE, 8X, 12HPRED VALUE  , 6X, 9HRESIDUAL , 8X, 3HRES)
 1010 FORMAT (/53X, 9HDEPENDENT, 7X, 9HPREDICTED, 5X, 12H STD DEV OF ,
     +   24X, 4HSTD /
     +   2X, 3HROW, 13X, 16HPREDICTOR VALUES, 20X, 8HVARIABLE, 8X,
     +   6H VALUE, 8X, 12HPRED VALUE  , 6X, 9HRESIDUAL , 8X, 3HRES,
     +   4X, 6HWEIGHT)
 1060 FORMAT (// 37H *  NC  -  VALUE NOT COMPUTED BECAUSE,
     +   20H THE WEIGHT IS ZERO.)
 1070 FORMAT (// 44H *  NC  -  VALUE NOT COMPUTED BECAUSE EITHER,
     +   53H THE WEIGHT OR THE STANDARD DEVIATION OF THE RESIDUAL,
     +   9H IS ZERO.)
 1080 FORMAT (// 37H *  NC  -  VALUE NOT COMPUTED BECAUSE,
     +   48H THE STANDARD DEVIATION OF THE RESIDUAL IS ZERO.)
 1090 FORMAT (// 29H *  NC  -  VALUE NOT COMPUTED,
     +   54H BECAUSE CONVERGENCE PROBLEMS PREVENTED THE COVARIANCE,
     +   28H MATRIX FROM BEING COMPUTED.)
 1100 FORMAT (//31H RESULTS FROM LEAST SQUARES FIT/ 1X, 31('-'))
 1110 FORMAT (' ')
 1120 FORMAT (4X, '.', 25X, '.')
 1130 FORMAT (4X, '.', 3X, 2(14X, '.'))
 1140 FORMAT (4X, '.', 10X, '.', 2(14X, '.'))
 1150 FORMAT ('+', 49X, 11X, '.', 3(15X, '.'), 11X, '.')
 1160 FORMAT ('+', 130X, '.')
      END