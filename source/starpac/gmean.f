*GMEAN
      SUBROUTINE GMEAN(Y, N, YMEAN)
C
C     LATEST REVISION  -  03/15/90  (JRD)
C
C     THIS ROUTINE COMPUTES THE GEOMETRIC MEAN OF A SERIES, ASSUMING
C     ALL VALUES IN Y ARE NON-ZERO.
C
C     WRITTEN BY - JANET R. DONALDSON
C                  STATISTICAL ENGINEERING DIVISION
C                  NATIONAL BUREAU OF STANDARDS, BOULDER, COLORADO
C
C     CREATION DATE  -  APRIL 2, 1981
C
C
C  VARIABLE DECLARATIONS
C
C  SCALAR ARGUMENTS
      DOUBLE PRECISION
     +   YMEAN
      INTEGER
     +   N
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   Y(N)
C
C  LOCAL SCALARS
      INTEGER
     +   I
C
C  INTRINSIC FUNCTIONS
      INTRINSIC EXP,LOG
C
C     VARIABLE DEFINITIONS (ALPHABETICALLY)
C
C     INTEGER I
C        AN INDEX VARIABLE
C     INTEGER N
C        THE NUMBER OF OBSERVATIONS IN THE SERIES
C     DOUBLE PRECISION Y(N)
C        THE VECTOR CONTAINING THE OBSERVED SERIES
C     DOUBLE PRECISION YMEAN
C        THE GEOMETRIC MEAN OF THE OBSERVED SERIES
C
      YMEAN = 0.0D0
      DO 10 I = 1, N
         YMEAN = YMEAN + LOG(Y(I))
   10 CONTINUE
      YMEAN = EXP(YMEAN/N)
      RETURN
      END