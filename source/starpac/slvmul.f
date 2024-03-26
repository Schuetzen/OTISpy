*SLVMUL
      SUBROUTINE SLVMUL(P, Y, S, X)
C
C  ***  SET  Y = S * X,  S = P X P SYMMETRIC MATRIX.  ***
C  ***  LOWER TRIANGLE OF  S  STORED ROWWISE.         ***
C
C
C  VARIABLE DECLARATIONS
C
C  SCALAR ARGUMENTS
      INTEGER
     +   P
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   S(1),X(P),Y(P)
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   XI
      INTEGER
     +   I,IM1,J,K
C
C  EXTERNAL FUNCTIONS
      DOUBLE PRECISION
     +   DOTPRD
      EXTERNAL DOTPRD
C
C  ***  PARAMETER DECLARATIONS  ***
C
C     INTEGER P
C     DOUBLE PRECISION S(1), X(P), Y(P)
C     DIMENSION S(P*(P+1)/2)
C
C  ***  LOCAL VARIABLES  ***
C
C     INTEGER I, IM1, J, K
C     DOUBLE PRECISION XI
C
C  ***  EXTERNAL FUNCTION  ***
C
C     EXTERNAL DOTPRD
C     DOUBLE PRECISION DOTPRD
C
C-----------------------------------------------------------------------
C
      J = 1
      DO 10 I = 1, P
         Y(I) = DOTPRD(I, S(J), X)
         J = J + I
 10      CONTINUE
C
      IF (P .LE. 1) GO TO 999
      J = 1
      DO 40 I = 2, P
         XI = X(I)
         IM1 = I - 1
         J = J + 1
         DO 30 K = 1, IM1
              Y(K) = Y(K) + S(J)*XI
              J = J + 1
 30           CONTINUE
 40      CONTINUE
C
 999  RETURN
C  ***  LAST CARD OF SLVMUL FOLLOWS  ***
      END
