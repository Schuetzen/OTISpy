*LIVMUL
      SUBROUTINE LIVMUL(N, X, L, Y)
C
C  ***  SOLVE  L*X = Y, WHERE  L  IS AN  N X N  LOWER TRIANGULAR
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME
C  ***  STORAGE.  ***
C
C
C  VARIABLE DECLARATIONS
C
C  SCALAR ARGUMENTS
      INTEGER
     +   N
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   L(1),X(N),Y(N)
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   T,ZERO
      INTEGER
     +   I,J,K
C
C  EXTERNAL FUNCTIONS
      DOUBLE PRECISION
     +   DOTPRD
      EXTERNAL DOTPRD
C
      DATA ZERO/0.0D0/
C
      DO 10 K = 1, N
         IF (Y(K) .NE. ZERO) GO TO 20
         X(K) = ZERO
 10      CONTINUE
      GO TO 999
 20   J = K*(K+1)/2
      X(K) = Y(K) / L(J)
      IF (K .GE. N) GO TO 999
      K = K + 1
      DO 30 I = K, N
         T = DOTPRD(I-1, L(J+1), X)
         J = J + I
         X(I) = (Y(I) - T)/L(J)
 30      CONTINUE
 999  RETURN
C  ***  LAST CARD OF LIVMUL FOLLOWS  ***
      END
