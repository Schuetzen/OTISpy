*VAXPY
      SUBROUTINE VAXPY(P, W, A, X, Y)
C
C  ***  SET W = A*X + Y  --  W, X, Y = P-VECTORS, A = SCALAR  ***
C
C
C  VARIABLE DECLARATIONS
C
C  SCALAR ARGUMENTS
      DOUBLE PRECISION
     +   A
      INTEGER
     +   P
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   W(*),X(*),Y(*)
C
C  LOCAL SCALARS
      INTEGER
     +   I
C
C
      DO 10 I = 1, P
 10      W(I) = A*X(I) + Y(I)
      RETURN
      END
