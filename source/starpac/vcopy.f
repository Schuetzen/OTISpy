*VCOPY
      SUBROUTINE VCOPY(P, Y, X)
C
C  ***  SET Y = X, WHERE X AND Y ARE P-VECTORS  ***
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
     +   X(*),Y(*)
C
C  LOCAL SCALARS
      INTEGER
     +   I
C
C
      DO 10 I = 1, P
 10      Y(I) = X(I)
      RETURN
      END