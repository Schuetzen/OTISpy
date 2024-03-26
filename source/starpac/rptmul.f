*RPTMUL
      SUBROUTINE RPTMUL(FUNC, IPIVOT, J, NN, P, RD, X, Y, Z)
C
C  ***  FUNC = 1... SET  Y = RMAT * (PERM**T) * X.
C  ***  FUNC = 2... SET  Y = PERM * (RMAT**T) * RMAT * (PERM**T) * X.
C  ***  FUNC = 3... SET  Y = PERM * (RMAT**T) X.
C
C
C  ***  PERM = MATRIX WHOSE I-TH COL. IS THE IPIVOT(I)-TH UNIT VECTOR.
C  ***  RMAT IS THE UPPER TRIANGULAR MATRIX WHOSE STRICT UPPER TRIANGLE
C  ***       IS STORED IN  J  AND WHOSE DIAGONAL IS STORED IN RD.
C  ***  Z IS A SCRATCH VECTOR.
C  ***  X AND Y MAY SHARE STORAGE.
C
C
C  VARIABLE DECLARATIONS
C
C  SCALAR ARGUMENTS
      INTEGER
     +   FUNC,NN,P
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   J(NN,P),RD(P),X(P),Y(P),Z(P)
      INTEGER
     +   IPIVOT(P)
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   ZK
      INTEGER
     +   I,IM1,K,KM1
C
C  EXTERNAL FUNCTIONS
      DOUBLE PRECISION
     +   DOTPRD
      EXTERNAL DOTPRD
C
C
C-----------------------------------------------------------------------
C
      IF (FUNC .GT. 2) GO TO 50
C
C  ***  FIRST SET  Z = (PERM**T) * X  ***
C
      DO 10 I = 1, P
         K = IPIVOT(I)
         Z(I) = X(K)
 10      CONTINUE
C
C  ***  NOW SET  Y = RMAT * Z  ***
C
      Y(1) = Z(1) * RD(1)
      IF (P .LE. 1) GO TO 40
      DO 30 K = 2, P
         KM1 = K - 1
         ZK = Z(K)
         DO 20 I = 1, KM1
 20           Y(I) = Y(I) + J(I,K)*ZK
         Y(K) = ZK*RD(K)
 30      CONTINUE
C
 40   IF (FUNC .LE. 1) GO TO 999
      GO TO 70
C
 50   DO 60 I = 1, P
 60      Y(I) = X(I)
C
C  ***  SET  Z = (RMAT**T) * Y  ***
C
 70   Z(1) = Y(1) * RD(1)
      IF (P .EQ. 1) GO TO 90
      DO 80 I = 2, P
         IM1 = I - 1
         Z(I) = Y(I)*RD(I) + DOTPRD(IM1, J(1,I), Y)
 80      CONTINUE
C
C  ***  NOW SET  Y = PERM * Z  ***
C
 90   DO 100 I = 1, P
         K = IPIVOT(I)
         Y(K) = Z(I)
 100     CONTINUE
C
 999  RETURN
C  ***  LAST CARD OF RPTMUL FOLLOWS  ***
      END
