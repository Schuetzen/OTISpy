*LINVRT
      SUBROUTINE LINVRT(N, LIN, L)
C
C  ***  COMPUTE  LIN = L**-1,  BOTH  N X N  LOWER TRIANG. STORED   ***
C  ***  COMPACTLY BY ROWS.  LIN AND L MAY SHARE THE SAME STORAGE.  ***
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
     +   L(*),LIN(*)
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   ONE,T,ZERO
      INTEGER
     +   I,II,IM1,J0,J1,JJ,K,K0,NP1
C
C  ***  PARAMETERS  ***
C
C     INTEGER N
C     DOUBLE PRECISION L(*), LIN(*)
C     DIMENSION L(N*(N+1)/2), LIN(N*(N+1)/2)
C
C  ***  LOCAL VARIABLES  ***
C
C     INTEGER I, II, IM1, JJ, J0, J1, K, K0, NP1
C     DOUBLE PRECISION ONE, T, ZERO
      DATA ONE/1.0D0/, ZERO/0.0D0/
C
C  ***  BODY  ***
C
      NP1 = N + 1
      J0 = N*(NP1)/2
      DO 30 II = 1, N
         I = NP1 - II
         LIN(J0) = ONE/L(J0)
         IF (I .LE. 1) GO TO 999
         J1 = J0
         IM1 = I - 1
         DO 20 JJ = 1, IM1
              T = ZERO
              J0 = J1
              K0 = J1 - JJ
              DO 10 K = 1, JJ
                   T = T - L(K0)*LIN(J0)
                   J0 = J0 - 1
                   K0 = K0 + K - I
 10                CONTINUE
              LIN(J0) = T/L(K0)
 20           CONTINUE
         J0 = J0 - 1
 30      CONTINUE
 999  RETURN
C  ***  LAST CARD OF LINVRT FOLLOWS  ***
      END
