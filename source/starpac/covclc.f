*COVCLC
      SUBROUTINE COVCLC(COVIRC, D, IV, J, N, NN, P, R, V, X)
C
C     LATEST REVISION  -  03/15/90  (JRD)
C
C  ***  COMPUTE COVARIANCE MATRIX FOR NL2ITR (NL2SOL VERSION 2.2)  ***
C
C  ***  LET K = ABS(IV(COVREQ).  FOR K .LE. 2, A FINITE-DIFFERENCE
C  ***  HESSIAN H IS COMPUTED (USING FUNC. AND GRAD. VALUES IF
C  ***  IV(COVREQ) IS NONNEGATIVE, AND USING ONLY FUNC. VALUES IF
C  ***  IV(COVREQ) IS NEGATIVE).  FOR SCALE = 2*F(X) / MAX(1, N-P),
C  ***  WHERE 2*F(X) IS THE RESIDUAL SUM OF SQUARES, COVCLC COMPUTES...
C  ***             K = 0 OR 1...  SCALE * H**-1 * (J**T * J) * H**-1.
C  ***             K = 2...  SCALE * H**-1.
C  ***             K .GE. 3...  SCALE * (J**T * J)**-1.
C
C
C  VARIABLE DECLARATIONS
C
C  SCALAR ARGUMENTS
      INTEGER
     +   COVIRC,N,NN,P
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   D(P),J(NN,P),R(N),V(1),X(P)
      INTEGER
     +   IV(1)
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   DEL,HALF,NEGPT5,ONE,T,TWO,WK,ZERO
      INTEGER
     +   COV,COVMAT,COVREQ,DELTA,DELTA0,DLTFDC,F,FX,G,G1,GP,GSAVE1,
     +   H,HC,HMI,HPI,HPM,I,IERR,IP1,IPIV0,IPIVI,IPIVK,IPIVOT,IRC,
     +   K,KAGQT,KALM,KIND,KL,L,LMAT,M,MM1,MM1O2,MODE,NFGCAL,PP1O2,
     +   QTR,QTR1,RD,RD1,RSAVE,SAVEI,STP0,STPI,STPM,SWITCH,TOOBIG,
     +   W,W0,W1,WL,XMSAVE
      LOGICAL
     +   HAVEJ
C
C  EXTERNAL SUBROUTINES
      EXTERNAL LINVRT,LITVMU,LIVMUL,LSQRT,LTSQAR,QRFACT,VCOPY,VSCOPY
C
C  INTRINSIC FUNCTIONS
      INTRINSIC ABS,MAX
C
C  ***  PARAMETER DECLARATIONS  ***
C
C     INTEGER COVIRC, IV(1), N, NN, P
C     DOUBLE PRECISION D(P), J(NN,P), R(N), V(1), X(P)
C     DIMENSION IV(*), V(*)
C
C  ***  LOCAL VARIABLES  ***
C
C     LOGICAL HAVEJ
C     INTEGER COV, GP, GSAVE1, G1, HC, HMI, HPI, HPM, I, IPIVI, IPIVK,
C    1        IP1, IRC, K, KIND, KL, L, M, MM1, MM1O2, PP1O2, QTR1,
C    2        RD1, STPI, STPM, STP0, WL, W0, W1
C     DOUBLE PRECISION DEL, HALF, NEGPT5, ONE, T, TWO, WK, ZERO
C
C/
C  ***  EXTERNAL SUBROUTINES  ***
C
C     EXTERNAL LINVRT, LITVMU, LIVMUL, LSQRT, LTSQAR, QRFACT,
C    1         VCOPY, VSCOPY
C
C LINVRT... INVERT LOWER TRIANGULAR MATRIX.
C LITVMU... APPLY INVERSE-TRANSPOSE OF COMPACT LOWER TRIANG. MATRIX.
C LIVMUL... APPLY INVERSE OF COMPACT LOWER TRIANG. MATRIX.
C LSQRT.... COMPUTE CHOLESKY FACTOR OF (LOWER TRINAG. OF) A SYM. MATRIX.
C LTSQAR... GIVEN LOWER TRIANG. MATRIX L, COMPUTE (L**T)*L.
C QRFACT... COMPUTE QR DECOMPOSITION OF A MATRIX.
C VCOPY.... COPY ONE VECTOR TO ANOTHER.
C VSCOPY... SET ALL ELEMENTS OF A VECTOR TO A SCALAR.
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
C     INTEGER COVMAT, COVREQ, DELTA, DELTA0, DLTFDC, F, FX, G, H, IERR,
C    1        IPIVOT, IPIV0, KAGQT, KALM, LMAT, MODE, NFGCAL, QTR,
C    2        RD, RSAVE, SAVEI, SWITCH, TOOBIG, W, XMSAVE
C
      DATA HALF/0.5D0/, NEGPT5/-0.5D0/, ONE/1.0D0/, TWO/2.0D0/,
     +     ZERO/0.0D0/
C
      DATA COVMAT/26/, COVREQ/15/, DELTA/50/, DELTA0/44/,
     +     DLTFDC/40/, F/10/, FX/46/, G/28/, H/44/, IERR/32/,
     +     IPIVOT/61/, IPIV0/60/, KAGQT/35/, KALM/36/,
     +     LMAT/58/, MODE/38/, NFGCAL/7/, QTR/49/,
     +     RD/51/, RSAVE/52/, SAVEI/54/, SWITCH/12/,
     +     TOOBIG/2/, W/59/, XMSAVE/49/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      COV = IV(LMAT)
C
      COVIRC = 4
      KIND = IV(COVREQ)
      M = IV(MODE)
      IF (M .GT. 0) GO TO 10
         IV(KAGQT) = -1
         IF (IV(KALM) .GT. 0) IV(KALM) = 0
         IF (ABS(KIND) .GE. 3) GO TO 300
         V(FX) = V(F)
         K = IV(RSAVE)
         CALL VCOPY(N, V(K), R)
 10   IF (M .GT. P) GO TO 200
      IF (KIND .LT. 0) GO TO 100
C
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN USING BOTH FUNCTION AND
C  ***  GRADIENT VALUES.
C
      GSAVE1 = IV(W) + P
      G1 = IV(G)
      IF (M .GT. 0) GO TO 15
C        ***  FIRST CALL ON COVCLC.  SET GSAVE = G, TAKE FIRST STEP  ***
         CALL VCOPY(P, V(GSAVE1), V(G1))
         IV(SWITCH) = IV(NFGCAL)
         GO TO 80
C
 15   DEL = V(DELTA)
      X(M) = V(XMSAVE)
      IF (IV(TOOBIG) .EQ. 0) GO TO 30
C
C     ***  HANDLE OVERSIZE V(DELTA)  ***
C
         IF (DEL*X(M) .GT. ZERO) GO TO 20
C             ***  WE ALREADY TRIED SHRINKING V(DELTA), SO QUIT  ***
              IV(COVMAT) = -2
              GO TO 190
C
C        ***  TRY SHRINKING V(DELTA)  ***
 20      DEL = NEGPT5 * DEL
         GO TO 90
C
 30   COV = IV(LMAT)
      GP = G1 + P - 1
C
C  ***  SET  G = (G - GSAVE)/DEL  ***
C
      DO 40 I = G1, GP
         V(I) = (V(I) - V(GSAVE1)) / DEL
         GSAVE1 = GSAVE1 + 1
 40      CONTINUE
C
C  ***  ADD G AS NEW COL. TO FINITE-DIFF. HESSIAN MATRIX  ***
C
      K = COV + M*(M-1)/2
      L = K + M - 2
      IF ( M .EQ. 1) GO TO 60
C
C  ***  SET  H(I,M) = 0.5 * (H(I,M) + G(I))  FOR I = 1 TO M-1  ***
C
      DO 50 I = K, L
         V(I) = HALF * (V(I) + V(G1))
         G1 = G1 + 1
 50      CONTINUE
C
C  ***  ADD  H(I,M) = G(I)  FOR I = M TO P  ***
C
 60   L = L + 1
      DO 70 I = M, P
         V(L) = V(G1)
         L = L + I
         G1 = G1 + 1
 70      CONTINUE
C
 80   M = M + 1
      IV(MODE) = M
      IF (M .GT. P) GO TO 190
C
C  ***  CHOOSE NEXT FINITE-DIFFERENCE STEP, RETURN TO GET G THERE  ***
C
      DEL = V(DELTA0) * MAX(ONE/D(M), ABS(X(M)))
      IF (X(M) .LT. ZERO) DEL = -DEL
      V(XMSAVE) = X(M)
 90   X(M) = X(M) + DEL
      V(DELTA) = DEL
      COVIRC = 2
      GO TO 999
C
C  ***  COMPUTE FINITE-DIFFERENCE HESSIAN USING FUNCTION VALUES ONLY.
C
 100  STP0 = IV(W) + P - 1
      MM1 = M - 1
      MM1O2 = M*MM1/2
      IF (M .GT. 0) GO TO 105
C        ***  FIRST CALL ON COVCLC.  ***
         IV(SAVEI) = 0
         GO TO 180
C
 105  I = IV(SAVEI)
      IF (I .GT. 0) GO TO 160
      IF (IV(TOOBIG) .EQ. 0) GO TO 120
C
C     ***  HANDLE OVERSIZE STEP  ***
C
         STPM = STP0 + M
         DEL = V(STPM)
         IF (DEL*X(XMSAVE) .GT. ZERO) GO TO 110
C             ***  WE ALREADY TRIED SHRINKING THE STEP, SO QUIT  ***
              IV(COVMAT) = -2
              GO TO 999
C
C        ***  TRY SHRINKING THE STEP  ***
 110     DEL = NEGPT5 * DEL
         X(M) = X(XMSAVE) + DEL
         V(STPM) = DEL
         COVIRC = 1
         GO TO 999
C
C  ***  SAVE F(X + STP(M)*E(M)) IN H(P,M)  ***
C
 120  PP1O2 = P * (P-1) / 2
      COV = IV(LMAT)
      HPM = COV + PP1O2 + MM1
      V(HPM) = V(F)
C
C  ***  START COMPUTING ROW M OF THE FINITE-DIFFERENCE HESSIAN H.  ***
C
      HMI = COV + MM1O2
      IF (MM1 .EQ. 0) GO TO 140
      HPI = COV + PP1O2
      DO 130 I = 1, MM1
         V(HMI) = V(FX) - (V(F) + V(HPI))
         HMI = HMI + 1
         HPI = HPI + 1
 130     CONTINUE
 140  V(HMI) = V(F) - TWO*V(FX)
C
C  ***  COMPUTE FUNCTION VALUES NEEDED TO COMPLETE ROW M OF H.  ***
C
      I = 1
C
 150  IV(SAVEI) = I
      STPI = STP0 + I
      V(DELTA) = X(I)
      X(I) = X(I) + V(STPI)
      IF (I .EQ. M) X(I) = V(XMSAVE) - V(STPI)
      COVIRC = 1
      GO TO 999
C
 160  X(I) = V(DELTA)
      IF (IV(TOOBIG) .EQ. 0) GO TO 170
C        ***  PUNT IN THE EVENT OF AN OVERSIZE STEP  ***
         IV(COVMAT) = -2
         GO TO 999
C
C  ***  FINISH COMPUTING H(M,I)  ***
C
 170  STPI = STP0 + I
      HMI = COV + MM1O2 + I - 1
      STPM = STP0 + M
      V(HMI) = (V(HMI) + V(F)) / (V(STPI)*V(STPM))
      I = I + 1
      IF (I .LE. M) GO TO 150
      IV(SAVEI) = 0
      X(M) = V(XMSAVE)
C
 180  M = M + 1
      IV(MODE) = M
      IF (M .GT. P) GO TO 190
C
C  ***  PREPARE TO COMPUTE ROW M OF THE FINITE-DIFFERENCE HESSIAN H.
C  ***  COMPUTE M-TH STEP SIZE STP(M), THEN RETURN TO OBTAIN
C  ***  F(X + STP(M)*E(M)), WHERE E(M) = M-TH STD. UNIT VECTOR.
C
      DEL = V(DLTFDC) * MAX(ONE/D(M), ABS(X(M)))
      IF (X(M) .LT. ZERO) DEL = -DEL
      V(XMSAVE) = X(M)
      X(M) = X(M) + DEL
      STPM = STP0 + M
      V(STPM) = DEL
      COVIRC = 1
      GO TO 999
C
C  ***  RESTORE R, V(F), ETC.  ***
C
 190  K = IV(RSAVE)
      CALL VCOPY(N, R, V(K))
      V(F) = V(FX)
      IF (KIND .LT. 0) GO TO 200
         IV(NFGCAL) = IV(SWITCH)
         QTR1 = IV(QTR)
         CALL VCOPY(N, V(QTR1), R)
         IF (IV(COVMAT) .LT. 0) GO TO 999
         COVIRC = 3
         GO TO 999
C
 200  COV = IV(LMAT)
C
C  ***  THE COMPLETE FINITE-DIFF. HESSIAN IS NOW STORED AT V(COV).   ***
C  ***  USE IT TO COMPUTE THE REQUESTED COVARIANCE MATRIX.           ***
C
C     ***  COMPUTE CHOLESKY FACTOR C OF H = C*(C**T)  ***
C     ***  AND STORE IT AT V(HC).  ***
C
      HC = COV
      IF (ABS(KIND) .EQ. 2) GO TO 210
         HC = ABS(IV(H))
         IV(H) = -HC
 210  CALL LSQRT(1, P, V(HC), V(COV), IRC)
      IV(COVMAT) = -1
      IF (IRC .NE. 0) GO TO 999
C
      W1 = IV(W) + P
      IF (ABS(KIND) .GT. 1) GO TO 350
C
C  ***  COVARIANCE = SCALE * H**-1 * (J**T * J) * H**-1  ***
C
      CALL VSCOPY(P*(P+1)/2, V(COV), ZERO)
      HAVEJ = IV(KALM) .EQ. (-1)
C     ***  HAVEJ = .TRUE. MEANS J IS IN ITS ORIGINAL FORM, WHILE
C     ***  HAVEJ = .FALSE. MEANS QRFACT HAS BEEN APPLIED TO J.
C
      M = P
      IF (HAVEJ) M = N
      W0 = W1 - 1
      RD1 = IV(RD)
      DO 290 I = 1, M
         IF (HAVEJ) GO TO 240
C
C        ***  SET W = IPIVOT * (ROW I OF R MATRIX FROM QRFACT).  ***
C
              CALL VSCOPY(P, V(W1), ZERO)
              IPIVI = IPIV0 + I
              L = W0 + IV(IPIVI)
              V(L) = V(RD1)
              RD1 = RD1 + 1
              IF (I .EQ. P) GO TO 260
              IP1 = I + 1
              DO 230 K = IP1, P
                   IPIVK = IPIV0 + K
                   L = W0 + IV(IPIVK)
                   V(L) = J(I,K)
 230               CONTINUE
              GO TO 260
C
C        ***  SET W = (ROW I OF J).  ***
C
 240     L = W0
         DO 250 K = 1, P
              L = L + 1
              V(L) = J(I,K)
 250          CONTINUE
C
C        ***  SET W = H**-1 * W.  ***
C
 260     CALL LIVMUL(P, V(W1), V(HC), V(W1))
         CALL LITVMU(P, V(W1), V(HC), V(W1))
C
C        ***  ADD  W * W**T  TO COVARIANCE MATRIX.  ***
C
         KL = COV
         DO 280 K = 1, P
              L = W0 + K
              WK = V(L)
              DO 270 L = 1, K
                   WL = W0 + L
                   V(KL) = V(KL)  +  WK * V(WL)
                   KL = KL + 1
 270               CONTINUE
 280          CONTINUE
 290     CONTINUE
      GO TO 380
C
C  ***  COVARIANCE = SCALE * (J**T * J)**-1.  ***
C
 300  RD1 = IV(RD)
      IF (IV(KALM) .NE. (-1)) GO TO 310
C
C        ***  APPLY QRFACT TO J  ***
C
         QTR1 = IV(QTR)
         CALL VCOPY(N, V(QTR1), R)
         W1 = IV(W) + P
         CALL QRFACT(NN, N, P, J, V(RD1), IV(IPIVOT), IV(IERR), 0,
     +               V(W1))
         IV(KALM) = -2
 310  IV(COVMAT) = -1
      IF (IV(IERR) .NE. 0) GO TO 999
      COV = IV(LMAT)
      HC = ABS(IV(H))
      IV(H) = -HC
C
C     ***  SET HC = (R MATRIX FROM QRFACT).  ***
C
      L = HC
      DO 340 I = 1, P
         IF (I .GT. 1) CALL VCOPY(I-1, V(L), J(1,I))
         L = L + I - 1
         V(L) = V(RD1)
         L = L + 1
         RD1 = RD1 + 1
 340     CONTINUE
C
C  ***  THE CHOLESKY FACTOR C OF THE UNSCALED INVERSE COVARIANCE MATRIX
C  ***  (OR PERMUTATION THEREOF) IS STORED AT V(HC).
C
C  ***  SET C = C**-1.
C
 350  CALL LINVRT(P, V(HC), V(HC))
C
C  ***  SET C = C**T * C.
C
      CALL LTSQAR(P, V(HC), V(HC))
C
      IF (HC .EQ. COV) GO TO 380
C
C     ***  C = PERMUTED, UNSCALED COVARIANCE.
C     ***  SET COV = IPIVOT * C * IPIVOT**T.
C
         DO 370 I = 1, P
              M = IPIV0 + I
              IPIVI = IV(M)
              KL = COV-1 + IPIVI*(IPIVI-1)/2
              DO 360 K = 1, I
                   M = IPIV0 + K
                   IPIVK = IV(M)
                   L = KL + IPIVK
                   IF (IPIVK .GT. IPIVI)
     +                       L = L + (IPIVK-IPIVI)*(IPIVK+IPIVI-3)/2
                   V(L) = V(HC)
                   HC = HC + 1
 360               CONTINUE
 370          CONTINUE
C
 380  IV(COVMAT) = COV
C
C  ***  APPLY SCALE FACTOR = (RESID. SUM OF SQUARES) / MAX(1,N-P).
C
      T = V(F) / (HALF * MAX(1,N-P))
      K = COV - 1 + P*(P+1)/2
      DO 390 I = COV, K
 390     V(I) = T * V(I)
C
 999  RETURN
C  ***  LAST CARD OF COVCLC FOLLOWS  ***
      END
