*NL2ITR
      SUBROUTINE NL2ITR (D, IV, J, N, NN, P, R, V, X)
C
C     LATEST REVISION  -  03/15/90  (JRD)
C
C
C  ***  CARRY OUT NL2SOL (NONLINEAR LEAST-SQUARES) ITERATIONS  ***
C  ***  (NL2SOL VERSION 2.2)  ***
C
C
C  VARIABLE DECLARATIONS
C
C  SCALAR ARGUMENTS
      INTEGER
     +   N,NN,P
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   D(P),J(NN,P),R(N),V(*),X(P)
      INTEGER
     +   IV(*)
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   E,HALF,NEGONE,ONE,RDOF1,STTSST,T,T1,ZERO
      INTEGER
     +   CNVCOD,COSMIN,COVMAT,COVPRT,COVREQ,D0INIT,DGNORM,DIG,DIG1,
     +   DINIT,DSTNRM,DTYPE,DUMMY,F,F0,FDIF,FUZZ,G,G01,G1,GTSTEP,H,
     +   H0,H1,I,IERR,IM1,INCFAC,INITS,IPIV0,IPIV1,IPIVI,IPIVK,
     +   IPIVOT,IPK,IRC,JTINIT,JTOL1,K,KAGQT,KALM,KM1,L,LKY,LKY1,
     +   LMAT,LMAT1,LMAX0,LSTGST,M,MODE,MODEL,MXFCAL,MXITER,NFCALL,
     +   NFCOV,NFGCAL,NGCALL,NGCOV,NITER,NVSAVE,PHMXFC,PP1O2,
     +   PREDUC,QTR,QTR1,RAD0,RADFAC,RADINC,RADIUS,RD,RD0,RD1,RDK,
     +   RESTOR,RLIMIT,RSAVE,RSAVE1,S,S1,SIZE,SMH,SSTEP,STEP,STEP1,
     +   STGLIM,STLSTG,STPMOD,STPPAR,SUSED,SWITCH,TEMP1,TEMP2,
     +   TOOBIG,TUNER4,TUNER5,VSAVE1,W,W1,WSCALE,X0,X01,XIRC
C
C  EXTERNAL FUNCTIONS
      DOUBLE PRECISION
     +   DOTPRD,D1MACH,V2NORM
      LOGICAL
     +   STOPX
      EXTERNAL DOTPRD,D1MACH,V2NORM,STOPX
C
C  EXTERNAL SUBROUTINES
      EXTERNAL ASSESS,COVCLC,DUPDAT,GQTSTP,ITSMRY,LMSTEP,PARCHK,QAPPLY,
     +   QRFACT,RPTMUL,SLUPDT,SLVMUL,VAXPY,VCOPY,VSCOPY
C
C  INTRINSIC FUNCTIONS
      INTRINSIC ABS,SQRT
C
C  ***  PARAMETER DECLARATIONS  ***
C
C     INTEGER IV(1), N, NN, P
C     DOUBLE PRECISION D(P), J(NN,P), R(N), V(1), X(P)
C     DIMENSION IV(60+P), V(93 + 2*N + P*(3*P+31)/2)
C
C
C--------------------------  PARAMETER USAGE  --------------------------
C
C D.... SCALE VECTOR.
C IV... INTEGER VALUE ARRAY.
C J.... N BY P JACOBIAN MATRIX (LEAD DIMENSION NN).
C N.... NUMBER OF OBSERVATIONS (COMPONENTS IN R).
C NN... LEAD DIMENSION OF J.
C P.... NUMBER OF PARAMETERS (COMPONENTS IN X).
C R.... RESIDUAL VECTOR.
C V.... FLOATING-POINT VALUE ARRAY.
C X.... PARAMETER VECTOR.
C
C  ***  DISCUSSION  ***
C
C        PARAMETERS IV, N, P, V, AND X ARE THE SAME AS THE CORRESPOND-
C     ING ONES TO NL2SOL (WHICH SEE), EXCEPT THAT V CAN BE SHORTER
C     (SINCE THE PART OF V THAT NL2SOL USES FOR STORING D, J, AND R IS
C     NOT NEEDED).  MOREOVER, COMPARED WITH NL2SOL, IV(1) MAY HAVE THE
C     TWO ADDITIONAL OUTPUT VALUES 1 AND 2, WHICH ARE EXPLAINED BELOW,
C     AS IS THE USE OF IV(TOOBIG) AND IV(NFGCAL).  THE VALUES IV(D),
C     IV(J), AND IV(R), WHICH ARE OUTPUT VALUES FROM NL2SOL (AND
C     NL2SNO), ARE NOT REFERENCED BY NL2ITR OR THE SUBROUTINES IT CALLS.
C        ON A FRESH START, I.E., A CALL ON NL2ITR WITH IV(1) = 0 OR 12,
C     NL2ITR ASSUMES THAT R = R(X), THE RESIDUAL AT X, AND J = J(X),
C     THE CORRESPONDING JACOBIAN MATRIX OF R AT X.
C
C IV(1) = 1 MEANS THE CALLER SHOULD SET R TO R(X), THE RESIDUAL AT X,
C             AND CALL NL2ITR AGAIN, HAVING CHANGED NONE OF THE OTHER
C             PARAMETERS.  AN EXCEPTION OCCURS IF R CANNOT BE EVALUATED
C             AT X (E.G. IF R WOULD OVERFLOW), WHICH MAY HAPPEN BECAUSE
C             OF AN OVERSIZED STEP.  IN THIS CASE THE CALLER SHOULD SET
C             IV(TOOBIG) = IV(2) TO 1, WHICH WILL CAUSE NL2ITR TO IG-
C             NORE R AND TRY A SMALLER STEP.  THE PARAMETER NF THAT
C             NL2SOL PASSES TO CALCR (FOR POSSIBLE USE BY CALCJ) IS A
C             COPY OF IV(NFCALL) = IV(6).
C IV(1) = 2 MEANS THE CALLER SHOULD SET J TO J(X), THE JACOBIAN MATRIX
C             OF R AT X, AND CALL NL2ITR AGAIN.  THE CALLER MAY CHANGE
C             D AT THIS TIME, BUT SHOULD NOT CHANGE ANY OF THE OTHER
C             PARAMETERS.  THE PARAMETER NF THAT NL2SOL PASSES TO
C             CALCJ IS IV(NFGCAL) = IV(7).  IF J CANNOT BE EVALUATED
C             AT X, THEN THE CALLER MAY SET IV(NFGCAL) TO 0, IN WHICH
C             CASE NL2ITR WILL RETURN WITH IV(1) = 15.
C
C  ***  GENERAL  ***
C
C     CODED BY DAVID M. GAY.
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C
C     MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989, AND
C     MCS-7906671.
C        (SEE NL2SOL FOR REFERENCES.)
C
C+++++++++++++++++++++++++++  DECLARATIONS  ++++++++++++++++++++++++++++
C
C  ***  LOCAL VARIABLES  ***
C
C     INTEGER DUMMY, DIG1, G1, G01, H0, H1, I, IM1, IPIVI, IPIVK, IPIV1,
C    1        IPK, K, KM1, L, LKY1, LMAT1, LSTGST, M, PP1O2, QTR1,
C    2        RDK, RD0, RD1, RSAVE1, SMH, SSTEP, STEP1, STPMOD, S1,
C    3        TEMP1, TEMP2, W1, X01
C     DOUBLE PRECISION E, RDOF1, STTSST, T, T1
C
C     ***  CONSTANTS  ***
C
C     DOUBLE PRECISION HALF, NEGONE, ONE, ZERO
C
C/
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***
C
C     EXTERNAL ASSESS, COVCLC, DOTPRD, DUPDAT, GQTSTP, ITSMRY, LMSTEP,
C    1         PARCHK, QAPPLY, QRFACT, RPTMUL, SLUPDT, SLVMUL, STOPX,
C    2         VAXPY, VCOPY, VSCOPY, V2NORM
C     LOGICAL STOPX
C     DOUBLE PRECISION DOTPRD, D1MACH, V2NORM
C
C ASSESS... ASSESSES CANDIDATE STEP.
C COVCLC... COMPUTES COVARIANCE MATRIX.
C DOTPRD... RETURNS INNER PRODUCT OF TWO VECTORS.
C DUPDAT... UPDATES SCALE VECTOR D.
C GQTSTP... COMPUTES GOLDFELD-QUANDT-TROTTER STEP (AUGMENTED MODEL).
C ITSMRY... PRINTS ITERATION SUMMARY AND INFO ABOUT INITIAL AND FINAL X.
C LMSTEP... COMPUTES LEVENBERG-MARQUARDT STEP (GAUSS-NEWTON MODEL).
C PARCHK... CHECKS VALIDITY OF INPUT IV AND V VALUES.
C QAPPLY... APPLIES ORTHOGONAL MATRIX Q FROM QRFACT TO A VECTOR.
C QRFACT... COMPUTES QR DECOMPOSITION OF A MATRIX VIA HOUSEHOLDER TRANS.
C RPTMUL... MULTIPLIES VECTOR BY THE R MATRIX (AND/OR ITS TRANSPOSE)
C             STORED BY QRFACT.
C SLUPDT... PERFORMS QUASI-NEWTON UPDATE ON COMPACTLY STORED LOWER TRI-
C             ANGLE OF A SYMMETRIC MATRIX.
C STOPX.... RETURNS .TRUE. IF THE BREAK KEY HAS BEEN PRESSED.
C VAXPY.... COMPUTES SCALAR TIMES ONE VECTOR PLUS ANOTHER.
C VCOPY.... COPIES ONE VECTOR TO ANOTHER.
C VSCOPY... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.
C V2NORM... RETURNS THE 2-NORM OF A VECTOR.
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
C     INTEGER CNVCOD, COSMIN, COVMAT, COVPRT, COVREQ, DGNORM, DIG,
C    1        DINIT, DSTNRM, DTYPE, D0INIT, F, FDIF, FUZZ,
C    2        F0, G, GTSTEP, H, IERR, INCFAC, INITS, IPIVOT, IPIV0, IRC,
C    3        JTINIT, JTOL1, KAGQT, KALM, LKY, LMAT, LMAX0, MODE, MODEL,
C    4        MXFCAL, MXITER, NFCALL, NFGCAL, NFCOV, NGCOV, NGCALL,
C    5        NITER, NVSAVE, PHMXFC, PREDUC, QTR, RADFAC, RADINC,
C    6        RADIUS, RAD0, RD, RESTOR, RLIMIT, RSAVE, S, SIZE, STEP,
C    7        STGLIM, STLSTG, STPPAR, SUSED, SWITCH, TOOBIG, TUNER4,
C    8        TUNER5, VSAVE1, W, WSCALE, XIRC, X0
C
C  ***  IV SUBSCRIPT VALUES  ***
C
      DATA CNVCOD/34/, COVMAT/26/, COVPRT/14/,
     +     COVREQ/15/, DIG/43/, DTYPE/16/, G/28/, H/44/,
     +     IERR/32/, INITS/25/, IPIVOT/61/, IPIV0/60/,
     +     IRC/3/, KAGQT/35/, KALM/36/, LKY/37/, LMAT/58/,
     +     MODE/38/, MODEL/5/, MXFCAL/17/, MXITER/18/,
     +     NFCALL/6/, NFGCAL/7/, NFCOV/40/, NGCOV/41/,
     +     NGCALL/30/, NITER/31/, QTR/49/,
     +     RADINC/8/, RD/51/, RESTOR/9/, RSAVE/52/, S/53/,
     +     STEP/55/, STGLIM/11/, STLSTG/56/, SUSED/57/,
     +     SWITCH/12/, TOOBIG/2/, W/59/, XIRC/13/, X0/60/
C
C  ***  V SUBSCRIPT VALUES  ***
C
      DATA COSMIN/43/, DGNORM/1/, DINIT/38/, DSTNRM/2/,
     +     D0INIT/37/, F/10/, FDIF/11/, FUZZ/45/,
     +     F0/13/, GTSTEP/4/, INCFAC/23/,
     +     JTINIT/39/, JTOL1/87/, LMAX0/35/,
     +     NVSAVE/9/, PHMXFC/21/, PREDUC/7/,
     +     RADFAC/16/, RADIUS/8/, RAD0/9/, RLIMIT/42/,
     +     SIZE/47/, STPPAR/5/, TUNER4/29/, TUNER5/30/,
     +     VSAVE1/78/, WSCALE/48/
C
C
      DATA HALF/0.5D0/, NEGONE/-1.0D0/, ONE/1.0D0/, ZERO/0.0D0/
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      I = IV(1)
      IF (I .EQ. 1) GO TO 20
      IF (I .EQ. 2) GO TO 50
C
C  ***  CHECK VALIDITY OF IV AND V INPUT VALUES  ***
C
C     ***  NOTE -- IF IV(1) = 0, THEN PARCHK CALLS DFAULT(IV, V)  ***
      CALL PARCHK(IV, N, NN, P, V)
      I = IV(1) - 2
      IF (I .GT. 10) GO TO 999
      GO TO (350, 350, 350, 350, 350, 350, 195, 160, 195, 10), I
C
C  ***  INITIALIZATION AND STORAGE ALLOCATION  ***
C
 10   IV(NITER) = 0
      IV(NFCALL) = 1
      IV(NGCALL) = 1
      IV(NFGCAL) = 1
      IV(MODE) = -1
      IV(STGLIM) = 2
      IV(TOOBIG) = 0
      IV(CNVCOD) = 0
      IV(COVMAT) = 0
      IV(NFCOV) = 0
      IV(NGCOV) = 0
      IV(KALM) = -1
      IV(RADINC) = 0
      IV(S) = JTOL1 + 2*P
      PP1O2 = P * (P + 1) / 2
      IV(X0) = IV(S) + PP1O2
      IV(STEP) = IV(X0) + P
      IV(STLSTG) = IV(STEP) + P
      IV(DIG) = IV(STLSTG) + P
      IV(G) = IV(DIG) + P
      IV(LKY) = IV(G) + P
      IV(RD) = IV(LKY) + P
      IV(RSAVE) = IV(RD) + P
      IV(QTR) = IV(RSAVE) + N
      IV(H) = IV(QTR) + N
      IV(W) = IV(H) + PP1O2
      IV(LMAT) = IV(W) + 4*P + 7
C     +++ LENGTH OF W = P*(P+9)/2 + 7.  LMAT IS CONTAINED IN W.
      IF (V(DINIT) .GE. ZERO) CALL VSCOPY(P, D, V(DINIT))
      IF (V(JTINIT) .GT. ZERO) CALL VSCOPY(P, V(JTOL1), V(JTINIT))
      I = JTOL1 + P
      IF (V(D0INIT) .GT. ZERO) CALL VSCOPY(P, V(I), V(D0INIT))
      V(RAD0) = ZERO
      V(STPPAR) = ZERO
      V(RADIUS) = V(LMAX0) / (ONE + V(PHMXFC))
C
C  ***  SET INITIAL MODEL AND S MATRIX  ***
C
      IV(MODEL) = 1
      IF (IV(INITS) .EQ. 2) IV(MODEL) = 2
      S1 = IV(S)
      IF (IV(INITS) .EQ. 0) CALL VSCOPY(PP1O2, V(S1), ZERO)
C
C  ***  COMPUTE FUNCTION VALUE (HALF THE SUM OF SQUARES)  ***
C
 20   T = V2NORM(N, R)
      IF (T .GT. V(RLIMIT)) IV(TOOBIG) = 1
      IF (IV(TOOBIG) .NE. 0) GO TO 30
      V(F) = 0.0
      IF (T.GT.SQRT(D1MACH(1))) V(F) = HALF * T**2
 30   IF (IV(MODE)) 40, 350, 730
C
 40   IF (IV(TOOBIG) .EQ. 0) GO TO 60
         IV(1) = 13
         GO TO 900
C
C  ***  MAKE SURE JACOBIAN COULD BE COMPUTED  ***
C
 50   IF (IV(NFGCAL) .NE. 0) GO TO 60
         IV(1) = 15
         GO TO 900
C
C  ***  COMPUTE GRADIENT  ***
C
 60   IV(KALM) = -1
      G1 = IV(G)
      DO 70 I = 1, P
         V(G1) = DOTPRD(N, R, J(1,I))
         G1 = G1 + 1
 70      CONTINUE
      IF (IV(MODE) .GT. 0) GO TO 710
C
C  ***  UPDATE D AND MAKE COPIES OF R FOR POSSIBLE USE LATER  ***
C
      IF (IV(DTYPE) .GT. 0) CALL DUPDAT(D, IV, J, N, NN, P, V)
      RSAVE1 = IV(RSAVE)
      CALL VCOPY(N, V(RSAVE1), R)
      QTR1 = IV(QTR)
      CALL VCOPY(N, V(QTR1), R)
C
C  ***  COMPUTE  D**-1 * GRADIENT  ***
C
      G1 = IV(G)
      DIG1 = IV(DIG)
      K = DIG1
      DO 80 I = 1, P
         V(K) = V(G1) / D(I)
         K = K + 1
         G1 = G1 + 1
 80      CONTINUE
      V(DGNORM) = V2NORM(P, V(DIG1))
C
      IF (IV(CNVCOD) .NE. 0) GO TO 700
      IF (IV(MODE) .EQ. 0) GO TO 570
      IV(MODE) = 0
C
C
C-----------------------------  MAIN LOOP  -----------------------------
C
C
C  ***  PRINT ITERATION SUMMARY, CHECK ITERATION LIMIT  ***
C
 150  CALL ITSMRY(D, IV, P, V, X)
 160  K = IV(NITER)
      IF (K .LT. IV(MXITER)) GO TO 170
         IV(1) = 10
         GO TO 900
 170  IV(NITER) = K + 1
C
C  ***  UPDATE RADIUS  ***
C
      IF (K .EQ. 0) GO TO 185
      STEP1 = IV(STEP)
      DO 180 I = 1, P
         V(STEP1) = D(I) * V(STEP1)
         STEP1 = STEP1 + 1
 180     CONTINUE
      STEP1 = IV(STEP)
      V(RADIUS) = V(RADFAC) * V2NORM(P, V(STEP1))
C
C  ***  INITIALIZE FOR START OF NEXT ITERATION  ***
C
 185  X01 = IV(X0)
      V(F0) = V(F)
      IV(KAGQT) = -1
      IV(IRC) = 4
      IV(H) = -ABS(IV(H))
      IV(SUSED) = IV(MODEL)
C
C     ***  COPY X TO X0  ***
C
      CALL VCOPY(P, V(X01), X)
C
C  ***  CHECK STOPX AND FUNCTION EVALUATION LIMIT  ***
C
 190  IF (.NOT. STOPX(DUMMY)) GO TO 200
         IV(1) = 11
         GO TO 205
C
C     ***  COME HERE WHEN RESTARTING AFTER FUNC. EVAL. LIMIT OR STOPX.
C
 195  IF (V(F) .GE. V(F0)) GO TO 200
         V(RADFAC) = ONE
         K = IV(NITER)
         GO TO 170
C
 200  IF (IV(NFCALL) .LT. IV(MXFCAL) + IV(NFCOV)) GO TO 210
         IV(1) = 9
 205     IF (V(F) .GE. V(F0)) GO TO 900
C
C        ***  IN CASE OF STOPX OR FUNCTION EVALUATION LIMIT WITH
C        ***  IMPROVED V(F), EVALUATE THE GRADIENT AT X.
C
              IV(CNVCOD) = IV(1)
              GO TO 560
C
C. . . . . . . . . . . . .  COMPUTE CANDIDATE STEP  . . . . . . . . . .
C
 210  STEP1 = IV(STEP)
      W1 = IV(W)
      IF (IV(MODEL) .EQ. 2) GO TO 240
C
C  ***  COMPUTE LEVENBERG-MARQUARDT STEP  ***
C
         QTR1 = IV(QTR)
         IF (IV(KALM) .GE. 0) GO TO 215
              RD1 = IV(RD)
              IF (-1 .EQ. IV(KALM)) CALL QRFACT(NN, N, P, J, V(RD1),
     +                                   IV(IPIVOT), IV(IERR), 0, V(W1))
              CALL QAPPLY(NN, N, P, J, V(QTR1), IV(IERR))
 215     H1 = IV(H)
         IF (H1 .GT. 0) GO TO 230
C
C        ***  COPY R MATRIX TO H  ***
C
              H1 = -H1
              IV(H) = H1
              K = H1
              RD1 = IV(RD)
              V(K) = V(RD1)
              IF (P .EQ. 1) GO TO 230
              DO 220 I = 2, P
                   CALL VCOPY(I-1, V(K+1), J(1,I))
                   K = K + I
                   RD1 = RD1 + 1
                   V(K) = V(RD1)
 220               CONTINUE
C
 230     G1 = IV(G)
         CALL LMSTEP(D, V(G1), IV(IERR), IV(IPIVOT), IV(KALM), P,
     +               V(QTR1), V(H1), V(STEP1), V, V(W1))
         GO TO 310
C
C  ***  COMPUTE GOLDFELD-QUANDT-TROTTER STEP (AUGMENTED MODEL)  ***
C
 240  IF (IV(H) .GT. 0) GO TO 300
C
C     ***  SET H TO  D**-1 * ( (J**T)*J + S) ) * D**-1.  ***
C
         H1 = -IV(H)
         IV(H) = H1
         S1 = IV(S)
         IF (IV(KALM) .GE. 0) GO TO 270
C
C        ***  J IS IN ITS ORIGINAL FORM  ***
C
              DO 260 I = 1, P
                   T = ONE / D(I)
                   DO 250 K = 1, I
                        V(H1) = T*(DOTPRD(N,J(1,I),J(1,K))+V(S1)) / D(K)
                        H1 = H1 + 1
                        S1 = S1 + 1
 250                    CONTINUE
 260               CONTINUE
              GO TO 300
C
C  ***  LMSTEP HAS APPLIED QRFACT TO J  ***
C
 270     SMH = S1 - H1
         H0 = H1 - 1
         IPIV1 = IV(IPIVOT)
         T1 = ONE / D(IPIV1)
         RD0 = IV(RD) - 1
         RDOF1 = V(RD0 + 1)
         DO 290 I = 1, P
              L = IPIV0 + I
              IPIVI = IV(L)
              H1 = H0 + IPIVI*(IPIVI-1)/2
              L = H1 + IPIVI
              M = L + SMH
C             ***  V(L) = H(IPIVOT(I), IPIVOT(I))  ***
C             ***  V(M) = S(IPIVOT(I), IPIVOT(I))  ***
              T = ONE / D(IPIVI)
              RDK = RD0 + I
              E = V(RDK)**2
              IF (I .GT. 1) E = E + DOTPRD(I-1, J(1,I), J(1,I))
              V(L) = (E + V(M)) * T**2
              IF (I .EQ. 1) GO TO 290
              L = H1 + IPIV1
              IF (IPIVI .LT. IPIV1) L = L +
     +                               ((IPIV1-IPIVI)*(IPIV1+IPIVI-3))/2
              M = L + SMH
C             ***  V(L) = H(IPIVOT(I), IPIVOT(1))  ***
C             ***  V(M) = S(IPIVOT(I), IPIVOT(1))  ***
              V(L) = T * (RDOF1 * J(1,I)  +  V(M)) * T1
              IF (I .EQ. 2) GO TO 290
              IM1 = I - 1
              DO 280 K = 2, IM1
                   IPK = IPIV0 + K
                   IPIVK = IV(IPK)
                   L = H1 + IPIVK
                   IF (IPIVI .LT. IPIVK) L = L +
     +                               ((IPIVK-IPIVI)*(IPIVK+IPIVI-3))/2
                   M = L + SMH
C                  ***  V(L) = H(IPIVOT(I), IPIVOT(K))  ***
C                  ***  V(M) = S(IPIVOT(I), IPIVOT(K))  ***
                   KM1 = K - 1
                   RDK = RD0 + K
                   V(L) = T * (DOTPRD(KM1, J(1,I), J(1,K)) +
     +                            V(RDK)*J(K,I) + V(M)) / D(IPIVK)
 280               CONTINUE
 290          CONTINUE
C
C  ***  COMPUTE ACTUAL GOLDFELD-QUANDT-TROTTER STEP  ***
C
 300  H1 = IV(H)
      DIG1 = IV(DIG)
      LMAT1 = IV(LMAT)
      CALL GQTSTP(D, V(DIG1), V(H1), IV(KAGQT), V(LMAT1), P, V(STEP1),
     +            V, V(W1))
C
C
C  ***  COMPUTE R(X0 + STEP)  ***
C
 310  IF (IV(IRC) .EQ. 6) GO TO 350
      X01 = IV(X0)
      STEP1 = IV(STEP)
      CALL VAXPY(P, X, ONE, V(STEP1), V(X01))
      IV(NFCALL) = IV(NFCALL) + 1
      IV(1) = 1
      IV(TOOBIG) = 0
      GO TO 999
C
C. . . . . . . . . . . . .  ASSESS CANDIDATE STEP  . . . . . . . . . . .
C
 350  STEP1 = IV(STEP)
      LSTGST = IV(STLSTG)
      X01 = IV(X0)
      CALL ASSESS(D, IV, P, V(STEP1), V(LSTGST), V, X, V(X01))
C
C  ***  IF NECESSARY, SWITCH MODELS AND/OR RESTORE R  ***
C
      IF (IV(SWITCH) .EQ. 0) GO TO 360
         IV(H) = -ABS(IV(H))
         IV(SUSED) = IV(SUSED) + 2
         CALL VCOPY(NVSAVE, V, V(VSAVE1))
 360  IF (IV(RESTOR) .EQ. 0) GO TO 390
         RSAVE1 = IV(RSAVE)
         CALL VCOPY(N, R, V(RSAVE1))
 390  L = IV(IRC) - 4
      STPMOD = IV(MODEL)
      IF (L .GT. 0) GO TO (410,440,450,450,450,450,450,450,640,570), L
C
C  ***  DECIDE WHETHER TO CHANGE MODELS  ***
C
      E = V(PREDUC) - V(FDIF)
      SSTEP = IV(LKY)
      S1 = IV(S)
      CALL SLVMUL(P, V(SSTEP), V(S1), V(STEP1))
      STTSST = HALF * DOTPRD(P, V(STEP1), V(SSTEP))
      IF (IV(MODEL) .EQ. 1) STTSST = -STTSST
      IF (ABS(E + STTSST) * V(FUZZ) .GE. ABS(E)) GO TO 400
C
C     ***  SWITCH MODELS  ***
C
         IV(MODEL) = 3 - IV(MODEL)
         IF (IV(MODEL) .EQ. 1) IV(KAGQT) = -1
         IF (IV(MODEL) .EQ. 2 .AND. IV(KALM) .GT. 0) IV(KALM) = 0
         IF (-2 .LT. L) GO TO 480
              IV(H) = -ABS(IV(H))
              IV(SUSED) = IV(SUSED) + 2
              CALL VCOPY(NVSAVE, V(VSAVE1), V)
              GO TO 420
C
 400  IF (-3 .LT. L) GO TO 480
C
C     ***  RECOMPUTE STEP WITH DECREASED RADIUS  ***
C
         V(RADIUS) = V(RADFAC) * V(DSTNRM)
         GO TO 190
C
C  ***  RECOMPUTE STEP, SAVING V VALUES AND R IF NECESSARY  ***
C
 410  V(RADIUS) = V(RADFAC) * V(DSTNRM)
 420  IF (V(F) .GE. V(F0)) GO TO 190
      RSAVE1 = IV(RSAVE)
      CALL VCOPY(N, V(RSAVE1), R)
      GO TO 190
C
C  ***  COMPUTE STEP OF LENGTH V(LMAX0) FOR SINGULAR CONVERGENCE TEST
C
 440  V(RADIUS) = V(LMAX0)
      GO TO 210
C
C  ***  CONVERGENCE OR FALSE CONVERGENCE  ***
C
 450  IV(CNVCOD) = L
      IF (V(F) .GE. V(F0)) GO TO 700
         IF (IV(XIRC) .EQ. 14) GO TO 700
              IV(XIRC) = 14
C
C. . . . . . . . . . . .  PROCESS ACCEPTABLE STEP  . . . . . . . . . . .
C
 480  IV(COVMAT) = 0
C
C  ***  SET  LKY = (J(X0)**T) * R(X)  ***
C
      LKY1 = IV(LKY)
      IF (IV(KALM) .GE. 0) GO TO 500
C
C     ***  JACOBIAN HAS NOT BEEN MODIFIED  ***
C
         DO 490 I = 1, P
              V(LKY1) = DOTPRD(N, J(1,I), R)
              LKY1 = LKY1 + 1
 490          CONTINUE
         GO TO 510
C
C  ***  QRFACT HAS BEEN APPLIED TO J.  STORE COPY OF R IN QTR AND  ***
C  ***  APPLY Q TO IT.                                             ***
C
 500  QTR1 = IV(QTR)
      CALL VCOPY(N, V(QTR1), R)
      CALL QAPPLY(NN, N, P, J, V(QTR1), IV(IERR))
C
C  ***  MULTIPLY TOP P-VECTOR IN QTR BY PERMUTED UPPER TRIANGLE    ***
C  ***  STORED BY QRFACT IN J AND RD.                              ***
C
      RD1 = IV(RD)
      TEMP1 = IV(STLSTG)
      CALL RPTMUL(3, IV(IPIVOT), J, NN, P, V(RD1), V(QTR1), V(LKY1),
     +            V(TEMP1))
C
C  ***  SEE WHETHER TO SET V(RADFAC) BY GRADIENT TESTS  ***
C
 510  IF (IV(IRC) .NE. 3) GO TO 560
         STEP1 = IV(STEP)
         TEMP1 = IV(STLSTG)
         TEMP2 = IV(X0)
C
C     ***  SET  TEMP1 = HESSIAN * STEP  FOR USE IN GRADIENT TESTS  ***
C
         IF (STPMOD .EQ. 2) GO TO 530
C
C        ***  STEP COMPUTED USING GAUSS-NEWTON MODEL  ***
C        ***  -- QRFACT HAS BEEN APPLIED TO J         ***
C
              RD1 = IV(RD)
              CALL RPTMUL(2, IV(IPIVOT), J, NN, P, V(RD1),
     +                    V(STEP1), V(TEMP1), V(TEMP2))
              GO TO 560
C
C     ***  STEP COMPUTED USING AUGMENTED MODEL  ***
C
 530     H1 = IV(H)
         K = TEMP2
         DO 540 I = 1, P
              V(K) = D(I) * V(STEP1)
              K = K + 1
              STEP1 = STEP1 + 1
 540          CONTINUE
         CALL SLVMUL(P, V(TEMP1), V(H1), V(TEMP2))
         DO 550 I = 1, P
              V(TEMP1) = D(I) * V(TEMP1)
              TEMP1 = TEMP1 + 1
 550          CONTINUE
C
C  ***  SAVE OLD GRADIENT AND COMPUTE NEW ONE  ***
C
 560  IV(NGCALL) = IV(NGCALL) + 1
      G1 = IV(G)
      G01 = IV(W)
      CALL VCOPY(P, V(G01), V(G1))
      IV(1) = 2
      GO TO 999
C
C  ***  INITIALIZATIONS -- G0 = G - G0, ETC.  ***
C
 570  G01 = IV(W)
      G1 = IV(G)
      CALL VAXPY(P, V(G01), NEGONE, V(G01), V(G1))
      STEP1 = IV(STEP)
      TEMP1 = IV(STLSTG)
      TEMP2 = IV(X0)
      IF (IV(IRC) .NE. 3) GO TO 600
C
C  ***  SET V(RADFAC) BY GRADIENT TESTS  ***
C
C     ***  SET  TEMP1 = D**-1 * (HESSIAN * STEP  +  (G(X0) - G(X)))  ***
C
         K = TEMP1
         L = G01
         DO 580 I = 1, P
              V(K) = (V(K) - V(L)) / D(I)
              K = K + 1
              L = L + 1
 580          CONTINUE
C
C        ***  DO GRADIENT TESTS  ***
C
         IF (V2NORM(P, V(TEMP1)) .LE. V(DGNORM) * V(TUNER4))  GO TO 590
              IF (DOTPRD(P, V(G1), V(STEP1))
     +                  .GE. V(GTSTEP) * V(TUNER5))  GO TO 600
 590               V(RADFAC) = V(INCFAC)
C
C  ***  FINISH COMPUTING LKY = ((J(X) - J(X0))**T) * R  ***
C
C     ***  CURRENTLY LKY = (J(X0)**T) * R  ***
C
 600  LKY1 = IV(LKY)
      CALL VAXPY(P, V(LKY1), NEGONE, V(LKY1), V(G1))
C
C  ***  DETERMINE SIZING FACTOR V(SIZE)  ***
C
C     ***  SET TEMP1 = S * STEP  ***
      S1 = IV(S)
      CALL SLVMUL(P, V(TEMP1), V(S1), V(STEP1))
C
      T1 = ABS(DOTPRD(P, V(STEP1), V(TEMP1)))
      T = ABS(DOTPRD(P, V(STEP1), V(LKY1)))
      V(SIZE) = ONE
      IF (T .LT. T1) V(SIZE) = T / T1
C
C  ***  UPDATE S  ***
C
      CALL SLUPDT(V(S1), V(COSMIN), P, V(SIZE), V(STEP1), V(TEMP1),
     +            V(TEMP2), V(G01), V(WSCALE), V(LKY1))
      IV(1) = 2
      GO TO 150
C
C. . . . . . . . . . . . . .  MISC. DETAILS  . . . . . . . . . . . . . .
C
C  ***  BAD PARAMETERS TO ASSESS  ***
C
 640  IV(1) = 14
      GO TO 900
C
C  ***  CONVERGENCE OBTAINED -- COMPUTE COVARIANCE MATRIX IF DESIRED ***
C
 700  IF (IV(COVREQ) .EQ. 0 .AND. IV(COVPRT) .EQ. 0) GO TO 760
      IF (IV(COVMAT) .NE. 0) GO TO 760
      IF (IV(CNVCOD) .GE. 7) GO TO 760
      IV(MODE) = 0
 710  CALL COVCLC(I, D, IV, J, N, NN, P, R, V, X)
      GO TO (720, 720, 740, 750), I
 720  IV(NFCOV) = IV(NFCOV) + 1
      IV(NFCALL) = IV(NFCALL) + 1
      IV(RESTOR) = I
      IV(1) = 1
      GO TO 999
C
 730  IF (IV(RESTOR) .EQ. 1 .OR. IV(TOOBIG) .NE. 0) GO TO 710
      IV(NFGCAL) = IV(NFCALL)
 740  IV(NGCOV) = IV(NGCOV) + 1
      IV(NGCALL) = IV(NGCALL) + 1
      IV(1) = 2
      GO TO 999
C
 750  IV(MODE) = 0
      IF (IV(NITER) .EQ. 0) IV(MODE) = -1
C
 760  IV(1) = IV(CNVCOD)
      IV(CNVCOD) = 0
C
C  ***  PRINT SUMMARY OF FINAL ITERATION AND OTHER REQUESTED ITEMS  ***
C
 900  CALL ITSMRY(D, IV, P, V, X)
C
 999  RETURN
C
C  ***  LAST CARD OF NL2ITR FOLLOWS  ***
      END
