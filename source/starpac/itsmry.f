*ITSMRY
      SUBROUTINE ITSMRY(D, IV, P, V, X)
C
C     LATEST REVISION  -  03/15/90  (JRD)
C
C  ***  PRINT NL2SOL (VERSION 2.2) ITERATION SUMMARY  ***
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
     +   D(P),V(*),X(P)
      INTEGER
     +   IV(*)
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   NRELDF,OLDF,PRELDF,RELDF,ZERO
      INTEGER
     +   COV1,COVMAT,COVPRT,COVREQ,DSTNRM,F,F0,FDIF,G,G1,I,I1,ICH,
     +   II,IV1,J,M,NEEDHD,NF,NFCALL,NFCOV,NG,NGCALL,NGCOV,NITER,
     +   NREDUC,OL,OUTLEV,PREDUC,PRNTIT,PRUNIT,PU,RELDX,SIZE,
     +   SOLPRT,STATPR,STPPAR,SUSED,X0PRT
C
C  LOCAL ARRAYS
      CHARACTER
     +   MODEL1(3,6)*1,MODEL2(4,6)*1
C
C  INTRINSIC FUNCTIONS
      INTRINSIC ABS
C
C  ***  PARAMETER DECLARATIONS  ***
C
C     INTEGER IV(1), P
C     DOUBLE PRECISION D(P), V(1), X(P)
C     DIMENSION IV(*), V(*)
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  ***  LOCAL VARIABLES  ***
C
C     INTEGER COV1, G1, I, II, IV1, I1, J, M, NF, NG, OL, PU
C     CHARACTER*1 MODEL1(3, 6), MODEL2(4, 6)
C     DOUBLE PRECISION NRELDF, OLDF, PRELDF, RELDF, ZERO
C
C/
C  ***  NO EXTERNAL FUNCTIONS OR SUBROUTINES  ***
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
C     INTEGER COVMAT, COVPRT, COVREQ, DSTNRM, F, FDIF, F0, G,
C    1        NEEDHD, NFCALL, NFCOV, NGCOV, NGCALL, NITER, NREDUC,
C    2        OUTLEV, PREDUC, PRNTIT, PRUNIT, RELDX, SIZE, SOLPRT,
C    3        STATPR, STPPAR, SUSED, X0PRT
C
C  ***  IV SUBSCRIPT VALUES  ***
C
      DATA COVMAT/26/, COVPRT/14/, G/28/, COVREQ/15/,
     +     NEEDHD/39/, NFCALL/6/, NFCOV/40/, NGCOV/41/,
     +     NGCALL/30/, NITER/31/, OUTLEV/19/, PRNTIT/48/,
     +     PRUNIT/21/, SOLPRT/22/, STATPR/23/, SUSED/57/,
     +     X0PRT/24/
C
C  ***  V SUBSCRIPT VALUES  ***
C
      DATA DSTNRM/2/, F/10/, F0/13/, FDIF/11/, NREDUC/6/,
     +     PREDUC/7/, RELDX/17/, SIZE/47/, STPPAR/5/
C
      DATA MODEL1(1, 1), MODEL1(2, 1), MODEL1(3, 1)
     +   /        ' ',          ' ',          ' '  /
      DATA MODEL1(1, 2), MODEL1(2, 2), MODEL1(3, 2)
     +   /        ' ',          ' ',          ' '  /
      DATA MODEL1(1, 3), MODEL1(2, 3), MODEL1(3, 3)
     +   /        ' ',          ' ',          ' '  /
      DATA MODEL1(1, 4), MODEL1(2, 4), MODEL1(3, 4)
     +   /        ' ',          ' ',          ' '  /
      DATA MODEL1(1, 5), MODEL1(2, 5), MODEL1(3, 5)
     +   /        ' ',          'G',          ' '  /
      DATA MODEL1(1, 6), MODEL1(2, 6), MODEL1(3, 6)
     +   /        ' ',          'S',          ' '  /
      DATA MODEL2(1, 1), MODEL2(2, 1), MODEL2(3, 1), MODEL2(4, 1)
     +    /       ' ',          'G',          ' ',          ' '  /
      DATA MODEL2(1, 2), MODEL2(2, 2), MODEL2(3, 2), MODEL2(4, 2)
     +   /        ' ',          'S',          ' ',          ' '  /
      DATA MODEL2(1, 3), MODEL2(2, 3), MODEL2(3, 3), MODEL2(4, 3)
     +   /        'G',          '-',          'S',          ' '  /
      DATA MODEL2(1, 4), MODEL2(2, 4), MODEL2(3, 4), MODEL2(4, 4)
     +   /        'S',          '-',          'G',          ' '  /
      DATA MODEL2(1, 5), MODEL2(2, 5), MODEL2(3, 5), MODEL2(4, 5)
     +   /        '-',          'S',          '-',          'G'  /
      DATA MODEL2(1, 6), MODEL2(2, 6), MODEL2(3, 6), MODEL2(4, 6)
     +   /        '-',          'G',          '-',          'S'  /
      DATA ZERO/0.0D0/
C
C-----------------------------------------------------------------------
C
      PU = IV(PRUNIT)
      IF (PU .EQ. 0) GO TO 999
      IV1 = IV(1)
      OL = IV(OUTLEV)
      IF (IV1 .LT. 2 .OR. IV1 .GT. 15) GO TO 140
      IF (OL .EQ. 0) GO TO 20
      IF (IV1 .GE. 12) GO TO 20
      IF (IV1 .GE. 10 .AND. IV(PRNTIT) .EQ. 0) GO TO 20
      IF (IV1 .GT. 2) GO TO 10
         IV(PRNTIT) = IV(PRNTIT) + 1
         IF (IV(PRNTIT) .LT. ABS(OL)) GO TO 999
 10   NF = IV(NFCALL) - ABS(IV(NFCOV))
      IV(PRNTIT) = 0
      RELDF = ZERO
      PRELDF = ZERO
      OLDF = V(F0)
      IF (OLDF .LE. ZERO) GO TO 12
         RELDF = V(FDIF) / OLDF
         PRELDF = V(PREDUC) / OLDF
 12   IF (OL .GT. 0) GO TO 15
C
C        ***  PRINT SHORT SUMMARY LINE  ***
C
         IF (IV(NEEDHD) .EQ. 1) WRITE(PU, 1010)
 1010 FORMAT(12H0   IT    NF,6X,'F',8X,5HRELDF,6X,6HPRELDF,5X,5HRELDX)
         IV(NEEDHD) = 0
         WRITE(PU,1017) IV(NITER), NF, V(F), RELDF, PRELDF, V(RELDX)
         GO TO 20
C
C     ***  PRINT LONG SUMMARY LINE  ***
C
 15   IF (IV(NEEDHD) .EQ. 1) WRITE(PU,1015)
 1015 FORMAT(12H0   IT    NF,6X,'F',8X,5HRELDF,6X,6HPRELDF,5X,5HRELDX,
     +       4X,15HMODEL    STPPAR,6X,4HSIZE,6X,6HD*STEP,5X,7HNPRELDF)
      IV(NEEDHD) = 0
      M = IV(SUSED)
      NRELDF = ZERO
      IF (OLDF .GT. ZERO) NRELDF = V(NREDUC) / OLDF
      WRITE(PU,1017) IV(NITER), NF, V(F), RELDF, PRELDF, V(RELDX),
     +               (MODEL1(ICH, M), ICH = 1, 3),
     +               (MODEL2(ICH, M), ICH = 1, 4),
     +               V(STPPAR), V(SIZE), V(DSTNRM), NRELDF
 1017 FORMAT(1X,I5,I6,4D11.3,7A1,4D11.3)
C
 20   GO TO (999,999,30,35,40,45,50,60,70,80,90,150,110,120,130), IV1
C
 30   WRITE(PU,1030)
 1030 FORMAT(26H0***** X-CONVERGENCE *****)
      GO TO 180
C
 35   WRITE(PU,1035)
 1035 FORMAT(42H0***** RELATIVE FUNCTION CONVERGENCE *****)
      GO TO 180
C
 40   WRITE(PU,1040)
 1040 FORMAT(49H0***** X- AND RELATIVE FUNCTION CONVERGENCE *****)
      GO TO 180
C
 45   WRITE(PU,1045)
 1045 FORMAT(42H0***** ABSOLUTE FUNCTION CONVERGENCE *****)
      GO TO 180
C
 50   WRITE(PU,1050)
 1050 FORMAT(33H0***** SINGULAR CONVERGENCE *****)
      GO TO 180
C
 60   WRITE(PU,1060)
 1060 FORMAT(30H0***** FALSE CONVERGENCE *****)
      GO TO 180
C
 70   WRITE(PU,1070)
 1070 FORMAT(38H0***** FUNCTION EVALUATION LIMIT *****)
      GO TO 180
C
 80   WRITE(PU,1080)
 1080 FORMAT(28H0***** ITERATION LIMIT *****)
      GO TO 180
C
 90   WRITE(PU,1090)
 1090 FORMAT(18H0***** STOPX *****)
      GO TO 180
C
 110  WRITE(PU,1100)
 1100 FORMAT(45H0***** INITIAL SUM OF SQUARES OVERFLOWS *****)
C
      GO TO 150
C
 120  WRITE(PU,1120)
 1120 FORMAT(37H0***** BAD PARAMETERS TO ASSESS *****)
      GO TO 999
C
 130  WRITE(PU,1130)
 1130 FORMAT(36H0***** J COULD NOT BE COMPUTED *****)
      IF (IV(NITER) .GT. 0) GO TO 190
      GO TO 150
C
 140  WRITE(PU,1140) IV1
 1140 FORMAT(14H0***** IV(1) =,I5,6H *****)
      GO TO 999
C
C  ***  INITIAL CALL ON ITSMRY  ***
C
 150  IF (IV(X0PRT) .NE. 0) WRITE(PU,1150) (I, X(I), D(I), I = 1, P)
 1150 FORMAT(23H0    I     INITIAL X(I),7X,4HD(I)//(1X,I5,D17.6,D14.3))
      IF (IV1 .GE. 13) GO TO 999
      IV(NEEDHD) = 0
      IV(PRNTIT) = 0
      IF (OL .EQ. 0) GO TO 999
      IF (OL .LT. 0) WRITE(PU,1010)
      IF (OL .GT. 0) WRITE(PU,1015)
      WRITE(PU,1160) V(F)
 1160 FORMAT(12H0    0     1,D11.3,11X,D11.3)
      GO TO 999
C
C  ***  PRINT VARIOUS INFORMATION REQUESTED ON SOLUTION  ***
C
 180  IV(NEEDHD) = 1
      IF (IV(STATPR) .EQ. 0) GO TO 190
         OLDF = V(F0)
         PRELDF = ZERO
         NRELDF = ZERO
         IF (OLDF .LE. ZERO) GO TO 185
              PRELDF = V(PREDUC) / OLDF
              NRELDF = V(NREDUC) / OLDF
 185     NF = IV(NFCALL) - IV(NFCOV)
         NG = IV(NGCALL) - IV(NGCOV)
         WRITE(PU,1180) V(F), V(RELDX), NF, NG, PRELDF, NRELDF
 1180 FORMAT(9H0FUNCTION,D17.6,8H   RELDX,D20.6/12H FUNC. EVALS,
     +   I8,9X,'GRAD. EVALS',I8/' PRELDF',D19.6,3X,'NPRELDF',D18.6)
C
         IF (IV(NFCOV) .GT. 0) WRITE(PU,1185) IV(NFCOV)
 1185    FORMAT('0',I4,' EXTRA FUNC. EVALS FOR COVARIANCE.')
         IF (IV(NGCOV) .GT. 0) WRITE(PU,1186) IV(NGCOV)
 1186    FORMAT(1X,I4,' EXTRA GRAD. EVALS FOR COVARIANCE.')
C
 190  IF (IV(SOLPRT) .EQ. 0) GO TO 210
         IV(NEEDHD) = 1
         G1 = IV(G)
         WRITE(PU,1190)
 1190 FORMAT('0    I      FINAL X(I)',8X,'D(I)',10X,'G(I)'/)
         DO 200 I = 1, P
              WRITE(PU,1200) I, X(I), D(I), V(G1)
              G1 = G1 + 1
 200          CONTINUE
 1200    FORMAT(1X,I5,D17.6,2D14.3)
C
 210  IF (IV(COVPRT) .EQ. 0) GO TO 999
      COV1 = IV(COVMAT)
      IV(NEEDHD) = 1
      IF (COV1) 220, 230, 240
 220  IF (-1 .EQ. COV1) WRITE(PU,1220)
 1220 FORMAT(43H0++++++ INDEFINITE COVARIANCE MATRIX ++++++)
      IF (-2 .EQ. COV1) WRITE(PU,1225)
 1225 FORMAT(52H0++++++ OVERSIZE STEPS IN COMPUTING COVARIANCE +++++)
      GO TO 999
C
 230  WRITE(PU,1230)
 1230 FORMAT(45H0++++++ COVARIANCE MATRIX NOT COMPUTED ++++++)
      GO TO 999
C
 240  I = ABS(IV(COVREQ))
      IF (I .LE. 1) WRITE(PU,1241)
 1241 FORMAT(48H0COVARIANCE = SCALE * H**-1 * (J**T * J) * H**-1/)
      IF (I .EQ. 2) WRITE(PU,1242)
 1242 FORMAT(27H0COVARIANCE = SCALE * H**-1/)
      IF (I .GE. 3) WRITE(PU,1243)
 1243 FORMAT(36H0COVARIANCE = SCALE * (J**T * J)**-1/)
      II = COV1 - 1
      IF (OL .LE. 0) GO TO 260
      DO 250 I = 1, P
         I1 = II + 1
         II = II + I
         WRITE(PU,1250) I, (V(J), J = I1, II)
 250     CONTINUE
 1250 FORMAT(4H ROW,I3,2X,9D12.4/(9X,9D12.4))
      GO TO 999
C
 260  DO 270 I = 1, P
         I1 = II + 1
         II = II + I
         WRITE(PU,1270) I, (V(J), J = I1, II)
 270     CONTINUE
 1270 FORMAT(4H ROW,I3,2X,5D12.4/(9X,5D12.4))
C
 999  RETURN
C  ***  LAST CARD OF ITSMRY FOLLOWS  ***
      END
