*QRFACT
      SUBROUTINE QRFACT(NM,M,N,QR,ALPHA,IPIVOT,IERR,NOPIVK,SUM)
C
C     LATEST REVISION  -  03/15/90  (JRD)
C
C  ***  COMPUTE THE QR DECOMPOSITION OF THE MATRIX STORED IN QR  ***
C
C
C  VARIABLE DECLARATIONS
C
C  SCALAR ARGUMENTS
      INTEGER
     +   IERR,M,N,NM,NOPIVK
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   ALPHA(N),QR(NM,N),SUM(N)
      INTEGER
     +   IPIVOT(N)
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   ALPHAK,BETA,ONE,P01,P99,QRKK,QRKMAX,RKTOL,RKTOL1,SIGMA,SUMJ,
     +   TEMP,UFETA,ZERO
      INTEGER
     +   I,J,JBAR,K,K1,MINUM,MK1
C
C  EXTERNAL FUNCTIONS
      DOUBLE PRECISION
     +   DOTPRD,RMDCON,V2NORM
      EXTERNAL DOTPRD,RMDCON,V2NORM
C
C  EXTERNAL SUBROUTINES
      EXTERNAL VAXPY,VSCOPY
C
C  INTRINSIC FUNCTIONS
      INTRINSIC ABS,MIN,SQRT
C
C     *****PARAMETERS.
C     INTEGER NM,M,N,IPIVOT(N),IERR,NOPIVK
C     DOUBLE PRECISION  QR(NM,N),ALPHA(N),SUM(N)
C     *****LOCAL VARIABLES.
C     INTEGER I,J,JBAR,K,K1,MINUM,MK1
C     DOUBLE PRECISION  ALPHAK,BETA,QRKK,QRKMAX,SIGMA,TEMP,UFETA,RKTOL,
C    1        RKTOL1,SUMJ
C     *****FUNCTIONS.
C/+
C     INTEGER MIN
C     DOUBLE PRECISION              ABS,SQRT
C/
C     EXTERNAL DOTPRD, RMDCON, VAXPY, VSCOPY, V2NORM
C     DOUBLE PRECISION DOTPRD, RMDCON, V2NORM
C DOTPRD... RETURNS INNER PRODUCT OF TWO VECTORS.
C RMDCON... RETURNS MACHINE-DEPENDENT CONSTANTS.
C VAXPY... COMPUTES SCALAR TIMES ONE VECTOR PLUS ANOTHER.
C VSCOPY... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.
C V2NORM... RETURNS THE 2-NORM OF A VECTOR.
C
C     *****CONSTANTS.
C     DOUBLE PRECISION ONE, P01, P99, ZERO
      DATA ONE/1.0D0/, P01/0.01D0/, P99/0.99D0/, ZERO/0.0D0/
C
C
C     ==================================================================
C
C
C     *****PURPOSE.
C
C     THIS SUBROUTINE DOES A QR-DECOMPOSITION ON THE M X N MATRIX QR,
C        WITH AN OPTIONALLY MODIFIED COLUMN PIVOTING, AND RETURNS THE
C        UPPER TRIANGULAR R-MATRIX, AS WELL AS THE ORTHOGONAL VECTORS
C        USED IN THE TRANSFORMATIONS.
C
C     *****PARAMETER DESCRIPTION.
C     ON INPUT.
C
C        NM MUST BE SET TO THE ROW DIMENSION OF THE TWO DIMENSIONAL
C             ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C             DIMENSION STATEMENT.
C
C        M MUST BE SET TO THE NUMBER OF ROWS IN THE MATRIX.
C
C        N MUST BE SET TO THE NUMBER OF COLUMNS IN THE MATRIX.
C
C        QR CONTAINS THE REAL RECTANGULAR MATRIX TO BE DECOMPOSED.
C
C     NOPIVK IS USED TO CONTROL PIVOTTING.  COLUMNS 1 THROUGH
C        NOPIVK WILL REMAIN FIXED IN POSITION.
C
C        SUM IS USED FOR TEMPORARY STORAGE FOR THE SUBROUTINE.
C
C     ON OUTPUT.
C
C        QR CONTAINS THE NON-DIAGONAL ELEMENTS OF THE R-MATRIX
C             IN THE STRICT UPPER TRIANGLE. THE VECTORS U, WHICH
C             DEFINE THE HOUSEHOLDER TRANSFORMATIONS   I - U*U-TRANSP,
C             ARE IN THE COLUMNS OF THE LOWER TRIANGLE. THESE VECTORS U
C             ARE SCALED SO THAT THE SQUARE OF THEIR 2-NORM IS 2.0.
C
C        ALPHA CONTAINS THE DIAGONAL ELEMENTS OF THE R-MATRIX.
C
C        IPIVOT REFLECTS THE COLUMN PIVOTING PERFORMED ON THE INPUT
C             MATRIX TO ACCOMPLISH THE DECOMPOSITION. THE J-TH
C             ELEMENT OF IPIVOT GIVES THE COLUMN OF THE ORIGINAL
C             MATRIX WHICH WAS PIVOTED INTO COLUMN J DURING THE
C             DECOMPOSITION.
C
C        IERR IS SET TO.
C             0 FOR NORMAL RETURN,
C             K IF NO NON-ZERO PIVOT COULD BE FOUND FOR THE K-TH
C                  TRANSFORMATION, OR
C             -K FOR AN ERROR EXIT ON THE K-TH THANSFORMATION.
C             IF AN ERROR EXIT WAS TAKEN, THE FIRST (K - 1)
C             TRANSFORMATIONS ARE CORRECT.
C
C
C     *****APPLICATIONS AND USAGE RESTRICTIONS.
C     THIS MAY BE USED WHEN SOLVING LINEAR LEAST-SQUARES PROBLEMS --
C     SEE SUBROUTINE QR1 OF ROSEPACK.  IT IS CALLED FOR THIS PURPOSE
C     BY LLSQST IN THE NL2SOL (NONLINEAR LEAST-SQUARES) PACKAGE.
C
C     *****ALGORITHM NOTES.
C     THIS VERSION OF QRFACT TRIES TO ELIMINATE THE OCCURRENCE OF
C     UNDERFLOWS DURING THE ACCUMULATION OF INNER PRODUCTS.  RKTOL1
C     IS CHOSEN BELOW SO AS TO INSURE THAT DISCARDED TERMS HAVE NO
C     EFFECT ON THE COMPUTED TWO-NORMS.
C
C     ADAPTED FROM THE ALGOL ROUTINE SOLVE (1).
C
C     *****REFERENCES.
C     (1)     BUSINGER,P. AND GOLUB,G.H., LINEAR LEAST SQUARES
C     SOLUTIONS BY HOUSHOLDER TRANSFORMATIONS, IN WILKINSON,J.H.
C     AND REINSCH,C.(EDS.), HANDBOOK FOR AUTOMATIC COMPUTATION,
C     VOLUME II. LINEAR ALGEBRA, SPRINGER-VERLAG, 111-118 (1971).
C     PREPUBLISHED IN NUMER.MATH. 7, 269-276 (1965).
C
C     *****HISTORY.
C     THIS AMOUNTS TO THE SUBROUTINE QR1 OF ROSEPACK WITH RKTOL1 USED
C     IN PLACE OF RKTOL BELOW, WITH V2NORM USED TO INITIALIZE (AND
C     SOMETIMES UPDATE) THE SUM ARRAY, AND WITH CALLS ON DOTPRD AND
C     VAXPY IN PLACE OF SOME LOOPS.
C
C     *****GENERAL.
C
C     DEVELOPMENT OF THIS PROGRAM SUPPORTED IN PART BY
C     NATIONAL SCIENCE FOUNDATION GRANT GJ-1154X3 AND
C     NATIONAL SCIENCE FOUNDATION GRANT DCR75-08802
C     TO NATIONAL BUREAU OF ECONOMIC RESEARCH, INC.
C
C
C
C     =================================================================
C     =================================================================
C
C
C     ==========  UFETA IS THE SMALLEST POSITIVE FLOATING POINT NUMBER
C        S.T. UFETA AND -UFETA CAN BOTH BE REPRESENTED.
C
C     ==========  RKTOL IS THE SQUARE ROOT OF THE RELATIVE PRECISION
C        OF FLOATING POINT ARITHMETIC (MACHEP).
      DATA RKTOL/0.0D0/, UFETA/0.0D0/
C     *****BODY OF PROGRAM.
      IF (UFETA .GT. ZERO) GO TO 10
         UFETA = RMDCON(1)
         RKTOL = RMDCON(4)
   10 IERR = 0
      RKTOL1 = P01 * RKTOL
C
      DO 20 J=1,N
         SUM(J) = V2NORM(M, QR(1,J))
         IPIVOT(J) = J
   20 CONTINUE
C
      MINUM = MIN(M,N)
C
      DO 120 K=1,MINUM
         MK1 = M - K + 1
C        ==========K-TH HOUSEHOLDER TRANSFORMATION==========
         SIGMA = ZERO
         JBAR = 0
C        ==========FIND LARGEST COLUMN SUM==========
      IF (K .LE. NOPIVK) GO TO 50
         DO 30 J=K,N
              IF (SIGMA .GE. SUM(J))  GO TO 30
              SIGMA = SUM(J)
              JBAR = J
   30    CONTINUE
C
         IF (JBAR .EQ. 0)  GO TO 220
         IF (JBAR .EQ. K)  GO TO 50
C        ==========COLUMN INTERCHANGE==========
         I = IPIVOT(K)
         IPIVOT(K) = IPIVOT(JBAR)
         IPIVOT(JBAR) = I
         SUM(JBAR) = SUM(K)
         SUM(K) = SIGMA
C
         DO 40 I=1,M
              SIGMA = QR(I,K)
              QR(I,K) = QR(I,JBAR)
              QR(I,JBAR) = SIGMA
   40    CONTINUE
C        ==========END OF COLUMN INTERCHANGE==========
   50    CONTINUE
C        ==========  SECOND INNER PRODUCT  ==========
         QRKMAX = ZERO
C
         DO 60 I=K,M
              IF (ABS( QR(I,K) ) .GT. QRKMAX)  QRKMAX = ABS( QR(I,K) )
   60    CONTINUE
C
         IF (QRKMAX .LT. UFETA)  GO TO 210
         ALPHAK = V2NORM(MK1, QR(K,K)) / QRKMAX
         SIGMA = ALPHAK**2
C
C        ==========  END SECOND INNER PRODUCT  ==========
         QRKK = QR(K,K)
         IF (QRKK .GE. ZERO)  ALPHAK = -ALPHAK
         ALPHA(K) = ALPHAK * QRKMAX
         BETA = QRKMAX * SQRT(SIGMA - (QRKK*ALPHAK/QRKMAX) )
         QR(K,K) = QRKK - ALPHA(K)
         DO 65 I=K,M
   65         QR(I,K) =  QR(I,K) / BETA
         K1 = K + 1
         IF (K1 .GT. N) GO TO 120
C
         DO 110 J = K1, N
              TEMP = -DOTPRD(MK1, QR(K,K), QR(K,J))
C
C             ***  SET QR(I,J) = QR(I,J) + TEMP*QR(I,K), I = K,...,M.
C
              CALL VAXPY(MK1, QR(K,J), TEMP, QR(K,K), QR(K,J))
C
              IF (K1 .GT. M) GO TO 110
              SUMJ = SUM(J)
              IF (SUMJ .LT. UFETA) GO TO 110
              TEMP = ABS(QR(K,J)/SUMJ)
              IF (TEMP .LT. RKTOL1) GO TO 110
              IF (TEMP .GE. P99) GO TO 90
                   SUM(J) = SUMJ * SQRT(ONE - TEMP**2)
                   GO TO 110
   90         SUM(J) = V2NORM(M-K, QR(K1,J))
  110    CONTINUE
C        ==========END OF K-TH HOUSEHOLDER TRANSFORMATION==========
  120 CONTINUE
C
      GO TO 999
C     ==========ERROR EXIT ON K-TH TRANSFORMATION==========
  210 IERR = -K
      GO TO 230
C     ==========NO NON-ZERO ACCEPTABLE PIVOT FOUND==========
  220 IERR = K
  230 DO 240 I = K, N
         ALPHA(I) = ZERO
         IF (I .GT. K) CALL VSCOPY(I-K, QR(K,I), ZERO)
 240     CONTINUE
C     ==========RETURN TO CALLER==========
  999 RETURN
C     ==========LAST CARD OF QRFACT==========
      END
