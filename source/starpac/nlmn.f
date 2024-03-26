*NLMN
      SUBROUTINE NLMN(Y, WEIGHT, NNZW, WT, LWT, XM, N, M, IXM, MDL,
     +   NLDRV, APRXDV, DRV, IFIXD, PAR, PARE, NPAR, RES, PAGE, WIDE,
     +   HLFRPT, STP, LSTP, MIT, STOPSS, STOPP, SCALE, LSCALE, DELTA,
     +   IVAPRX, IPTOUT, NDIGIT, RSD, PV, SDPVI, SDRESI, VCVL, LVCVL, D,
     +   IWORK, IIWORK, RWORK, IRWORK, NLHDR, NPARE)
C
C     LATEST REVISION  -  03/15/90  (JRD)
C
C     THIS IS THE CONTROLING SUBROUTINE FOR PERFORMING NONLINEAR
C     LEAST SQUARES REGRESSION USING THE NL2 SOFTWARE PACKAGE
C     (IMPLEMENTING THE METHOD OF DENNIS, GAY AND WELSCH).
C     THIS SUBROUTINE WAS ADAPTED FROM SUBROUTINE NL2SOL.
C
C     REFERENCES
C
C     DENNIS, J.E., GAY, D.M., AND WELSCH, R.E. (1979), AN ADAPTIVE
C             NONLINEAR LEAST-SQUARES ALGORITHM, (BEING REVISED).
C
C     WRITTEN BY  -  JANET R. DONALDSON
C                    STATISTICAL ENGINEERING DIVISION
C                    NATIONAL BUREAU OF STANDARDS, BOULDER, COLORADO
C
C     CREATION DATE  -  APRIL 2, 1981
C
C
C  VARIABLE DECLARATIONS
C
C  SCALAR ARGUMENTS
      DOUBLE PRECISION
     +   DELTA,RSD,STOPP,STOPSS
      INTEGER
     +   IIWORK,IRWORK,IVAPRX,IXM,LSCALE,LSTP,LVCVL,LWT,M,MIT,N,
     +   NDIGIT,NNZW,NPAR,NPARE,SDPVI,SDRESI,VCVL
      LOGICAL
     +   APRXDV,HLFRPT,PAGE,WEIGHT,WIDE
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   D(N,NPAR),PAR(NPAR),PARE(NPAR),PV(N),RES(N),RWORK(IRWORK),
     +   SCALE(LSCALE),STP(LSTP),WT(LWT),XM(IXM,M),Y(N)
      INTEGER
     +   IFIXD(NPAR),IPTOUT(NDIGIT),IWORK(IIWORK)
C
C  SUBROUTINE ARGUMENTS
      EXTERNAL DRV,MDL,NLDRV,NLHDR
C
C  SCALARS IN COMMON
      INTEGER
     +   IERR
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   WTSQRT
      INTEGER
     +   CNVCOD,COVMAT,I,ICNVCD,IVCVPT,QTR,RD,RDI,RSAVE,RSSHLF,S,
     +   SCL
      LOGICAL
     +   CMPDRV,DONE,HEAD,NEWITR,PRTSMY
C
C  LOCAL ARRAYS
      INTEGER
     +   ISKULL(10)
C
C  EXTERNAL SUBROUTINES
      EXTERNAL NL2ITR,NLERR,NLFIN,NLINIT,NLISM,NLITRP,NLSUPK
C
C  INTRINSIC FUNCTIONS
      INTRINSIC SQRT
C
C  COMMON BLOCKS
      COMMON /ERRCHK/IERR
C
C     VARIABLE DEFINITIONS (ALPHABETICALLY)
C
C     LOGICAL APRXDV
C        THE VARIABLE USED TO INDICATE WHETHER NUMERICAL
C        APPROXIMATIONS TO THE DERIVATIVE WERE USED (TRUE) OR NOT
C        (FALSE).
C     LOGICAL CMPDRV
C        THE VARIABLE USED TO INDICATE WHETHER DERIVATIVES MUST BE
C        COMPUTED (TRUE) OR NOT (FALSE).
C     INTEGER CNVCOD
C        A VALUE USED TO CONTROL THE PRINTING OF ITERATION REPORTS.
C     INTEGER COVMAT
C        THE LOCATION IN IWORK OF THE STARTING LOCATION IN RWORK
C        OF THE BEGINNING OF THE VCV MATRIX.
C     DOUBLE PRECISION D(N,NPAR)
C        THE FORWARD DIFFERENCE QUOTIENT APPROXIMATION TO THE
C        DERIVATIVE OF THE MODEL WITH RESPECT TO THE JTH PARAMETER.
C     DOUBLE PRECISION DELTA
C        THE MAXIMUM CHANGE ALLOWED IN THE MODEL PARAMETERS AT THE
C        FIRST ITERATION.
C     EXTERNAL DRV
C        THE NAME OF THE USER SUPPLIED SUBROUTINE WHICH COMPUTES THE
C        DERIVATIVE (JACOBIAN) MATRIX OF THE MODEL.
C     LOGICAL DONE
C        THE VARIABLE USED TO INDICATE WHETHER THIS IS THE FINAL
C        COMPUTATION OF THE JACOBIAN OR NOT.
C     LOGICAL HEAD
C        THE VARIABLE USED TO INDICATE WHETHER A HEADING IS TO BE
C        PRINTED DURING A GIVEN CALL TO THE ITERATION REPORT (TRUE)
C        OR NOT (FALSE).
C     LOGICAL HLFRPT
C        THE VARIABLE WHICH INDICATES WHETHER THE DERIVATIVE
C        CHECKING ROUTINE HAS ALREADY PRINTED PART OF THE
C        INITIAL SUMMARY (TRUE) OR NOT (FALSE).
C     INTEGER I
C        AN INDEXING VARIABLE.
C     INTEGER ICNVCD
C        THE LOCATION IN IWORK OF
C        THE CONVERGENCE CONDITION.
C     INTEGER IERR
C        THE INTEGER VALUE RETURNED BY THIS ROUTINE DESIGNATING
C        WHETHER ANY ERRORS WERE DETECTED IN THE PARAMETER LIST.
C        IF IERR .EQ. 0, NO ERRORS WERE DETECTED.
C        IF IERR .GE. 1, ERRORS WERE DETECTED.
C     INTEGER IFIXD(NPAR)
C        THE INDICATOR VALUES USED TO DESIGNATE WHETHER THE
C        PARAMETERS ARE TO BE OPTIMIZED OR ARE TO BE HELD FIXED.
C        IF IFIXED(I).NE.0, THEN PAR(I) WILL BE HELD FIXED.
C        IF IFIXED(I).EQ.0, THEN PAR(I) WILL BE OPTIMIZED.
C     INTEGER IIWORK
C        THE DIMENSION OF THE INTEGER WORK VECTOR IWORK.
C     INTEGER IPTOUT(NDIGIT)
C        THE VARIABLE USED TO CONTROL PRINTED OUTPUT FOR EACH SECTION.
C     INTEGER IRWORK
C        THE DIMENSION OF THE DOUBLE PRECISION WORK VECTOR RWORK.
C     INTEGER ISKULL(10)
C        AN ERROR MESSAGE INDICATOR VARIABLE.
C     INTEGER IVAPRX
C        AN INDICATOR VALUE USED TO DESIGNATE WHICH OPTION IS TO BE USED
C        TO COMPUTE THE VARIANCE COVARIANCE MATRIX (VCV), WHERE
C        IVAPRX LE 0 INDICATES THE THE DEFAULT OPTION WILL BE USED
C        IVAPRX EQ 1 INDICATES THE VCV IS TO BE COMPUTED BY
C                       INVERSE(TRANSPOSE(JACOBIAN)*JACOBIAN)
C                    USING BOTH THE MODEL SUBROUTINE THE USER SUPPLIED
C                    DERIVATIVE SUBROUTINE WHEN IT IS AVAILABLE
C        IVAPRX EQ 2 INDICATES THE VCV IS TO BE COMPUTED BY
C                       INVERSE(HESSIAN)
C                    USING BOTH THE MODEL SUBROUTINE THE USER SUPPLIED
C                    DERIVATIVE SUBROUTINE WHEN IT IS AVAILABLE
C        IVAPRX EQ 3 INDICATES THE VCV IS TO BE COMPUTED BY
C                       INVERSE(HESSIAN)*TRANSPOSE(JACOBIAN)*JACOBIAN
C                          *INVERSE(HESSIAN)
C                    USING BOTH THE MODEL SUBROUTINE THE USER SUPPLIED
C                    DERIVATIVE SUBROUTINE WHEN IT IS AVAILABLE
C        IVAPRX EQ 4 INDICATES THE VCV IS TO BE COMPUTED BY
C                       INVERSE(TRANSPOSE(JACOBIAN)*JACOBIAN)
C                    USING ONLY THE MODEL SUBROUTINE
C        IVAPRX EQ 5 INDICATES THE VCV IS TO BE COMPUTED BY
C                       INVERSE(HESSIAN)
C                    USING ONLY THE MODEL SUBROUTINE
C        IVAPRX EQ 6 INDICATES THE VCV IS TO BE COMPUTED BY
C                       INVERSE(HESSIAN)*TRANSPOSE(JACOBIAN)*JACOBIAN
C                          *INVERSE(HESSIAN)
C                    USING ONLY THE MODEL SUBROUTINE
C        IVAPRX GE 7 INDICATES THE DEFAULT OPTION WILL BE USED
C     INTEGER IVCVPT
C        AN INDICATOR VALUE USED TO DESIGNATE WHICH FORM OF THE
C        VARIANCE COVARIANCE MATRIX (VCV) IS BEING PRINTED, WHERE
C        IVCVPT = 1 INDICATES THE VCV WAS COMPUTED AS
C                   INVERSE(TRANSPOSE(JACOBIAN)*JACOBIAN)
C        IVCVPT = 2 INDICATES THE VCV WAS COMPUTED AS
C                   INVERSE(HESSIAN)
C        IVCVPT = 3 INDICATES THE VCV WAS COMPUTED AS
C                   INVERSE(HESSIAN)*TRANSPOSE(JACOBIAN)*JACOBIAN
C                       *INVERSE(HESSIAN)
C     INTEGER IWORK(IIWORK)
C        THE INTEGER WORK SPACE VECTOR USED BY THE NL2 SUBROUTINES.
C     INTEGER IXM
C        THE FIRST DIMENSION OF THE INDEPENDENT VARIABLE ARRAY.
C     INTEGER LSCALE
C        THE ACTUAL LENGTH OF THE VECTOR SCALE.
C     INTEGER LSTP
C        THE ACTUAL LENGTH OF THE VECTOR STP.
C     INTEGER LVCVL
C        THE LENGTH OF THE VECTOR CONTAINING
C        THE LOWER HALF OF THE VCV MATRIX, STORED ROW WISE.
C     INTEGER LWT
C        THE ACTUAL LENGTH OF THE VECTOR WT.
C     INTEGER M
C        THE NUMBER OF INDEPENDENT VARIABLES.
C     INTEGER MIT
C        THE MAXIMUM NUMBER OF ITERATIONS ALLOWED.
C     EXTERNAL MDL
C        THE NAME OF THE USER SUPPLIED SUBROUTINE WHICH COMPUTES THE
C        PREDICTED VALUES BASED ON THE CURRENT PARAMETER ESTIMATES.
C     INTEGER N
C        THE NUMBER OF OBSERVATIONS.
C     INTEGER NDIGIT
C        THE NUMBER OF DIGITS IN THE PRINT CONTROL VALUE.
C     LOGICAL NEWITR
C        A FLAG USED TO INDICATE WHETHER A NEW ITERATION HAS BEEN
C        COMPLETED (TRUE) OR NOT (FALSE).
C     EXTERNAL NLDRV
C        THE NAME OF THE ROUTINE WHICH CALCULATED THE DERIVATIVES
C     EXTERNAL NLHDR
C        THE NAME OF THE ROUTINE WHICH PRODUCES THE HEADING.
C     INTEGER NNZW
C        THE NUMBER OF NON ZERO WEIGHTS.
C     INTEGER NPAR
C        THE NUMBER OF PARAMETERS IN THE MODEL.
C     INTEGER NPARE
C        THE NUMBER OF PARAMETERS TO BE OPTIMIZED.
C     LOGICAL PAGE
C        THE VARIABLE USED TO INDICATE WHETHER A GIVEN SECTION OF
C        THE OUTPUT IS TO BEGIN ON A NEW PAGE (TRUE) OR NOT (FALSE).
C     DOUBLE PRECISION PAR(NPAR)
C        THE CURRENT ESTIMATES OF THE PARAMETERS.
C     DOUBLE PRECISION PARE(NPAR)
C        THE CURRENT ESTIMATES OF THE PARAMETERS, BUT ONLY
C        THOSE TO BE OPTIMIZED (NOT THOSE WHOSE VALUES ARE FIXED).
C     LOGICAL PRTSMY
C        THE VARIABLE USED TO INDICATE WHETHER THE SUMMARY
C        INFORMATION IS TO BE PRINTED (TRUE) OR NOT (FALSE).
C     DOUBLE PRECISION PV(N)
C        THE PREDICTED VALUES.
C     INTEGER QTR
C        THE LOCATION IN IWORK OF THE STARTING LOCATION IN RWORK
C        THE ARRAY Q TRANSPOSE R.
C     INTEGER RD
C        THE LOCATION IN IWORK OF THE STARTING LOCATION IN RWORK OF
C        THE DIAGONAL ELEMENTS OF THE R MATRIX OF THE Q - R
C        FACTORIZATION OF D.
C     INTEGER RDI
C        THE LOCATION IN RWORK OF THE DIAGONAL ELEMENTS OF THE R
C        MATRIX OF THE Q - R FACTORIZATION OF D.
C     DOUBLE PRECISION RES(N)
C        THE RESIDUALS FROM THE FIT.
C     INTEGER RSAVE
C        THE LOCATION IN IWORK OF THE STARTING LOCATION IN RWORK
C        THE ARRAY RSAVE.
C     DOUBLE PRECISION RSD
C        THE VALUE OF THE RESIDUAL STANDARD DEVIATION AT THE SOLUTION.
C     INTEGER RSSHLF
C        THE LOCATION IN RWORK OF
C        HALF THE RESIDUAL SUM OF SQUARES.
C     DOUBLE PRECISION RWORK(IRWORK)
C        THE DOUBLE PRECISION WORK VECTOR USED BY THE NL2 SUBROUTINES.
C     INTEGER S
C        THE LOCATION IN IWORK OF THE STARTING LOCATION IN RWORK
C        THE ARRAY OF SECOND ORDER TERMS OF THE HESSIAN.
C     DOUBLE PRECISION SCALE(LSCALE)
C        THE TYPICAL SIZE OF THE PARAMETERS.
C     INTEGER SCL
C        THE INDEX IN RWORK OF THE 1ST VALUE OF THE USER SUPPLIED SCALE
C        VALUE.
C     INTEGER SDPVI
C        THE STARTING LOCATION IN RWORK OF
C        THE STANDARD DEVIATIONS OF THE PREDICTED VALUES.
C     INTEGER SDRESI
C        THE STARTING LOCATION IN RWORK OF THE
C        THE STANDARDIZED RESIDUALS.
C     DOUBLE PRECISION STOPP
C        THE STOPPING CRITERION FOR THE TEST BASED ON THE MAXIMUM SCALED
C        RELATIVE CHANGE IN THE ELEMENTS OF THE MODEL PARAMETER VECTOR
C        PREDICTED DECREASE IN THE RESIDUAL STANDARD DEVIATION (COMPUTED
C        BY STARPAC) TO THE CURRENT RESIDUAL SUM OF SQUARES ESTIMATE.
C     DOUBLE PRECISION STOPSS
C        THE STOPPING CRITERION FORTHE TEST BASED ON THE RATIO OF THE
C        PREDICTED DECREASE IN THE RESIDUAL SUM OF SQUARES (COMPUTED
C        BY STARPAC) TO THE CURRENT RESIDUAL SUM OF SQUARES ESTIMATE.
C     DOUBLE PRECISION STP(LSTP)
C        THE DUMMY STEP SIZE ARRAY.
C     INTEGER VCVL
C        THE STARTING LOCATION IN RWORK OF THE LOWER HALF OF THE
C        VCV MATRIX, STORED ROW WISE.
C     LOGICAL WEIGHT
C        THE VARIABLE USED TO INDICATE WHETHER WEIGHTED ANALYSIS IS TO
C        BE PERFORMED (TRUE) OR NOT (FALSE).
C     LOGICAL WIDE
C        THE VARIABLE USED TO INDICATE WHETHER THE HEADING SHOULD
C        BE FULL WIDTH (TRUE) OR NOT (FALSE).
C     DOUBLE PRECISION WT(LWT)
C        THE USER SUPPLIED WEIGHTS.
C     DOUBLE PRECISION WTSQRT
C        THE SQUARE ROOT OF THE USER SUPPLIED WEIGHTS.
C     DOUBLE PRECISION XM(IXM,M)
C        THE ARRAY IN WHICH ONE ROW OF THE INDEPENDENT VARIABLE ARRAY
C        IS STORED.
C     DOUBLE PRECISION Y(N)
C        THE ARRAY OF THE DEPENDENT VARIABLE.
C
C     IWORK SUBSCRIPT VALUES
C
      DATA CNVCOD /34/, ICNVCD /1/, COVMAT /26/, QTR /49/, RD /51/,
     +   RSAVE /52/, S/53/
      DATA RSSHLF /10/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
C     INITIALIZE CONTROL PARAMETERS
C
      CALL NLINIT (N, IFIXD, PAR, NPAR, PARE, NPARE, MIT, STOPSS,
     +   STOPP, SCALE, LSCALE, DELTA, IVAPRX, APRXDV, IVCVPT, IWORK,
     +   IIWORK, RWORK, IRWORK, SCL)
C
      CMPDRV = .TRUE.
      DONE = .FALSE.
      HEAD = .TRUE.
      NEWITR = .FALSE.
      PRTSMY = (IPTOUT(1).NE.0)
C
C
C     COMPUTE RESIDUALS
C
   10 CALL MDL(PAR, NPAR, XM, N, M, IXM, PV, WT)
C
      DO 20 I=1,N
         WTSQRT = 1.0D0
         IF (WEIGHT) WTSQRT = SQRT(WT(I))
         RES(I) = WTSQRT*(Y(I)-PV(I))
   20 CONTINUE
C
C     PRINT INITIAL SUMMARY
C
      IF (.NOT.PRTSMY) GO TO 30
      CALL NLISM(NLHDR, PAGE, WIDE, HLFRPT, NPAR, M, N, NNZW, WEIGHT,
     +   IFIXD, PAR, SCALE, IWORK, IIWORK, RWORK, IRWORK, RES, APRXDV,
     +   STP, LSTP, NPARE)
      PRTSMY = .FALSE.
C
   30 CONTINUE
C
      IF (.NOT.CMPDRV) GO TO 50
C
      CMPDRV = .FALSE.
C
   40 CONTINUE
C
C     PRINT ITERATION REPORT IF DESIRED
C
      IF ((IPTOUT(2).NE.0) .AND. NEWITR) CALL NLITRP(NLHDR, HEAD, PAGE,
     +   WIDE, IPTOUT(2), NPAR, NNZW, IWORK, IIWORK, RWORK, IRWORK,
     +   IFIXD, PARE, NPARE)
C
C  ***  COMPUTE JACOBIAN  ***
C
      IF (DONE) CALL MDL(PAR, NPAR, XM, N, M, IXM, PV, WT)
C
      CALL NLDRV (MDL, DRV, DONE, IFIXD, PAR, NPAR, XM, N, M, IXM,
     +   PV, D, WEIGHT, WT, LWT, STP, LSTP, RWORK(SCL), NPARE)
C
      IF (DONE) GO TO 70
C
C     COMPUTE NEXT ITERATION
C
   50 CALL NL2ITR(RWORK(SCL), IWORK, D, N, N, NPARE, RES, RWORK, PARE)
C
C     UNPACK PARAMETERS
C
      CALL NLSUPK(PARE, NPARE, PAR, IFIXD, NPAR)
C
      NEWITR = (IWORK(CNVCOD).EQ.0)
      IF (IWORK(1)-2) 10, 40, 60
C
   60 DONE = .TRUE.
      GO TO 40
   70 CONTINUE
C
C     SET ERROR FLAGS, IF NECESSARY
C
      CALL NLERR(IWORK(ICNVCD), ISKULL)
C
C     FINISH COMPUTATIONS AND PRINT ANY DESIRED RESULTS
C
C     EQUIVALENCE LOCATIONS WITHIN RWORK.
C
      SDPVI = IWORK(RSAVE)
      SDRESI = IWORK(QTR)
      VCVL = IWORK(COVMAT)
      IF (VCVL.GE.1) GO TO 80
C
      VCVL = IWORK(S)
      IF (IERR.NE.0) GO TO 80
      ISKULL(1) = 1
      ISKULL(7) = 1
      IERR = 7
C
   80 CONTINUE
C
      LVCVL = NPARE*(NPARE+1)/2
C
      RDI = IWORK(RD)
C
      CALL NLFIN(Y, WEIGHT, NNZW, WT, LWT, XM, N, M, IXM, IFIXD, PAR,
     +   NPAR, NPARE, RES, PAGE, WIDE, IPTOUT, NDIGIT, RWORK(RSSHLF),
     +   RSD, PV, RWORK(SDPVI), RWORK(SDRESI), RWORK(RDI), RWORK(VCVL),
     +   LVCVL, D, NLHDR, IVCVPT, ISKULL)
C
      RETURN
C
      END