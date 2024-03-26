*STPCNT
      SUBROUTINE STPCNT(XM, N, M, IXM, MDL, PAR, NPAR, STP,
     +   EXMPT, NETA, SCALE, LSCALE, NPRT, HDR, PAGE, WIDE, ISUBHD,
     +   HLFRPT, PRTFXD, IFIXED, LIFIXD, WT)
C
C     LATEST REVISION  -  03/15/90  (JRD)
C
C     THIS ROUTINE CONTROLS THE STEP SIZE SELECTION PROCESS.
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
     +   EXMPT
      INTEGER
     +   ISUBHD,IXM,LIFIXD,LSCALE,M,N,NETA,NPAR,NPRT
      LOGICAL
     +   HLFRPT,PAGE,PRTFXD,WIDE
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   PAR(NPAR),SCALE(LSCALE),STP(NPAR),XM(IXM,M),WT(*)
      INTEGER
     +   IFIXED(LIFIXD)
C
C  SUBROUTINE ARGUMENTS
      EXTERNAL HDR,MDL
C
C  SCALARS IN COMMON
      DOUBLE PRECISION
     +   Q
      INTEGER
     +   IERR
C
C  ARRAYS IN COMMON
      DOUBLE PRECISION DSTAK(12)
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   ETA,EXM,FPLRS,SCL,TAU
      INTEGER
     +   CD,FD,FDLAST,FDSAVE,IFAILJ,IFIXD,IFP,ITEMP,J,MXFAIL,NALL0,
     +   NDD,NDGT1,NEXMPT,NFAIL,NFAILJ,PARTMP,PV,PVMCD,PVNEW,PVPCD,
     +   PVSTP,PVTEMP
      LOGICAL
     +   HEAD
C
C  LOCAL ARRAYS
      DOUBLE PRECISION
     +   RSTAK(12)
      INTEGER
     +   ISTAK(12)
C
C  EXTERNAL FUNCTIONS
      DOUBLE PRECISION
     +   D1MACH
      INTEGER
     +   STKGET,STKST
      EXTERNAL D1MACH,STKGET,STKST
C
C  EXTERNAL SUBROUTINES
      EXTERNAL CPYVII,ETAMDL,SETIV,STKCLR,STPMN,STPOUT
C
C  INTRINSIC FUNCTIONS
      INTRINSIC ABS,INT,LOG10,MAX,MIN
C
C  COMMON BLOCKS
      COMMON /CSTAK/DSTAK
      COMMON /ERRCHK/IERR
      COMMON /NOTOPT/Q
C
C  EQUIVALENCES
      EQUIVALENCE (DSTAK(1),ISTAK(1))
      EQUIVALENCE (DSTAK(1),RSTAK(1))
C
C     VARIABLE DEFINITIONS (ALPHABETICALLY)
C
C     INTEGER CD
C        THE STARTING LOCATION IN THE WORK AREA OF
C        THE CENTRAL DIFFERENCE QUOTIENT APPROXIMATION TO THE
C        DERIVATIVE OF THE MODEL WITH RESPECT TO THE JTH PARAMETER.
C     DOUBLE PRECISION DSTAK(12)
C        THE DOUBLE PRECISION VERSION OF THE /CSTAK/ WORK AREA.
C     DOUBLE PRECISION ETA
C        THE RELATIVE NOISE IN THE MODEL
C     DOUBLE PRECISION EXM
C        THE PROPORTION OF OBSERVATIONS ACTUALLY USED FOR WHICH THE
C        COMPUTED NUMERICAL DERIVATIVES WRT A GIVEN PARAMETER ARE
C        EXEMPTED FROM MEETING THE DERIVATIVE ACCEPTANCE CRITERIA.
C     DOUBLE PRECISION EXMPT
C        THE PROPORTION OF OBSERVATIONS FOR WHICH THE COMPUTED
C        NUMERICAL DRVATIVES WRT A GIVEN PARAMETER ARE EXEMPTED
C        FROM MEETING THE DERIVATIVE ACCEPTANCE CRITERIA.
C     INTEGER FD
C        THE STARTING LOCATION IN THE WORK AREA OF
C        THE FORWARD DIFFERENCE QUOTIENT APPROXIMATION TO THE
C        DERIVATIVE OF THE MODEL WITH RESPECT TO THE JTH PARAMETER.
C     INTEGER FDLAST
C        THE STARTING LOCATION IN THE WORK AREA OF
C        THE FORWARD DIFFERENCE QUOTIENT APPROXIMATION TO THE
C        DERIVATIVE OF THE MODEL WITH RESPECT TO THE JTH PARAMETER
C        FOR THE LAST STEP SIZE TRIED.
C     INTEGER FDSAVE
C        THE STARTING LOCATION IN THE WORK AREA OF
C        THE FORWARD DIFFERENCE QUOTIENT APPROXIMATION TO THE
C        DERIVATIVE OF THE MODEL WITH RESPECT TO THE JTH PARAMETER
C        FOR THE BEST STEP SIZE TRIED SO FAR.
C     DOUBLE PRECISION FPLRS
C        THE FLOATING POINT LARGEST RELATIVE SPACING.
C     EXTERNAL HDR
C        THE NAME OF THE ROUTINE WHICH PRODUCES THE HEADING
C     LOGICAL HEAD
C        A FLAG INDICATING WHETHER THE HEADING SHOULD BE PRINTED
C        (TRUE) OR NOT (FALSE).  IF A HEADING IS PRINTED, THE VALUE
C        OF HEAD WILL BE CHANGED TO FALSE.
C     LOGICAL HLFRPT
C        THE VARIABLE WHICH INDICATES WHETHER THE STEP SIZE SELECTION
C        ROUTINE HAS ALREADY PRINTED PART OF THE INITIAL SUMMARY (TRUE)
C        OR NOT (FALSE).
C     INTEGER IERR
C        THE INTEGER VALUE RETURNED BY THIS ROUTINE DESIGNATING
C        WHETHER ANY ERRORS WERE DETECTED IN THE PARAMETER LIST
C        IF IERR .EQ. 0, NO ERRORS WERE DETECTED
C        IF IERR .EQ. 1, ERRORS HAVE BEEN DETECTED
C     INTEGER IFAILJ
C        THE STARTING LOCATION IN ISTAK FOR
C        THE ARRAY OF INDICATOR VARIABLES DESIGNATING WHETHER
C        THE SETP SIZE SELECTED WAS SATISFACOTRY FOR A GIVEN
C        OBSERVATION AND THE JTH PARAMETER.
C     INTEGER IFIXD
C        THE STARTING LOCATION IN ISTAK OF
C        THE INDICATOR VALUES USED TO DESIGNATE WHETHER THE
C        PARAMETERS ARE TO BE OPTIMIZED OR ARE TO BE HELD FIXED.
C     INTEGER IFIXED(LIFIXD)
C        THE INDICATOR VALUES USED TO DESIGNATE WHETHER THE
C        PARAMETERS ARE TO BE OPTIMIZED OR ARE TO BE HELD FIXED.  IF
C        IFIXED(I).NE.0, THEN PAR(I) WILL BE OPTIMIZED.  IF
C        IFIXED(I).EQ.0, THEN PAR(I) WILL BE HELD FIXED.
C     INTEGER IFP
C        AN INDICATOR FOR STACK ALLOCATION TYPE, WHERE IFP=3 INDICATES
C        REAL AND IFP=4 INDICATES DOUBLE PRECISION.
C     INTEGER ISTAK(12)
C        THE INTEGER VERSION OF THE /CSTAK/ WORK AREA.
C     INTEGER ISUBHD
C        AN INDICATOR VALUE SPECIFYING SUBHEADINGS TO BE PRINTED.
C     INTEGER ITEMP
C        THE STARTING LOCATION IN ISTAK FOR
C        A TEMPORARY STORAGE VECTOR.
C     INTEGER IXM
C        THE FIRST DIMENSION OF THE INDEPENDENT VARIABLE ARRAY.
C     INTEGER J
C        THE INDEX OF THE PARAMETER BEING EXAMINED.
C     INTEGER LIFIXD
C        THE LENGTH OF THE VECTOR IFIXED.
C     INTEGER LSCALE
C        THE LENGTH OF VECTOR SCALE.
C     INTEGER M
C        THE NUMBER OF INDEPENDENT VARIABLES.
C     EXTERNAL MDL
C        THE NAME OF THE USER SUPPLIED SUBROUTINE WHICH COMPUTES THE
C        PREDICTED VALUES BASED ON THE CURRENT PARAMETER ESTIMATES.
C     INTEGER MXFAIL
C        THE MAXIMUM NUMBER OF FAILURES FOR ANY PARAMETER.
C     INTEGER N
C        THE NUMBER OF OBSERVATIONS.
C     INTEGER NALL0
C        THE NUMBER OF STACK ALLOCATIONS ON ENTRY.
C     INTEGER NDD
C        THE NUMBER OF DECIMAL DIGITS CARRIED FOR A DOUBLE PRECISION
C        NUMBERS.
C     INTEGER NDGT1
C        THE NUMBER OF RELIABLE DIGITS IN THE MODEL USED, EITHER
C        SET TO THE USER SUPPLIED VALUE OF NETA, OR COMPUTED
C        BY ETAMDL.
C     INTEGER NETA
C        THE USER SUPPLIED NUMBER OF RELIABLE DIGITS IN THE MODEL.
C     INTEGER NEXMPT
C        THE NUMBER OF OBSERVATIONS FOR WHICH A GIVEN STEP SIZE
C        DOES NOT HAVE TO BE SATISFACTORY AND THE SELECTED STEP
C        SIZE STILL BE CONSIDERED OK.
C     INTEGER NFAIL
C        THE NUMBER OF OBSERVATIONS FOR WHICH THE SELECTED STEP SIZE
C        FOR THE PARAMETER DOES NOT MEET THE CRITERIA.
C     INTEGER NFAILJ
C        THE NUMBER OF OBSERVATIONS FOR WHICH THE SELECTED STEP SIZE
C        FOR THE JTH PARAMETER DOES NOT MEET THE CRITERIA.
C     INTEGER NPAR
C        THE NUMBER OF PARAMETERS IN THE MODEL.
C     INTEGER NPRT
C        THE INDICATOR VARIABLE USED TO SPECIFY WHETHER OR NOT
C        PRINTED OUTPUT IS TO BE PROVIDED, WHERE IF THE VALUE OF
C        NPRT IS ZERO, NO PRINTED OUTPUT IS GIVEN.
C     LOGICAL PAGE
C        THE VARIABLE USED TO INDICATE WHETHER A GIVEN SECTION OF
C        THE OUTPUT IS TO BEGIN ON A NEW PAGE (TRUE) OR NOT (FALSE).
C     DOUBLE PRECISION PAR(NPAR)
C        THE ARRAY IN WHICH THE CURRENT ESTIMATES OF THE UNKNOWN
C        PARAMETERS ARE STORED.
C     INTEGER PARTMP
C        THE STARTING LOCATION IN THE WORK AREA OF
C        THE MODIFIED MODEL PARAMETERS
C     LOGICAL PRTFXD
C        THE INDICATOR VALUE USED TO DESIGNATE WHETHER THE
C        OUTPUT IS TO INCLUDE INFORMATION ON WHETHER THE
C        PARAMETER IS FIXED (TRUE) OR NOT (FALSE).
C     INTEGER PV
C        THE STARTING LOCATION IN THE WORK AREA OF
C        THE PREDICTED VALUE BASED ON THE CURRENT PARAMETER ESTIMATES
C     INTEGER PVMCD
C        THE STARTING LOCATION IN THE WORK AREA OF
C        THE PREDICTED VALUE BASED ON THE CURRENT PARAMETER ESTIMATES
C     INTEGER PVNEW
C        THE STARTING LOCATION IN THE WORK AREA OF
C        THE PREDICTED VALUE BASED ON THE CURRENT PARAMETER ESTIMATES
C        FOR ALL BUT THE JTH PARAMETER VALUE, WHICH IS PAR(J)+STPNEW.
C     INTEGER PVPCD
C        THE STARTING LOCATION IN THE WORK AREA OF
C        THE PREDICTED VALUE BASED ON THE CURRENT PARAMETER ESTIMATES
C        FOR ALL BUT THE JTH PARAMETER VALUE, WHICH IS PAR(J)+STPCD.
C     INTEGER PVSTP
C        THE STARTING LOCATION IN THE WORK AREA OF
C        THE PREDICTED VALUE BASED ON THE CURRENT PARAMETER ESTIMATES
C        FOR ALL BUT THE JTH PARAMETER VALUE, WHICH IS PAR(J)+STP(J).
C     INTEGER PVTEMP
C        THE STARTING LOCATION IN THE WORK AREA OF
C        A TEMPORY STORAGE LOCATION FOR PREDICTED VALUES BEGINS.
C     DOUBLE PRECISION Q
C        A DUMMY VARIABLE WHICH IS USED, ALONG WITH COMMON NOTOPT (NO
C        OPTIMIZATION), TO COMPUTE THE STEP SIZE.
C     DOUBLE PRECISION RSTAK(12)
C        THE DOUBLE PRECISION VERSION OF THE /CSTAK/ WORK AREA.
C     DOUBLE PRECISION SCALE(LSCALE)
C        THE TYPICAL SIZE OF THE PARAMETERS.
C     DOUBLE PRECISION SCL
C        THE ACTUAL TYPICAL SIZE USED.
C     DOUBLE PRECISION STP(NPAR)
C        THE SELECTED STEP SIZES.
C     DOUBLE PRECISION TAU
C        THE AGREEMENT TOLERANCE.
C     LOGICAL WIDE
C        THE VARIABLE USED TO INDICATE WHETHER THE HEADING SHOULD
C        BE FULL WIDTH (TRUE) OR NOT (FALSE).
C     DOUBLE PRECISION XM(IXM,M)
C        THE ARRAY IN WHICH ONE ROW OF THE INDEPENDENT VARIABLE ARRAY
C        IS STORED.
C
C
      NALL0 = STKST(1)
C
      FPLRS = D1MACH(4)
      IFP = 4
C
C     SET PRINT CONTROLS
C
      HEAD = .TRUE.
C
C     SUBDIVIDE WORK AREA
C
      IFIXD = STKGET(NPAR, 2)
      ITEMP = STKGET(N, 2)
      IFAILJ = STKGET(N, 2)
      NFAIL = STKGET(NPAR, 2)
C
      CD = STKGET(MAX(N,NPAR), IFP)
      FD = STKGET(N, IFP)
      FDLAST = STKGET(N, IFP)
      FDSAVE = STKGET(N, IFP)
      PV = STKGET(N, IFP)
      PVMCD = STKGET(N, IFP)
      PVNEW = STKGET(N, IFP)
      PVPCD = STKGET(N, IFP)
      PVSTP = STKGET(N, IFP)
      PVTEMP = STKGET(N, IFP)
C
      IF (IERR .EQ. 1) RETURN
C
      PARTMP = CD
C
C     SET UP IFIXD
C
      IF (IFIXED(1).LT.0) THEN
         CALL SETIV(ISTAK(IFIXD), NPAR, 0)
      ELSE
         CALL CPYVII(NPAR, IFIXED, 1, ISTAK(IFIXD), 1)
      END IF
C
C     SET PARAMETERS NECESSARY FOR THE COMPUTATIONS
C
      NDD = INT(-LOG10(FPLRS))
C
      IF ((NETA.GE.2) .AND. (NETA.LE.NDD)) THEN
         ETA = 10.0D0 ** (-NETA)
         NDGT1 = NETA
      ELSE
         CALL ETAMDL(MDL, PAR, NPAR, XM, N, M, IXM, ETA, NDGT1,
     +               RSTAK(PARTMP), RSTAK(PVTEMP), 0, WT)
      END IF
C
      TAU = MIN(ETA ** (0.25D0), 0.01D0)
C
      EXM = EXMPT
      IF ((EXM.LT.0.0D0) .OR. (EXM.GT.1.0D0)) EXM = 0.10D0
      NEXMPT = EXM * N
      IF (EXM .NE. 0.0D0) NEXMPT = MAX(NEXMPT, 1)
C
C     COMPUTE PREDICTED VALUES OF THE MODEL USING THE INPUT PARAMETER
C     ESTIMATES
C
      CALL MDL(PAR, NPAR, XM, N, M, IXM, RSTAK(PV), WT)
C
      MXFAIL = 0
      NFAILJ = NFAIL
C
      DO 120 J = 1, NPAR
         IF (ISTAK(IFIXD-1+J).EQ.0) THEN
            IF (SCALE(1).LE.0.0D0) THEN
               IF (PAR(J).EQ.0.0D0) THEN
                  SCL = 1.0D0
               ELSE
                  SCL = ABS(PAR(J))
               END IF
            ELSE
               SCL = SCALE(J)
            END IF
C
            CALL STPMN(J, XM, N, M, IXM, MDL, PAR, NPAR, NEXMPT,
     +         ETA, TAU, SCL, STP(J), ISTAK(NFAILJ), ISTAK(IFAILJ),
     +         RSTAK(CD), ISTAK(ITEMP), RSTAK(FD), RSTAK(FDLAST),
     +         RSTAK(FDSAVE), RSTAK(PV), RSTAK(PVMCD), RSTAK(PVNEW),
     +         RSTAK(PVPCD), RSTAK(PVSTP), RSTAK(PVTEMP), WT)
C
C     COMPUTE THE MAXIMUM NUMBER OF FAILURES FOR ANY PARAMETER
C
            MXFAIL = MAX(ISTAK(NFAILJ), MXFAIL)
C
         ELSE
            STP(J) = 0.0
         END IF
C
C     PRINT RESULTS IF THEY ARE DESIRED
C
         IF ((NPRT.NE.0) .OR. (MXFAIL.GT.NEXMPT))
     +      CALL STPOUT(HEAD, N, EXM, NEXMPT, NDGT1, J, PAR, NPAR,
     +            STP, ISTAK(NFAIL), ISTAK(IFAILJ), SCALE,  LSCALE, HDR,
     +            PAGE, WIDE, ISUBHD, NPRT, PRTFXD, ISTAK(IFIXD))
         NFAILJ = NFAILJ + 1
  120 CONTINUE
C
      HLFRPT = .FALSE.
      IF ((NPRT.NE.0) .OR. (MXFAIL.GT.NEXMPT)) HLFRPT = .TRUE.
C
      IF (MXFAIL.GT.NEXMPT) IERR = 2
C
      CALL STKCLR(NALL0)
C
      RETURN
C
      END
