*STPSEL
      SUBROUTINE STPSEL(XM, N, M, IXM, MDL, PAR, NPAR,
     +   NEXMPT, STP, NFAIL, IFAIL, J, ETA3, RELTOL, ABSTOL, TAUABS,
     +   STPLOW, STPMID, STPUP, ITEMP, FD, FDLAST, FDSAVE, PV, PVNEW,WT)
C
C     LATEST REVISION  -  03/15/90  (JRD)
C
C     THIS SUBROUTINE SELECTS NEW STEP SIZES UNITL EITHER
C     THE NUMBER OF OBSERVATIONS AT WHICH THE SELECTION CRITERIA
C     IS NOT MET DOES NOT EXCEED NEXMPT OR UNTIL NO FURTHER
C     IMPROVEMENT CAN BE MADE.
C
C     WRITTEN BY  -  ROBERT B. SCHNABEL (CODED BY JANET R. DONALDSON)
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
     +   ABSTOL,ETA3,RELTOL,STP,STPLOW,STPMID,STPUP,TAUABS
      INTEGER
     +   IXM,J,M,N,NEXMPT,NFAIL,NPAR
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   FD(N),FDLAST(N),FDSAVE(N),PAR(NPAR),PV(N),PVNEW(N),XM(IXM,M),
     &   WT(*)
      INTEGER
     +   IFAIL(N),ITEMP(N)
C
C  SUBROUTINE ARGUMENTS
      EXTERNAL MDL
C
C  SCALARS IN COMMON
      DOUBLE PRECISION
     +   Q
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   FACTOR,STP1,STP2,STPNEW,TEMP
      INTEGER
     +   NCOUNT
      LOGICAL
     +   FAIL,FIRST,FORWRD,HICURV,SUCCES
C
C  EXTERNAL SUBROUTINES
      EXTERNAL ABSCOM,CMPFD,ICOPY,RELCOM,DCOPY,STPADJ
C
C  INTRINSIC FUNCTIONS
      INTRINSIC ABS
C
C  COMMON BLOCKS
      COMMON /NOTOPT/Q
C
C     VARIABLE DEFINITIONS (ALPHABETICALLY)
C
C     DOUBLE PRECISION ABSTOL
C        THE ABSOLUTE AGREEMENT TOLERANCE.
C     DOUBLE PRECISION ETA3
C        THE CUBE ROOT OF THE RELATIVE NOISE IN THE MODEL
C     DOUBLE PRECISION FACTOR
C        A FACTOR USED IN COMPUTING THE STEP SIZE.
C     LOGICAL FAIL
C        THE VARIABLE USED TO INDICATE WHETHER A STEP SIZE
C        CANNOT BE SELECTED WHICH WILL SUCCESSFULLY MEET THE CRITERIA.
C     DOUBLE PRECISION FD(N)
C        THE FORWARD DIFFERENCE QUOTIENT APPROXIMATION TO THE
C        DERIVATIVE OF THE MODEL WITH RESPECT TO THE JTH PARAMETER
C     DOUBLE PRECISION FDLAST(N)
C        THE FORWARD DIFFERENCE QUOTIENT APPROXIMATION TO THE
C        DERIVATIVE OF THE MODEL WITH RESPECT TO THE JTH PARAMETER
C        COMPUTED WITH THE MOST RECENT STEP SIZE SELECTED.
C     DOUBLE PRECISION FDSAVE(N)
C        A VECTOR USED TO SAVE THE BEST OF THE
C        THE FORWARD DIFFERENCE QUOTIENT APPROXIMATIONS TO THE
C        DERIVATIVE OF THE MODEL WITH RESPECT TO THE JTH PARAMETER
C     LOGICAL FIRST
C        THE VARIABLE USED TO INDICATE WHETHER THIS STEP SIZE
C        IS BEING USED FOR THE FIRST TIME OR WHETHER IT HAS BEEN
C        PREVIOUSLY ADJUSTED.
C     LOGICAL FORWRD
C        THE VARIABLE USED TO INDICATE THE DIRECTION OF CHANGE IN
C        THE STEP SIZE.
C     LOGICAL HICURV
C        THE VARIABLE USED TO INDICATE WHETHER THE MODEL HAS
C        HIGH CURVATURE.
C     INTEGER IFAIL(N)
C        AN INDICATOR VECTOR USED TO DESIGNATE THOSE OBSERVATIONS
C        FOR WHICH THE STEP SIZE DOES NOT MEET THE CRITERIA.
C     INTEGER ITEMP(N)
C        A TEMPORARY VECTOR USED FOR STORING PAST VALUES OF ITEMP.
C     INTEGER IXM
C        THE FIRST DIMENSION OF THE INDEPENDENT VARIABLE ARRAY.
C     INTEGER J
C        THE INDEX OF THE PARAMETER BEING EXAMINED.
C     INTEGER M
C        THE NUMBER OF INDEPENDENT VARIABLES.
C     EXTERNAL MDL
C        THE NAME OF THE USER SUPPLIED SUBROUTINE WHICH COMPUTES THE
C        PREDICTED VALUES BASED ON THE CURRENT PARAMETER ESTIMATES.
C     INTEGER N
C        THE NUMBER OF OBSERVATIONS.
C     INTEGER NPAR
C        THE NUMBER OF UNKNOWN PARAMETERS IN THE MODEL.
C     INTEGER NCOUNT
C        THE NUMBER OF OBSERVATIONS AT WHICH THE NEW STEP SIZE DOES
C        SATISFY THE CRITERIA.
C     INTEGER NEXMPT
C        THE NUMBER OF OBSERVATIONS FOR WHICH A GIVEN STEP SIZE
C        DOES NOT HAVE TO BE SATISFACTORY AND THE SELECTED STEP
C        SIZE STILL BE CONSIDERED OK.
C     INTEGER NFAIL
C        A VECTOR CONTAINING FOR EACH OBSERVATION THE NUMBER OF
C        OBSERVATIONS FOR WHICH THE STEP SIZE DID NOT MEET THE CRITERIA.
C     DOUBLE PRECISION PAR(NPAR)
C        THE ARRAY IN WHICH THE CURRENT ESTIMATES OF THE UNKNOWN
C        PARAMETERS ARE STORED.
C     DOUBLE PRECISION PV(N)
C        THE PREDICTED VALUE BASED ON THE CURRENT PARAMETER ESTIMATES
C     DOUBLE PRECISION PVNEW(N)
C        THE PREDICTED VALUE BASED ON THE CURRENT PARAMETER ESTIMATES
C        FOR ALL BUT THE JTH PARAMETER VALUE, WHICH IS PAR(J)+STPCD.
C     DOUBLE PRECISION Q
C        A DUMMY VARIABLE WHICH IS USED, ALONG WITH COMMON NOTOPT (NO
C        OPTIMIZATION), TO COMPUTE THE STEP SIZE.
C     DOUBLE PRECISION STP
C        THE STEP SIZE CURRENTLY BEING EXAMINED FOR THE FORWARD
C        DIFFERENCE APPROXIMATION TO THE DERIVATIVE.
C     DOUBLE PRECISION STPLOW
C        THE LOWER LIMIT ON THE STEP SIZE.
C     DOUBLE PRECISION STPMID
C        THE MIDPOINT OF THE ACCEPTABLE RANGE OF THE STEP SIZE.
C     DOUBLE PRECISION STPNEW
C        THE VALUE OF THE NEW STEP SIZE BEING TESTED.
C     DOUBLE PRECISION STPUP
C        THE UPPER LIMIT ON THE STEP SIZE.
C     DOUBLE PRECISION STP1, STP2
C        TEMPORARY STORAGE LOCATIONS FOR STEP SIZES.
C     LOGICAL SUCCES
C        THE VARIABLE USED TO INDICATE WHETHER THE STEP SIZE
C        SUCCESSFULLY MEETS THE CRITERIA USED TO SELECT THE STEP
C        SIZES.
C     DOUBLE PRECISION RELTOL
C        THE RELATIVE AGREEMENT TOLERANCE.
C     DOUBLE PRECISION TAUABS
C        THE ABSOLUTE AGREEMENT TOLERANCE.
C     DOUBLE PRECISION TEMP
C        A TEMPORARY LOCATION IN WHICH THE CURRENT ESTIMATE OF THE JTH
C        PARAMETER IS STORED.
C     DOUBLE PRECISION XM(IXM,M)
C        THE ARRAY IN WHICH ONE ROW OF THE INDEPENDENT VARIABLE ARRAY
C        IS STORED.
C
      CALL DCOPY(N, FD, 1, FDSAVE, 1)
C
      FACTOR = 10.0D0
      IF (ABS(STP) .GT. STPMID) FACTOR = 0.1D0
C
      STPNEW = STP * FACTOR
      STP1 = STPNEW
      STP2 = STPNEW
C
      Q = STPNEW + PAR(J)
      STPNEW = Q - PAR(J)
C
      FIRST = .TRUE.
      FORWRD = .TRUE.
      SUCCES = .FALSE.
      FAIL = .FALSE.
C
      NFAIL = N + 1
C
C     REPEAT FOLLOWING UNTIL (SUCCES) OR (FAIL)
C
   10 CONTINUE
C
      CALL DCOPY(N, FD, 1, FDLAST, 1)
C
      TEMP = PAR(J)
      PAR(J) = TEMP + STPNEW
C
      CALL MDL(PAR, NPAR, XM, N, M, IXM, PVNEW, WT)
C
      PAR(J) = TEMP
C
      CALL CMPFD(N, STPNEW, PVNEW, PV, FD)
C
      CALL RELCOM(N, FD, FDLAST, RELTOL, ABSTOL, NCOUNT, ITEMP)
C
      IF (NCOUNT.LE.NEXMPT) THEN
            SUCCES = .TRUE.
            NFAIL = NCOUNT
            CALL ICOPY(N, ITEMP, 1, IFAIL, 1)
            IF (ABS(ABS(STPNEW) - STPMID) .GT.
     +         ABS(ABS(STPNEW/FACTOR) - STPMID)) THEN
                  STP = STPNEW / FACTOR
            ELSE
                  STP = STPNEW
            END IF
      ELSE
            IF (NCOUNT.LT.NFAIL) THEN
                  NFAIL = NCOUNT
                  STP1 = STPNEW
                  STP2 = STPNEW / FACTOR
                  CALL ICOPY(N, ITEMP, 1, IFAIL, 1)
            END IF
            IF (FIRST) THEN
                  FIRST = .FALSE.
                  CALL ABSCOM(N, FD, FDLAST, TAUABS, NCOUNT)
                  IF (NCOUNT.LE.NEXMPT) THEN
                         HICURV = .TRUE.
                  ELSE
                         HICURV = .FALSE.
                  END IF
            END IF
            STPNEW = STPNEW * FACTOR
            Q = STPNEW + PAR(J)
            STPNEW = Q - PAR(J)
            IF ((FACTOR.GT.1.0D0 .AND. ABS(STPNEW).GT.STPUP) .OR.
     +          (FACTOR.LT.1.0D0 .AND. ABS(STPNEW).LT.STPLOW)) THEN
                  IF (FORWRD) THEN
                        FORWRD = .FALSE.
                        FACTOR = 1.0D0 / FACTOR
                        STPNEW = STP * FACTOR
                        Q = STPNEW + PAR(J)
                        STPNEW = Q - PAR(J)
                        CALL DCOPY(N, FDSAVE, 1, FD, 1)
                        STPLOW = STPLOW * (ETA3)
                        STPUP = STPUP / (ETA3)
                  ELSE
                        FAIL = .TRUE.
                  END IF
            END IF
      END IF
C
      IF (.NOT.(SUCCES.OR.FAIL)) GO TO 10
C
      IF (SUCCES .AND. FORWRD) THEN
            CALL STPADJ(XM, N, M, IXM, MDL, PAR, NPAR,
     +         NEXMPT, STP, NFAIL, IFAIL, J, RELTOL, ABSTOL, STPLOW,
     +         STPMID, STPUP, ITEMP, FD, FDLAST, PV, PVNEW, WT)
            RETURN
      ELSE
            IF (SUCCES) THEN
                  RETURN
            ELSE
C                 IF (HICURV) NFAIL = -NFAIL
C
                  IF (ABS(STP1).LT.ABS(STP2)) THEN
                        STP = STP1
                        RETURN
                  ELSE
                        STP = STP2
                        RETURN
                  END IF
            END IF
      END IF
C
      END
