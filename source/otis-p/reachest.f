************************************************************************
*
*     Subroutine REACHEST              Called by: MAIN
*
*     estimate parameters for the each reach.
*
************************************************************************
      SUBROUTINE REACHEST(MIT,IVAPRX,NPRT,DELTA,STOPSS,STOPP,SCALE,
     &                    ICOUNT,NPAR,NPAREST,LABEL,DIST,IMAX,NREACH,
     &                    JREACH,ISTART,IEND,IDATA,IFIXED,INDEXPV,
     &                    LASTSEG,TSTART,TEND,TSTEP,AREA2,DELTAX,AREA,
     &                    ALPHA,DFACE,WTPV,LAMBDA,KD,CHEM,LAMBDA2,
     &                    LAMHAT2,RHOLAM,LAMHAT)
      INCLUDE 'fmodules.inc'
      INCLUDE 'fmodules2.inc'
      INCLUDE 'lda.inc'
*
*     subroutine arguments (from otis.cmn)
*
      INTEGER*4 NREACH,ISTART,IEND,IDATA,JREACH
      INTEGER*4 IFIXED(*),INDEXPV(*),LASTSEG(0:MAXREACH)
      DOUBLE PRECISION TSTART,TEND,TSTEP
      DOUBLE PRECISION AREA2(*),DELTAX(*),AREA(*),ALPHA(*),DFACE(*),
     &                 WTPV(*)
      DOUBLE PRECISION LAMBDA2(MAXSEG,*),LAMBDA(MAXSEG,*),
     &                 RHOLAM(MAXSEG,*),LAMHAT(MAXSEG,*),
     &                 LAMHAT2(MAXSEG,*),KD(MAXSEG,*)
      CHARACTER*(*) CHEM
*
*     other subroutine arguments
*
      INTEGER*4 MIT,IVAPRX,NPRT,ICOUNT,NPAR,NPAREST,IMAX
      DOUBLE PRECISION DELTA,STOPSS,STOPP,SCALE(*),DIST(*)
      CHARACTER*(*) LABEL(MAXPAR,2)
*
*     local variables, STARPAC Basic Declaration Block & NLSWC
*
      INTEGER*4 I,K,N
      DOUBLE PRECISION Y(MAXOBS),RES(MAXOBS),OBSWT(MAXOBS),XM(MAXOBS),
     &                 PAR(MAXPAR),STP(MAXPAR),DSTAK(LDSTAK),
     &                 PARVAL(MAXPAR)
      CHARACTER*500 BUFFER
      COMMON /CSTAK/ DSTAK
      EXTERNAL OTISRUN
*
*     input format statements
*
 1000 FORMAT(8I5)
 1010 FORMAT(2F15.0)
*
*     output format statements
*
 2000 FORMAT(//,10X,'Observed Concentration Data, Reach #',I3,/,10X,
     &       39('-'),/,10X,'Number of Observations:',I3,//,12X,A8,5X,
     &       'Concentration',/,10X,40('-'))
 2010 FORMAT(8X,F11.4,5X,1PE11.4)
 2020 FORMAT(/////,5X,'STARPAC output for Reach #',I3,///) 
 2030 FORMAT(2X,10(1X,A10,3X))
 2040 FORMAT(2X,10(A15))
 2050 FORMAT(///,2X,'Parameter Estimation, Reach #',I3,/,2X,32('-'),/)
 2060 FORMAT(//,2X,'Estimated Parameters, Reach #',I3,/,2X,32('-'),/)
 2070 FORMAT(1P,6(E13.5,1X))
*
*     estimate parameters for each reach
*
      DO 50 JREACH=1,NREACH
*
*        read the number of observations and the observed data
*
         CALL GETLINE(LDDATA,BUFFER)
         READ(BUFFER,1000) N
         IF (N .GT. MAXOBS) CALL ERROR4(1,N,MAXOBS)

         IF (CHEM .EQ. 'Steady-State') THEN
            WRITE(LDECHO,2000) JREACH,N,'Distance'
         ELSE
            WRITE(LDECHO,2000) JREACH,N,'  Time'
         ENDIF

         DO 10 I=1,N
            CALL GETLINE(LDDATA,BUFFER)
            READ(BUFFER,1010) XM(I),Y(I)
            WRITE(LDECHO,2010) XM(I),Y(I)
            IF (CHEM .NE. 'Steady-State') THEN
               CALL DATACHK(XM,I,TSTEP,TSTART)
            ELSE
               CALL WEIGHTS2(IMAX,INDEXPV,WTPV,XM,DIST,I)
            ENDIF
 10      CONTINUE
*
*        increment ICOUNT, the number of reaches for which parameter
*        estimates apply.  ICOUNT is included so that reaches w/o data
*        (N=0) may be specified.  If ICOUNT equals 1, this is the first
*        reach in the current estimation cycle.  Therefore define the
*        the starting segment, ISTART.
*
         ICOUNT = ICOUNT + 1
         IF (ICOUNT .EQ. 1) ISTART = LASTSEG(JREACH-1) + 1
*
*        if there is data associated with the current reach, estimate
*        the parameter set that applies to this and the previous
*        ICOUNT-1 reaches.
*
         IF (N .GT. 0) THEN
*
*           increment the counter indicating the number of reaches w/
*           data
*
            IDATA = IDATA + 1
*
*           define the segment ending (IEND) the current reach.  Except
*           for the last reach, IEND is set such that the modeled system
*           extends 15 segments into the next reach.  Check to make sure
*           that IEND doesn't extend beyond the end of the last reach.
* 
            IF (JREACH .NE. NREACH) THEN
               IEND = LASTSEG(JREACH) + 15
               IF (IEND .GT. LASTSEG(NREACH)) IEND = LASTSEG(NREACH)
            ELSE
               IEND = LASTSEG(JREACH)
            ENDIF
*
*           set the simulation end time, TEND, based on the time of the
*           final observation
*
            IF (CHEM .NE. 'Steady-State') TEND = XM(N) + 2.D0*TSTEP
*
*           set initial parameter values using values from the parameter
*           file
*
            PAR(1) = DFACE(ISTART)
            PAR(2) = AREA(ISTART)
            PAR(3) = AREA2(ISTART)
            PAR(4) = ALPHA(ISTART)
            PAR(5) = LAMBDA(ISTART,1)
            PAR(6) = LAMBDA2(ISTART,1)
            PAR(8) = KD(ISTART,1)
            PAR(9) = LAMHAT(ISTART,1)
            PAR(10) = LAMHAT2(ISTART,1)

            IF (LAMHAT(ISTART,1) .NE. 0.D0) THEN
               PAR(7) = RHOLAM(ISTART,1)/LAMHAT(ISTART,1)
            ELSE
               PAR(7) = 0.D0
            ENDIF
*
*           initialize STARPAC parameters for this reach
*        
            DO 20 I=1,NPAR
               STP(I) = 0.D0
 20         CONTINUE
            DO 30 I=1,N
               OBSWT(I) = 1.D0
 30         CONTINUE
*
*           write headings for the STARPAC output file and the parameter
*           estimate output file
*
            WRITE (LDOUT2,2020) JREACH
            WRITE (LDOUT,2050) JREACH
            WRITE (LDOUT,2030) (LABEL(I,1), I=1,NPAREST)
            WRITE (LDOUT,2030) (LABEL(I,2), I=1,NPAREST)
            WRITE (LDOUT,2040) ('---------------', I=1,NPAREST)
*
*           perform nonlinear weighted regression w/numerically
*           approximated derivatives
*
            CALL NLSWC(Y,OBSWT,XM,N,1,MAXOBS,OTISRUN,PAR,NPAR,RES,
     &                 LDSTAK,IFIXED,STP,MIT,STOPSS,STOPP,SCALE,DELTA,
     &                 IVAPRX,NPRT)
*
*           fill a PARVAL with estimated parameter values and write the
*           final parameter values to LDOUT.  Note that only values for
*           estimated parameters (IFIXED=0) are output.
*
            WRITE (LDOUT,2060) JREACH
            WRITE (LDOUT,2030) (LABEL(I,1), I=1,NPAREST)
            WRITE (LDOUT,2030) (LABEL(I,2), I=1,NPAREST)
            WRITE (LDOUT,2040) ('---------------', I=1,NPAREST)

            K = 0
            DO 40 I=1,NPAR
               IF (IFIXED(I) .EQ. 0) THEN
                  K = K + 1
                  PARVAL(K) = PAR(I)
               ENDIF
 40         CONTINUE
            WRITE (LDOUT,2070) (PARVAL(I), I=1,K)
*
*           given the final parameter estimates, set up all the
*           variables for reach JREACH.
*
            CALL SETUP(ISTART,PAR,AREA2,DELTAX,AREA,ALPHA,DFACE,LASTSEG,
     &                 JREACH,LAMBDA,LAMBDA2,NREACH,IFIXED,RHOLAM,KD,
     &                 LAMHAT,LAMHAT2)
*
*           reset the counter indicating the number of reaches in the
*           estimation cycle.
*
            ICOUNT = 0

         ENDIF
 50   CONTINUE
*
*     close files
*
      CLOSE (UNIT=LDDATA)
      CLOSE (UNIT=LDOUT)
      CLOSE (UNIT=LDOUT2)

      RETURN
      END
