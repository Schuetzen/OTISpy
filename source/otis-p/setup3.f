************************************************************************+
*
*     Subroutine SETUP3              Called by: OTISRUN, MAINRUN
*
************************************************************************
      SUBROUTINE SETUP3(IEND,NSOLUTE,JBOUND,TSTART,QSTEP,TIMEB,TSTOP,
     &                  QSTOP,AREA2,DELTAX,Q,USTIME,AREA,ALPHA,QLATIN,
     &                  DFACE,HPLUSF,HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,
     &                  HADV,GAMMA,AFACE,A,C,USCONCN,USCONC,LAMBDA2,
     &                  LAM2DT,BTERMS,AWORK,BWORK,STOPTYPE,LAMBDA,
     &                  BCSTOP,IBOUND,NBOUND,QSTART,USBC,LAMHAT2,
     &                  LHAT2DT,KD,TWOPLUS,LAMHAT,LHATDT,SGROUP,SGROUP2,
     &                  RHOLAM,CSBACK,BN,TGROUP,IGROUP,CLATIN)
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 IEND,NSOLUTE,JBOUND,IBOUND,NBOUND
      DOUBLE PRECISION TSTART,QSTEP,TIMEB,TSTOP,QSTOP,BCSTOP,QSTART
      DOUBLE PRECISION AREA2(*),DELTAX(*),Q(*),USTIME(*),AREA(*),
     &                 ALPHA(*),QLATIN(*),DFACE(*),HPLUSF(*),HPLUSB(*),
     &                 HMULTF(*),HMULTB(*),HDIV(*),HDIVF(*),HADV(*),
     &                 GAMMA(*),AFACE(*),A(*),C(*),USCONCN(*)
      DOUBLE PRECISION USCONC(MAXBOUND,*),LAMBDA2(MAXSEG,*),
     &                 LAM2DT(MAXSEG,*),BTERMS(MAXSEG,*),
     &                 AWORK(MAXSEG,*),BWORK(MAXSEG,*),LAMBDA(MAXSEG,*),
     &                 USBC(MAXBOUND,*),LAMHAT2(MAXSEG,*),
     &                 LHAT2DT(MAXSEG,*),KD(MAXSEG,*),TWOPLUS(MAXSEG,*),
     &                 LAMHAT(MAXSEG,*),LHATDT(MAXSEG,*),
     &                 SGROUP(MAXSEG,*),SGROUP2(MAXSEG,*),
     &                 RHOLAM(MAXSEG,*),CSBACK(MAXSEG,*),BN(MAXSEG,*),
     &                 TGROUP(MAXSEG,*),IGROUP(MAXSEG,*),
     &                 CLATIN(MAXSEG,*)
      CHARACTER*12 STOPTYPE
*
*     local variables
*
      INTEGER*4 I,J
*
*     for now, run preproc for segments 1 to IEND.  There should
*     be a way to just go from ISTART to IEND, see notes
*   
*     In call to PREPROCs, IMAX is replaced by IEND
*
*     Note that in call to PREPROC3, GAMMA, BTERMS, QLATIN, CLATIN and
*     AREA are passed in twice as GAMMAN, BTERMSN, QLATINN, CLATINN and
*     AREAN are undefined.
*
      CALL PREPROC2(IEND,NSOLUTE,TIMEB,LAMBDA2,LAM2DT,LAMHAT2,LHAT2DT,
     &              LAMHAT,LHATDT,SGROUP,SGROUP2,RHOLAM,CSBACK)
      CALL PREPROC3(IEND,DELTAX,Q,AREA,DFACE,QLATIN,HPLUSF,HPLUSB,
     &              HMULTF,HMULTB,HDIV,HDIVF,HADV,GAMMA,BTERMS,AFACE,A,
     &              C,AWORK,BWORK,TIMEB,NSOLUTE,LAM2DT,LHAT2DT,KD,
     &              TWOPLUS,AREA2,ALPHA,LAMBDA,SGROUP,BTERMS,BN,TGROUP,
     &              GAMMA,IGROUP,QLATIN,CLATIN,AREA,CLATIN)
*
*     If IBOUND equals 2 or 3, the value of USCONC may have changed
*     during previous passes through OTISRUN.  Reset USCONC accordingly
*
      DO 20 J = 1,NSOLUTE
         DO 10 I = 1,NBOUND
            IF (IBOUND .EQ. 3) THEN
               USCONC(I,J) = USBC(I,J)
            ELSEIF (IBOUND .EQ. 2) THEN
               USCONC(I,J) = USBC(I,J) / QSTART
            ENDIF
 10      CONTINUE
 20   CONTINUE     
*
*     initialize the upstream boundary condition variables.  For a step
*     boundary condition (IBOUND = 1 or 2), the boundary condition will
*     change at user-specified times indicated by USTIME.  For a
*     continuous boundary condition (IBOUND =3), the b.c. will change
*     every time step.  In either case, set the time at which the
*     boundary condition changes, BCSTOP.
*
      JBOUND = 1

      IF (IBOUND .EQ. 1 .OR. IBOUND .EQ. 2) THEN
         BCSTOP = USTIME(JBOUND+1)
      ELSEIF (IBOUND .EQ. 3) THEN
         BCSTOP = TSTART
      ENDIF
*
*     initialize the boundary condition for time step 'N', USCONCN.
*
      DO 30 J = 1,NSOLUTE
         USCONCN(J) = USCONC(1,J)
 30   CONTINUE
*
*     initialize the time at which a change in flow occurs.  if the flow
*     is steady, set QSTOP to an arbitrarily large number.
*
      IF (QSTEP .EQ. 0.D0) THEN
         QSTOP = 999.D99
      ELSE
         QSTOP = TSTART + QSTEP
      ENDIF
*
*     determine TSTOP, the time at which the upstream boundary condition
*     or flow variables change.
*
      CALL SETSTOP(QSTOP,TSTOP,STOPTYPE,BCSTOP,IBOUND)

      RETURN
      END

