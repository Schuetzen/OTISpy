************************************************************************
*
*     Subroutine MAINRUN        Called by: MAIN Program 
*
*     execute a final OTIS run using the final set of parameter
*     estimates and output results.
*
************************************************************************
      SUBROUTINE MAINRUN(IMAX,NPRINT,PINDEX,WT,IPRINT,AREA2,DELTAX,Q,
     &                   USTIME,USCONC,USCONCN,AREA,ALPHA,DSBOUND,
     &                   QLATIN,CLATIN,TIME,TFINAL,TSTEP,CONC,CONC2,
     &                   QSTEP,PRTOPT,NSOLUTE,CHEM,NFLOW,QINDEX,QWT,
     &                   STOPTYPE,JBOUND,TIMEB,TSTOP,QSTOP,DFACE,HPLUSF,
     &                   HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,HADV,GAMMA,
     &                   BTERMS,AFACE,A,C,AWORK,BWORK,LAM2DT,DSDIST,
     &                   USDIST,QINVAL,CLVAL,AVALUE,QVALUE,IBOUND,USBC,
     &                   NBOUND,BCSTOP,SORB,LHAT2DT,KD,ISORB,LAMBDA,
     &                   QSTART,DIST,PRTLOC,CLATINN,QN,AREAN,QLATINN,
     &                   GAMMAN,AFACEN,AN,CN,BTERMSN,TWOPLUS,SGROUP2,
     &                   SGROUP,LHATDT,BN,TGROUP,IGROUP,LAMBDA2,CSBACK,
     &                   LAMHAT2,RHOLAM,LAMHAT)
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 IMAX,NPRINT,IPRINT,NSOLUTE,PRTOPT,NFLOW,JBOUND,IBOUND,
     &          NBOUND,ISORB
      INTEGER*4 PINDEX(*),QINDEX(*)
      DOUBLE PRECISION DSBOUND,TIME,TFINAL,TSTEP,QSTEP,TIMEB,TSTOP,
     &                 QSTOP,BCSTOP,QSTART
      DOUBLE PRECISION AREA2(*),DELTAX(*),Q(*),USTIME(*),AREA(*),
     &                 ALPHA(*),QLATIN(*),WT(*),QWT(*),DSDIST(*),
     &                 USDIST(*),QINVAL(*),DFACE(*),HPLUSF(*),HPLUSB(*),
     &                 HMULTF(*),HMULTB(*),HDIV(*),HDIVF(*),HADV(*),
     &                 GAMMA(*),AFACE(*),A(*),C(*),LAM2DT(*),AVALUE(*),
     &                 QVALUE(*),USCONCN(*),DIST(*),PRTLOC(*),QN(*),
     &                 AREAN(*),QLATINN(*),AFACEN(*),GAMMAN(*),AN(*),
     &                 CN(*)
      DOUBLE PRECISION USCONC(MAXBOUND,*),CLATIN(MAXSEG,*),
     &                 CONC(MAXSEG,*),CONC2(MAXSEG,*),
     &                 CLVAL(MAXFLOWLOC+1,*),BTERMS(MAXSEG,*),
     &                 AWORK(MAXSEG,*),BWORK(MAXSEG,*),USBC(MAXBOUND,*),
     &                 CSBACK(MAXSEG,*),LAMHAT2(MAXSEG,*),
     &                 SORB(MAXSEG,*),LHAT2DT(MAXSEG,*),KD(MAXSEG,*),
     &                 LAMBDA(MAXSEG,*),CLATINN(MAXSEG,*),
     &                 BTERMSN(MAXSEG,*),TWOPLUS(MAXSEG,*),
     &                 LAMBDA2(MAXSEG,*),SGROUP2(MAXSEG,*),
     &                 SGROUP(MAXSEG,*),LHATDT(MAXSEG,*),BN(MAXSEG,*),
     &                 TGROUP(MAXSEG,*),IGROUP(MAXSEG,*),
     &                 RHOLAM(MAXSEG,*),LAMHAT(MAXSEG,*)
      CHARACTER*(*) CHEM,STOPTYPE
*
*     local variables
*
      INTEGER*4 J,NCALLS
*
*     arguments passed to OUTPUT
*
      INTEGER*4 JSOLUTE
      DOUBLE PRECISION CNC(MAXPRINT),CNC2(MAXPRINT)
*
*     re-initialize varaibles based on final parameter set
*
      IF (CHEM .NE. 'Steady-State') THEN
         IF (QSTEP .NE. 0)
     &      CALL SETUP2(QINVAL,CLVAL,QVALUE,AVALUE,NSOLUTE,NFLOW,IMAX,Q,
     &                  AREA,QWT,QINDEX,QLATIN,CLATIN,DSDIST,USDIST)
         CALL SETUP3(IMAX,NSOLUTE,JBOUND,TIME,QSTEP,TIMEB,TSTOP,
     &               QSTOP,AREA2,DELTAX,Q,USTIME,AREA,ALPHA,QLATIN,
     &               DFACE,HPLUSF,HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,HADV,
     &               GAMMA,AFACE,A,C,USCONCN,USCONC,LAMBDA2,LAM2DT,
     &               BTERMS,AWORK,BWORK,STOPTYPE,LAMBDA,BCSTOP,IBOUND,
     &               NBOUND,QSTART,USBC,LAMHAT2,LHAT2DT,KD,TWOPLUS,
     &               LAMHAT,LHATDT,SGROUP,SGROUP2,RHOLAM,CSBACK,BN,
     &               TGROUP,IGROUP,CLATIN)
         CALL SAVEN(IMAX,NSOLUTE,AFACE,AFACEN,AREA,AREAN,GAMMA,GAMMAN,Q,
     &              QLATIN,QLATINN,QN,CLATIN,CLATINN,AN,CN,A,C,BTERMS,
     &              BTERMSN,TIMEB,TGROUP,BN,TWOPLUS,ALPHA,IGROUP)
      ENDIF

      CALL STEADY(IMAX,AREA2,DELTAX,Q,USCONC,AREA,ALPHA,DSBOUND,QLATIN,
     &            CLATIN,CONC,CONC2,NSOLUTE,LAMBDA,LAMBDA2,DFACE,HPLUSF,
     &            HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,HADV,LAMHAT2,CSBACK,
     &            KD,SORB)
*
*     Open the solute output files
*
      CALL OPENFILE2(NSOLUTE,ISORB)
*
*     Output Initial conditions
*
      CALL OUTINIT(NPRINT,PINDEX,PRTLOC,Q,CONC,CONC2,TIME,WT,NSOLUTE,
     &             PRTOPT,CHEM,ISORB,SORB)
*
*     compute the solution based on the final parameter set
*
      IF (CHEM .NE. 'Steady-State') THEN
*
*        compute the required number of calls to the dynamic solution
*        routine.  Each call advances the solution (in time) to the next
*        output point.
*
         NCALLS = INT((INT((TFINAL - TIME)/TSTEP) + 1) / IPRINT) + 1
*
*        begin the time loop
*
         DO 20 J = 1, NCALLS

            CALL DYNAMIC(IMAX,IPRINT,DELTAX,Q,USTIME,USCONC,USCONCN,
     &                   AREA,DSBOUND,QLATIN,CLATIN,TIME,TSTEP,CONC,
     &                   CONC2,QSTEP,NSOLUTE,LAMBDA,QVALUE,AVALUE,QWT,
     &                   QINDEX,NFLOW,QINVAL,CLVAL,CHEM,DSDIST,USDIST,
     &                   STOPTYPE,JBOUND,TIMEB,TSTOP,QSTOP,DFACE,HPLUSF,
     &                   HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,HADV,GAMMA,
     &                   BTERMS,AFACE,A,C,AWORK,BWORK,LAM2DT,IBOUND,
     &                   USBC,NBOUND,BCSTOP,SGROUP2,SGROUP,LHATDT,SORB,
     &                   LHAT2DT,KD,CLATINN,QN,AREAN,QLATINN,GAMMAN,
     &                   AFACEN,AN,CN,BTERMSN,TWOPLUS,AREA2,ALPHA,BN,
     &                   TGROUP,IGROUP)

            DO 10 JSOLUTE = 1, NSOLUTE
               CALL OUTPUT(NPRINT,PINDEX,CONC,CONC2,TIME,WT,JSOLUTE,
     &                     PRTOPT,CNC,CNC2,ISORB,SORB)
 10         CONTINUE
 20      CONTINUE

      ELSEIF (CHEM .EQ. 'Steady-State') THEN
         CALL OUTPUTSS(CONC,CONC2,PRTOPT,ISORB,SORB,NSOLUTE,IMAX,DIST)
      ENDIF

      RETURN
      END
