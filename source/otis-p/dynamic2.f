************************************************************************
*
*     Subroutine DYNAMIC2               Called by: OTISRUN
*
*     determine the predicted values of solute concentration based on
*     current parameter values.
*
************************************************************************
      SUBROUTINE DYNAMIC2(IEND,DELTAX,Q,USTIME,USCONC,USCONCN,AREA,
     &                    DSBOUND,QLATIN,CLATIN,TSTART,TSTEP,CONC,CONC2,
     &                    QSTEP,NSOLUTE,LAMBDA,QVALUE,AVALUE,QWT,QINDEX,
     &                    NFLOW,QINVAL,CLVAL,CHEM,DSDIST,USDIST,
     &                    STOPTYPE,JBOUND,TIMEB,TSTOP,QSTOP,DFACE,
     &                    HPLUSF,HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,HADV,
     &                    GAMMA,BTERMS,AFACE,A,C,AWORK,BWORK,LAM2DT,
     &                    IBOUND,USBC,NBOUND,BCSTOP,SGROUP2,SGROUP,
     &                    LHATDT,SORB,LHAT2DT,KD,CLATINN,QN,AREAN,
     &                    QLATINN,GAMMAN,AFACEN,AN,CN,BTERMSN,TWOPLUS,
     &                    AREA2,ALPHA,BN,TGROUP,IGROUP,TEND,XM,IDATA,
     &                    PINDEX,WT,PV)
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      CHARACTER*(*) CHEM,STOPTYPE
      INTEGER*4 IEND,NSOLUTE,NFLOW,JBOUND,IBOUND,NBOUND,IDATA
      INTEGER*4 QINDEX(*),PINDEX(*)
      DOUBLE PRECISION DSBOUND,TSTART,TSTEP,QSTEP,TIMEB,TSTOP,QSTOP,
     &                 BCSTOP,TEND
      DOUBLE PRECISION DELTAX(*),Q(*),USTIME(*),AREA(*),QLATIN(*),
     &                 QWT(*),QINVAL(*),DSDIST(*),USDIST(*),DFACE(*),
     &                 HPLUSF(*),HPLUSB(*),HMULTF(*),HMULTB(*),HDIV(*),
     &                 HDIVF(*),HADV(*),GAMMA(*),AFACE(*),A(*),C(*),
     &                 QVALUE(*),AVALUE(*),USCONCN(*),QN(*),AREAN(*),
     &                 QLATINN(*),AFACEN(*),GAMMAN(*),AN(*),CN(*),
     &                 AREA2(*),ALPHA(*),XM(*),PV(*),WT(*)
      DOUBLE PRECISION USCONC(MAXBOUND,*),CLATIN(MAXSEG,*),
     &                 CONC(MAXSEG,*),CONC2(MAXSEG,*),LAMBDA(MAXSEG,*),
     &                 LAM2DT(MAXSEG,*),CLVAL(MAXFLOWLOC+1,*),
     &                 AWORK(MAXSEG,*),BWORK(MAXSEG,*),USBC(MAXBOUND,*),
     &                 SGROUP2(MAXSEG,*),SGROUP(MAXSEG,*),
     &                 LHATDT(MAXSEG,*),SORB(MAXSEG,*),
     &                 LHAT2DT(MAXSEG,*),KD(MAXSEG,*),CLATINN(MAXSEG,*),
     &                 TWOPLUS(MAXSEG,*),BN(MAXSEG,*),TGROUP(MAXSEG,*),
     &                 BTERMS(MAXSEG,*),BTERMSN(MAXSEG,*),
     &                 IGROUP(MAXSEG,*)
*
*     local variables
*
      INTEGER*4 J,NCALLS,IOBS
      DOUBLE PRECISION TIME
      LOGICAL FOUND
*
*     initialize and begin the time loop
*
      FOUND = .FALSE.
      IOBS = 1
      TIME = TSTART
      NCALLS = INT((TEND - TSTART)/TSTEP) + 2

      DO 20 J = 1, NCALLS

         TIME = TIME + TSTEP
*
*        check to see if the boundary condition or flow values change.
*        IF statement executes a `dowhile TIME > TSTOP'
*
 10      IF (TIME .GT. TSTOP+1.D-7) THEN
*
*           Update the boundary conditions and/or the flow variables.
*
            CALL UPDATE(IEND,Q,AREA,QLATIN,CLATIN,NSOLUTE,QVALUE,AVALUE,
     &                  QWT,QINDEX,NFLOW,JBOUND,STOPTYPE,QSTOP,QSTEP,
     &                  USTIME,TSTOP,TIMEB,DELTAX,DFACE,HPLUSF,HPLUSB,
     &                  HMULTF,HMULTB,HDIV,HDIVF,HADV,GAMMA,BTERMS,
     &                  AFACE,A,C,AWORK,BWORK,QINVAL,CLVAL,DSDIST,
     &                  USDIST,LAMBDA,LAM2DT,IBOUND,USBC,NBOUND,USCONC,
     &                  BCSTOP,TSTEP,TIME,LHAT2DT,SGROUP,KD,TWOPLUS,
     &                  AREA2,ALPHA,BN,BTERMSN,TGROUP,GAMMAN,IGROUP,
     &                  QLATINN,CLATINN,AREAN)
            GOTO 10
         ENDIF
*
*        compute concentrations
*
         IF (CHEM .EQ. 'Conservative') THEN         
            CALL CONSER(IEND,JBOUND,DELTAX,Q,USCONC,USCONCN,AREA,DFACE,
     &                  DSBOUND,CONC,CONC2,NSOLUTE,GAMMA,AFACE,AWORK,
     &                  BWORK,QN,AREAN,GAMMAN,AFACEN,AN,CN,C,TWOPLUS,BN,
     &                  TGROUP,IGROUP)
         ELSE
            CALL REACT(IEND,JBOUND,DELTAX,Q,USCONC,USCONCN,AREA,DFACE,
     &                 DSBOUND,CONC,CONC2,NSOLUTE,GAMMA,AFACE,AWORK,
     &                 BWORK,LAM2DT,SGROUP2,SGROUP,LHATDT,SORB,LHAT2DT,
     &                 KD,QN,AREAN,GAMMAN,AFACEN,AN,CN,C,TWOPLUS,ALPHA,
     &                 BN,TGROUP,IGROUP)
         ENDIF
*
*        for unsteady flow, save various parameters at time step 'N' 
*
         IF (QSTEP .NE. 0.D0) 
     &      CALL SAVEN(IEND,NSOLUTE,AFACE,AFACEN,AREA,AREAN,GAMMA,
     &                 GAMMAN,Q,QLATIN,QLATINN,QN,CLATIN,CLATINN,AN,CN,
     &                 A,C,BTERMS,BTERMSN,TIMEB,TGROUP,BN,TWOPLUS,ALPHA,
     &                 IGROUP)
*
*        test to see if we are near an observed datapoint
*
         CALL SETPV(FOUND,IOBS,XM,TIME,TSTEP,IDATA,PINDEX,CONC,WT,PV)

 20   CONTINUE

      RETURN
      END


