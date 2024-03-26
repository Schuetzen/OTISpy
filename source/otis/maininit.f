************************************************************************
*
*     Subroutine MAININIT            Called by: MAIN program
*
*     Obtain input parameters, initialize variables, and preprocess.
*     If the steady-state option has been invoked, output results
*
************************************************************************
      SUBROUTINE MAININIT(IMAX,NPRINT,PINDEX,WT,IPRINT,DELTAX,Q,USTIME,
     &                    USCONC,USCONCN,AREA,DSBOUND,QLATIN,CLATIN,
     &                    TSTART,TFINAL,TSTEP,CONC,CONC2,QSTEP,PRTOPT,
     &                    NSOLUTE,LAMBDA,CHEM,NFLOW,QINDEX,QWT,STOPTYPE,
     &                    JBOUND,TIMEB,TSTOP,QSTOP,DFACE,HPLUSF,HPLUSB,
     &                    HMULTF,HMULTB,HDIV,HDIVF,HADV,GAMMA,BTERMS,
     &                    AFACE,A,C,AWORK,BWORK,LAM2DT,DSDIST,USDIST,
     &                    QINVAL,CLVAL,AVALUE,QVALUE,IBOUND,USBC,NBOUND,
     &                    BCSTOP,SGROUP2,SGROUP,LHATDT,SORB,LHAT2DT,KD,
     &                    ISORB,CLATINN,QN,AREAN,QLATINN,GAMMAN,AFACEN,
     &                    AN,CN,BTERMSN,TWOPLUS,AREA2,ALPHA,BN,TGROUP,
     &                    IGROUP)
      INCLUDE 'fmodules.inc'
      INCLUDE 'lda.inc'
*
*     subroutine arguments
* 
      INTEGER*4 IMAX,NPRINT,IPRINT,NSOLUTE,PRTOPT,NFLOW,JBOUND,IBOUND,
     &          NBOUND,ISORB
      INTEGER*4 PINDEX(*),QINDEX(*)
      DOUBLE PRECISION DSBOUND,TSTART,TFINAL,TSTEP,QSTEP,TIMEB,TSTOP,
     &                 QSTOP,BCSTOP
      DOUBLE PRECISION DELTAX(*),Q(*),USTIME(*),AREA(*),QLATIN(*),WT(*),
     &                 QWT(*),DSDIST(*),USDIST(*),QINVAL(*),DFACE(*),
     &                 HPLUSF(*),HPLUSB(*),HMULTF(*),HMULTB(*),HDIV(*),
     &                 HDIVF(*),HADV(*),GAMMA(*),AFACE(*),A(*),C(*),
     &                 AVALUE(*),QVALUE(*),USCONCN(*),QN(*),AREAN(*),
     &                 QLATINN(*),AFACEN(*),GAMMAN(*),AN(*),CN(*),
     &                 AREA2(*),ALPHA(*)
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
      CHARACTER*(*) CHEM,STOPTYPE
*
*     local variables
*
      INTEGER*4 IDECAY
      DOUBLE PRECISION XSTART,QSTART
      DOUBLE PRECISION PRTLOC(MAXPRINT),DISP(MAXSEG),
     &                 FLOWLOC(MAXFLOWLOC),QLATOUT(MAXSEG),DIST(MAXSEG)
      DOUBLE PRECISION LAMBDA2(MAXSEG,MAXSOLUTE),
     &                 LAMHAT(MAXSEG,MAXSOLUTE),
     &                 LAMHAT2(MAXSEG,MAXSOLUTE),
     &                 RHOLAM(MAXSEG,MAXSOLUTE),
     &                 CSBACK(MAXSEG,MAXSOLUTE)
*
*     Initialize Logical Device Assignments (LDAs)
*
      CALL LDAINIT
*
*     Output heading (date/time) information
*
      CALL HEADER('OTIS SOLUTE TRANSPORT MODEL            ')
*
*     open input files
*
      CALL OPENFILE
*
*     initialize chemical parameters
*
      CALL INIT(LAMBDA,LAMBDA2,LAMHAT,LAMHAT2,RHOLAM,KD,CSBACK)
*
*     input physical and chemical parameters
*
      CALL INPUT(TSTEP,TSTART,TFINAL,IPRINT,XSTART,DSBOUND,PRTOPT,IMAX,
     &           DISP,AREA2,ALPHA,DELTAX,NSOLUTE,LAMBDA,LAMBDA2,NPRINT,
     &           PRTLOC,NBOUND,USTIME,USCONC,USCONCN,QSTEP,QSTART,
     &           QLATIN,QLATOUT,CLATIN,AREA,FLOWLOC,QVALUE,AVALUE,NFLOW,
     &           QINVAL,CLVAL,PINDEX,WT,IBOUND,USBC,CSBACK,RHOLAM,
     &           LAMHAT,LAMHAT2,KD,ISORB,IDECAY,DIST)
*
*     determine the type of simulation to conduct so that the proper
*     preprocessing and finite difference routines are called.
*
      CALL SETTYPE(IDECAY,ISORB,TSTEP,CHEM)
*
*     Initialize flows for the steady-flow case (QSTEP=0), or
*     initialize flows and areas for the case of unsteady flow.
*
      IF (QSTEP .EQ. 0.D0) THEN
         CALL FLOWINIT(IMAX,DELTAX,QSTART,Q,QLATIN,QLATOUT)
      ELSE
         CALL QWEIGHTS(IMAX,DELTAX,XSTART,QINDEX,QWT,FLOWLOC,NFLOW,
     &                 DSDIST,USDIST,DIST)
         CALL QAINIT(IMAX,Q,AREA,QVALUE,AVALUE,QWT,QINDEX,QINVAL,CLVAL,
     &               QLATIN,CLATIN,DSDIST,USDIST,NSOLUTE)
      ENDIF
*
*     pre-process parameter groups and initialize variables
*
      CALL PREPROC(IMAX,NBOUND,AREA2,DELTAX,Q,USTIME,AREA,ALPHA,DISP,
     &             QLATIN,TSTART,TSTEP,QSTEP,NSOLUTE,LAMBDA,LAMBDA2,
     &             CHEM,STOPTYPE,JBOUND,TIMEB,TSTOP,QSTOP,DFACE,HPLUSF,
     &             HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,HADV,GAMMA,BTERMS,
     &             AFACE,A,C,AWORK,BWORK,LAM2DT,BCSTOP,IBOUND,RHOLAM,KD,
     &             LAMHAT,LAMHAT2,LHAT2DT,TWOPLUS,LHATDT,SGROUP,SGROUP2,
     &             CSBACK,BN,TGROUP,IGROUP,CLATIN)
*
*     save initial values of flow variables and preprocess
*
      IF (CHEM .NE. 'Steady-State')
     &   CALL SAVEN(IMAX,NSOLUTE,AFACE,AFACEN,AREA,AREAN,GAMMA,GAMMAN,Q,
     &              QLATIN,QLATINN,QN,CLATIN,CLATINN,AN,CN,A,C,BTERMS,
     &              BTERMSN,TIMEB,TGROUP,BN,TWOPLUS,ALPHA,IGROUP)
*
*     Initialize concentrations
*
      CALL STEADY(IMAX,AREA2,DELTAX,Q,USCONC,AREA,ALPHA,DSBOUND,QLATIN,
     &            CLATIN,CONC,CONC2,NSOLUTE,LAMBDA,LAMBDA2,DFACE,HPLUSF,
     &            HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,HADV,LAMHAT2,CSBACK,
     &            KD,SORB)
*
*     Open solute and sorption output files
*
      CALL OPENFILE2(NSOLUTE,ISORB)
*
*     Output Initial conditions
*
      CALL OUTINIT(NPRINT,PINDEX,PRTLOC,Q,CONC,CONC2,TSTART,WT,NSOLUTE,
     &             PRTOPT,CHEM,ISORB,SORB)
*
*     For the case of steady-state, output results
*
      IF (CHEM .EQ. 'Steady-State')
     &   CALL OUTPUTSS(CONC,CONC2,PRTOPT,ISORB,SORB,NSOLUTE,IMAX,DIST)
*
*     close input files 
*
      CLOSE (UNIT=LDCTRL)
      CLOSE (UNIT=LDPARAM)

      RETURN
      END
