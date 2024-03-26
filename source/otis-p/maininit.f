************************************************************************
*
*     Subroutine MAININIT              Called by: MAIN
*
*     open files, read OTIS & STARPAC parameter files, etc.
*
************************************************************************
      SUBROUTINE MAININIT(MIT,IVAPRX,NPRT,DELTA,STOPSS,STOPP,SCALE,NPAR,
     &                    NPAREST,LABEL,DIST,IMAX,ISORB,IPRINT,PRTOPT,
     &                    NPRINT,PRTLOC,NSOLUTE,NFLOW,JBOUND,IBOUND,
     &                    NBOUND,NREACH,IWEIGHT,PINDEX,QINDEX,IFIXED,
     &                    LASTSEG,DSBOUND,TSTART,TFINAL,TSTEP,QSTEP,
     &                    TIMEB,TSTOP,QSTOP,BCSTOP,QSTART,AREA2,DELTAX,
     &                    Q,USTIME,AREA,ALPHA,QLATIN,WT,QVALUE,AVALUE,
     &                    QWT,DSDIST,USDIST,DFACE,HPLUSF,HPLUSB,HMULTF,
     &                    HMULTB,HDIV,HDIVF,HADV,GAMMA,AFACE,A,C,QINVAL,
     &                    USCONCN,USCONC,CLATIN,CONC,CONC2,LAMBDA,CLVAL,
     &                    LAM2DT,BTERMS,AWORK,BWORK,USBC,SORB,LHAT2DT,
     &                    KD,CHEM,STOPTYPE,TWOPLUS,SGROUP2,SGROUP,
     &                    LHATDT,BN,TGROUP,IGROUP,LAMBDA2,CSBACK,
     &                    LAMHAT2,RHOLAM,LAMHAT)
      INCLUDE 'fmodules.inc'
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 NSOLUTE,NFLOW,JBOUND,IBOUND,NBOUND,NREACH,IWEIGHT
      INTEGER*4 PINDEX(*),QINDEX(*),IFIXED(*),LASTSEG(0:MAXREACH)
      DOUBLE PRECISION DSBOUND,TSTART,TFINAL,TSTEP,QSTEP,TIMEB,TSTOP,
     &                 QSTOP,BCSTOP,QSTART
      DOUBLE PRECISION AREA2(*),DELTAX(*),Q(*),USTIME(*),AREA(*),
     &                 ALPHA(*),QLATIN(*),WT(*),QVALUE(*),AVALUE(*),
     &                 QWT(*),DSDIST(*),USDIST(*),DFACE(*),HPLUSF(*),
     &                 HPLUSB(*),HMULTF(*),HMULTB(*),HDIV(*),HDIVF(*),
     &                 HADV(*),GAMMA(*),AFACE(*),A(*),C(*),QINVAL(*),
     &                 USCONCN(*)
      DOUBLE PRECISION USCONC(MAXBOUND,*),CLATIN(MAXSEG,*),
     &                 CONC(MAXSEG,*),CONC2(MAXSEG,*),
     &                 LAMBDA(MAXSEG,*),CLVAL(MAXFLOWLOC+1,*),
     &                 LAM2DT(MAXSEG,*),BTERMS(MAXSEG,*),
     &                 AWORK(MAXSEG,*),BWORK(MAXSEG,*),USBC(MAXBOUND,*),
     &                 SORB(MAXSEG,*),LHAT2DT(MAXSEG,*),KD(MAXSEG,*),
     &                 TWOPLUS(MAXSEG,*),SGROUP2(MAXSEG,*),
     &                 SGROUP(MAXSEG,*),LHATDT(MAXSEG,*),BN(MAXSEG,*),
     &                 TGROUP(MAXSEG,*),IGROUP(MAXSEG,*),
     &                 LAMBDA2(MAXSEG,*),LAMHAT2(MAXSEG,*),
     &                 CSBACK(MAXSEG,*),LAMHAT(MAXSEG,*),
     &                 RHOLAM(MAXSEG,*)
      CHARACTER*(*) CHEM,STOPTYPE
*
*     other subroutine arguments
*
      INTEGER*4 MIT,IVAPRX,NPRT,NPAR,NPAREST,IMAX,ISORB,IPRINT,PRTOPT,
     &          NPRINT
      DOUBLE PRECISION DELTA,STOPSS,STOPP,SCALE(*),DIST(*),PRTLOC(*)
      CHARACTER*(*) LABEL
*
*     local variables
*
      INTEGER*4 IDECAY
      DOUBLE PRECISION XSTART
      DOUBLE PRECISION DISP(MAXSEG),FLOWLOC(MAXFLOWLOC),QLATOUT(MAXSEG)
*
*     Initialize Logical Device Assignments (LDAs)
*
      CALL LDAINIT
*
*     Output heading (date/time) information
*
      CALL HEADER('OTIS SOLUTE TRANSPORT MODEL W/ PARAMETER ESTIMATION')
*
*     open the control, parameter, flow, observed data, and STARPAC
*     parameter files
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
     &           QINVAL,CLVAL,NREACH,LASTSEG,PINDEX,WT,IBOUND,USBC,
     &           CSBACK,RHOLAM,LAMHAT,LAMHAT2,KD,ISORB,IDECAY,DIST)
*
*     determine the type of simulation to conduct so that the proper
*     preprocessing and finite difference routines are called.
*
      CALL SETTYPE(IDECAY,ISORB,TSTEP,CHEM)
*
*     Initialize flows
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
*     Initialize concentrations
*
      CALL STEADY(IMAX,AREA2,DELTAX,Q,USCONC,AREA,ALPHA,DSBOUND,QLATIN,
     &            CLATIN,CONC,CONC2,NSOLUTE,LAMBDA,LAMBDA2,DFACE,HPLUSF,
     &            HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,HADV,LAMHAT2,CSBACK,
     &            KD,SORB)
*
*     read STARPAC parameters
*
      CALL STARINIT(IWEIGHT,MIT,IVAPRX,NPRT,IFIXED,DELTA,STOPSS,STOPP,
     &              SCALE,NPAR,NPAREST,LABEL)
*
*     close input files 
*
      CLOSE (UNIT=LDPARAM)
      CLOSE (UNIT=LDSTAR)

      RETURN
      END

