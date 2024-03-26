************************************************************************
*
*     OTIS-P: Parameter Estimation for the OTIS Solute Transport Model
*     ----------------------------------------------------------------
*     
*     OTIS-P, a modified version of OTIS, couples the solution of the
*     governing equations with a nonlinear regression package.  OTIS-P
*     determines an 'optimal' set of parameter estimates that minimize
*     the quared differences between the simulated and measured
*     concentrations, thereby automating the parameter estimation
*     process.
*
*     The OTIS-P software, data, and documentation are made available by
*     the U.S. Geological Survey (USGS) to be used in the public
*     interest and the advancement of science. You may, without any fee
*     or cost, use, copy, modify, or distribute this software, and any
*     derivative works thereof, and its supporting documentation,
*     SUBJECT TO the USGS software User's Rights Notice
*     (http://water.usgs.gov/software/software_notice.html)
*
************************************************************************
*
*     Version: MOD40         Feb. 1998
*
************************************************************************
*
*     Reference
*     ---------
*     Runkel, R.L., 1998, One dimensional transport with inflow and
*       storage (OTIS): A solute transport model for streams and rivers:
*       U.S. Geological Survey Water-Resources Investigation Report
*       98-4018. xx p.
*
*     Homepage:  http://webserver.cr.usgs.gov/otis/home.html
*     ---------
*
*     Author
*     ------
*     R.L. Runkel
*     U.S. Geological Survey
*     Mail Stop 415
*     Denver Federal Center
*     Lakewood, CO 80225
*     Internet: runkel@usgs.gov
*
************************************************************************
*
*                            SEGMENTATION
*
************************************************************************
*
*
*
*     |<--- Hi-1 --->|<---- Hi ---->|<--- Hi+1 --->|
*     ______________________________________________
*     |              |              |              |
*     |     Ci-1     |      Ci      |     Ci+1     |
*     |     Qi-1     |      Qi      |     Qi+1     | 
*     |     Ai-1     |      Ai      |     Ai+1     |
*     |              |              |              |              
*     ----------------------------------------------
*                  DFACEi         DFACEi+1
*                  AFACEi         AFACEi+1
*
*     where:
*
*           A = AREA
*           C = CONC, CONC2, or SORB
*           H = DELTAX
*
*     as defined below.
*
*
************************************************************************
*
*                      DICTIONARY - INPUT VARIABLES
*
************************************************************************
*
*     Required Input Data
*     -------------------
*     TITLE      simulation title
*     PRTOPT     print option
*     TSTEP      time step interval [hr]
*     PSTEP      print step interval [hr]
*     TSTART     starting time [hr]
*     TFINAL     final time [hr]
*     XSTART     starting distance at upstream boundary [L]
*     DSBOUND    downstream boundary condition (flux) [mass/L^2-s]
*     NREACH     number of reaches
*
*     Required Reach Parameters (One value for each reach)
*     ----------------------------------------------------
*     NSEG       number of segments
*     RCHLEN     length of reach [L]
*     DISP       dispersion coefficient [L^2/s]
*     AREA2      storage zone cross-sectional area [L^2]
*     ALPHA      exchange coefficient [/s]
*
*     Required Print Information
*     --------------------------
*     NPRINT     number of print locations
*     IOPT       interpolation option
*     PRTLOC     distance at which results are printed [L]
*
*     Required Chemical Information
*     -----------------------------
*     NSOLUTE    number of solutes to simulate
*     IDECAY     decay flag (=0 no decay; =1 first-order decay)
*     ISORB      sorption flag (=0 no sorption; =1 Kd sorption)
*
*     First-Order Decay Rate Information (Required for IDECAY=1)
*     ----------------------------------------------------------
*     LAMBDA     decay coefficient for the main channel [/s]
*     LAMBDA2    decay coefficient for the storage zone [/s]
*
*     Kd Sorption Information (Required for ISORB=1)
*     ----------------------------------------------
*     LAMHAT     sorption rate coefficient for the main channel [/sec]
*     LAMHAT2    sorption rate coefficient for the storage zone [/sec]
*     RHO        mass of accessibile sediment/volume water [mass/L^3]
*     KD         distribution coefficient [L^3/mass]
*     CSBACK     background storage zone solute concentration [mass/L^3]
*
*     Required Chemical and Upstream Boundary Condition Information
*     -------------------------------------------------------------
*     NBOUND     number of different upstream boundary conditions
*     IBOUND     boundary condition option
*     USTIME     time at which upstream boundary condition goes
*                into effect [hr]
*     USBC       upstream boundary condition
*
*     Required Flow and Area Information
*     ----------------------------------
*     QSTEP      time interval between changes in flow (0=steady flow)
*     QSTART     volumetric flowrate at upstream boundary [L^3/s]
*     QLATIN     lateral inflow rate [L^3/s-L]
*     QLATOUT    lateral outflow rate [L^3/s-L]
*     AREA       main channel cross-sectional area [L^2]
*     CLATIN     concentration of lateral inflow [mass/L^3]
*
*     Information for Unsteady Flow Regimes (QSTEP > 0)
*     -------------------------------------------------
*     NFLOW      number of locations at which Q and AREA are specified
*     FLOWLOC    distance at which Q and AREA are specified [L]
*     QVALUE     flowrates at specified distances
*     QINVAL     lateral inflow rate at specified distances
*     AVALUE     areas at specified distances
*     CLVAL      concentration of lateral inflows at specified distances
*
*     Required STARPAC Parameters and Data
*     ------------------------------------
*     IWEIGHT    option to revise weights (=0 no revision; =1 revision)
*     IVAPRX     specifies approx. for the variance-covariance matrix
*     MIT        maximum number of iterations allowed
*     NPRT       print option
*     DELTA      max. scaled change allowed in parameters, 1st iteration
*     STOPP      stopping value for scaled convergence test
*     STOPSS     stopping value for sum of squares convergence test
*     IFIXED     vector indicating which parameters are to be estimated
*     SCALE      scale or typical size of each parameter
*     N          number of observations
*     XM         times/distances of the observations
*     Y          observed concentrations
*
************************************************************************
*
*                      DICTIONARY - PROGRAM VARIABLES
*
************************************************************************
*
*     State Variables
*     ---------------
*     CONC       concentration in main channel [mass/L^3]
*     CONC2      concentration in storage zone [mass/L^3]
*     SORB       streambed sediment concentration [mass/mass]
*
*     System Definition and Hydrology
*     -------------------------------
*     AFACE      cross sectional area @ interface of segments I,I+1
*     DFACE      dispersion coefficient @ interface of segments I,I+1
*     DELTAX     segment length [L]
*     IMAX       number of segments
*     LASTSEG    last segment in each reach
*     DIST       distance corresponding to segment centroid
*     Q          volumetric flowrate [L^3/s]
*     QINDEX     flow location used for interpolation (when QSTEP > 0)
*     QWT        weight used to interpolate between QINDEX and QINDEX+1
*     DSDIST     distance to the nearest downstream flow location/DELTAX
*     USDIST     distance to the nearest upstream flow location / DELTAX 
*
*     Matrix Solution
*     ---------------
*     A          upper diagonal of the tridiagonal matrix
*     B          main diagonal of the tridiagonal matrix
*     C          lower diagonal of the tridiagonal matrix
*     D          constant vector
*     AWORK      work vector corresponding to A
*     BWORK      work vector corresponding to B
*
*     Parameter Groups - Time invariant (H = DELTAX)
*     ----------------------------------------------
*     HPLUSF     H(i) + H(i+1)
*     HPLUSB     H(i) + H(i-1)
*     HMULTF     H(i) [ H(i) + H(i+1) ]
*     HMULTB     H(i) [ H(i) + H(i-1) ]
*     HDIV       H(i) / [ H(i) + H(i+1) ]
*     HDIVF      H(i+1) / [ H(i) + H(i+1) ]
*     HADV       0.5/H(i) { H(i+1)/[H(i+1)+H(i)] - H(i-1)/[H(i-1)+H(i)]}
*     LAM2DT     storage zone decay coefficient times the timestep
*     LHATDT     main channel sorption rate times the timestep
*     LHAT2DT    storage zone sorption rate times the timestep
*     RHOLAM     mass of access. sediment times the m.c. sorption rate
*     SGROUP     sorption parameter group
*     SGROUP2    sorption parameter group
*
*     Parameter Groups - Time invariant given steady flow
*     ---------------------------------------------------
*     BN         group related to the main diagonal vector, B
*     BTERMS     group related to the main diagonal vector, B
*     GAMMA      group introduced by storage zone equation
*     IGROUP     inflow group 
*     TGROUP     transient storage group
*     TWOPLUS    2 + .....
*
*     Boundary Conditions & Flow Changes
*     ----------------------------------
*     BCSTOP     time at which the boundary conditions change
*     JBOUND     index of the current boundary condition
*     STOPTYPE   indicates whether the next TSTOP is due to a change
*                in the boundary conditions or the flow variables.
*     TSTOP      time at which the boundary condition or flow variables
*                change
*     USCONC     concentration at the upstream boundary
*     QSTOP      time at which the flow variables change
*
*     Program Variables - Misc.
*     -------------------------
*     TIMEB      inverse of the time step [/sec]
*     TIME       time [hr]
*     CHEM       type of chemistry considered (reactive or conservative)
*     IPRINT     number of iterations between printing of results
*     PINDEX     segments for which results are printed
*     WT         weight used to interpolate between PINDEX and PINDEX+1
*
*     Program Variables - time level 'N'
*     ----------------------------------
*     The following variables contain values of associated with the
*     previous timestep, time level 'N'.  Their counterparts at time
*     level 'N+1' are defined above:
*     
*     AFACEN,AN,AREAN,BTERMSN,CLATINN,CN,GAMMAN,QLATINN,QN,USCONCN
*
*     i.e. CLATINN is the lateral inflow concentration at time N, while
*     CLATIN is corresponding concentration at time N+1.
*
*     Program Variables - Starpac
*     ---------------------------
*     PAR        parameter estimates
*     RES        residuals
*     DSTAK      workspace vector
*     OBSWT      weights corresponding to each observation
*     STP        relative step sizes used to approximate derivatives
*     ICOUNT     number of reaches for which parameter estimates apply
*     IDATA      number of reaches with data
*
************************************************************************
*
*                      INCLUDE FILES
*
*************************************************************************
*   
*
*     Maximum Dimensions (set in fmodules.inc & fmodules2.inc)
*     --------------------------------------------------------
*     MAXSEG     maximum number of segments
*     MAXBOUND   maximum number of upstream boundary conditions
*     MAXREACH   maximum number of reaches
*     MAXPRINT   maximum number of print locations
*     MAXSOLUTE  maximum number of solutes
*     MAXFLOWLOC maximum number of flow locations (unsteady flow)
*     LDSTAK     length of workspace vector, DSTAK
*     MAXOBS     maximum number of observations
*     MAXPAR     the number of model parameters
*
*     Logical Devices (as defined in lda.inc)
*     ---------------------------------------
*     LDCTRL     input control information (I/O filenames)
*     LDPARAM    input file for chemical and physical parameters
*     LDFLOW     input file for flow, lateral inflow, and areas
*     LDECHO     output data and time, echo input parameters
*     LDFILES    output files, one file per solute
*     LDSORB     sorption output files (ISORB=1), one file per solute
*     LDDATA     input file for observed data, initial parameter
*     LDSTAR     input file for STARPAC parameters
*     LDOUT      output file for parameter estimates
*     LDOUT2     STARPAC output file
*
************************************************************************

      PROGRAM MAIN
*
*     dimensional parameters and common blocks
*
      INCLUDE 'fmodules.inc'
      INCLUDE 'fmodules2.inc'
      INCLUDE 'otis.cmn'
*
*     STARPAC Basic Declaration Block, Declarations for NLSWC
*
      INTEGER*4 MIT,IVAPRX,NPRT,NPAR
      DOUBLE PRECISION DELTA,STOPSS,STOPP,SCALE(MAXPAR)
      INTEGER*4 ICOUNT
*
*     other variables passed between MAININIT, REACHEST & MAINRUN, but
*     not needed in OTISRUN.
*
      INTEGER*4 NPAREST,IMAX,ISORB,IPRINT,PRTOPT,NPRINT
      DOUBLE PRECISION TFINAL,DIST(MAXSEG),PRTLOC(MAXPRINT)
      CHARACTER*10 LABEL(MAXPAR,2)
*
*     initialize, read system definition
*
      ICOUNT = 0
      IDATA = 0
      NPAR = MAXPAR

      CALL MAININIT(MIT,IVAPRX,NPRT,DELTA,STOPSS,STOPP,SCALE,NPAR,
     &              NPAREST,LABEL,DIST,IMAX,ISORB,IPRINT,PRTOPT,NPRINT,
     &              PRTLOC,NSOLUTE,NFLOW,JBOUND,IBOUND,NBOUND,NREACH,
     &              IWEIGHT,PINDEX,QINDEX,IFIXED,LASTSEG,DSBOUND,TSTART,
     &              TFINAL,TSTEP,QSTEP,TIMEB,TSTOP,QSTOP,BCSTOP,QSTART,
     &              AREA2,DELTAX,Q,USTIME,AREA,ALPHA,QLATIN,WT,QVALUE,
     &              AVALUE,QWT,DSDIST,USDIST,DFACE,HPLUSF,HPLUSB,HMULTF,
     &              HMULTB,HDIV,HDIVF,HADV,GAMMA,AFACE,A,C,QINVAL,
     &              USCONCN,USCONC,CLATIN,CONC,CONC2,LAMBDA,CLVAL,
     &              LAM2DT,BTERMS,AWORK,BWORK,USBC,SORB,LHAT2DT,KD,CHEM,
     &              STOPTYPE,TWOPLUS,SGROUP2,SGROUP,LHATDT,BN,TGROUP,
     &              IGROUP,LAMBDA2,CSBACK,LAMHAT2,RHOLAM,LAMHAT)
*
*     estimate parameters for each reach
*
      CALL REACHEST(MIT,IVAPRX,NPRT,DELTA,STOPSS,STOPP,SCALE,ICOUNT,
     &              NPAR,NPAREST,LABEL,DIST,IMAX,NREACH,JREACH,ISTART,
     &              IEND,IDATA,IFIXED,INDEXPV,LASTSEG,TSTART,TEND,TSTEP,
     &              AREA2,DELTAX,AREA,ALPHA,DFACE,WTPV,LAMBDA,KD,CHEM,
     &              LAMBDA2,LAMHAT2,RHOLAM,LAMHAT)
*
*     compute and output results based on final parameter set
*
      CALL MAINRUN(IMAX,NPRINT,PINDEX,WT,IPRINT,AREA2,DELTAX,Q,USTIME,
     &             USCONC,USCONCN,AREA,ALPHA,DSBOUND,QLATIN,CLATIN,
     &             TSTART,TFINAL,TSTEP,CONC,CONC2,QSTEP,PRTOPT,NSOLUTE,
     &             CHEM,NFLOW,QINDEX,QWT,STOPTYPE,JBOUND,TIMEB,TSTOP,
     &             QSTOP,DFACE,HPLUSF,HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,
     &             HADV,GAMMA,BTERMS,AFACE,A,C,AWORK,BWORK,LAM2DT,
     &             DSDIST,USDIST,QINVAL,CLVAL,AVALUE,QVALUE,IBOUND,USBC,
     &             NBOUND,BCSTOP,SORB,LHAT2DT,KD,ISORB,LAMBDA,QSTART,
     &             DIST,PRTLOC,CLATINN,QN,AREAN,QLATINN,GAMMAN,AFACEN,
     &             AN,CN,BTERMSN,TWOPLUS,SGROUP2,SGROUP,LHATDT,BN,
     &             TGROUP,IGROUP,LAMBDA2,CSBACK,LAMHAT2,RHOLAM,LAMHAT)
*
*     close files
*
      CALL CLOSEF(NSOLUTE,ISORB)

      END





