************************************************************************
*
*     Subroutine OTISRUN               Called by: NLSWC STPLS
*
*     determine the predicted values of solute concentration based on
*     current parameter values.
*
************************************************************************
      SUBROUTINE OTISRUN(PAR,NPAR,XM,N,M,IXM,PV,OBSWT)
*
*     maximum dimensions, logical devices & common block
*
      INCLUDE 'fmodules.inc'
      INCLUDE 'fmodules2.inc'
      INCLUDE 'lda.inc'
      INCLUDE 'otis.cmn'
*
*     subroutine arguments
*
      INTEGER NPAR,N,M,IXM
      DOUBLE PRECISION PAR(N),XM(IXM),PV(N),OBSWT(N)
*
*     local variables and function declaration
*
      INTEGER*4 I,J
      DOUBLE PRECISION PARVAL(MAXPAR)
      LOGICAL OKPARAM
*
*     format statement
*
 100  FORMAT(1P,6(E13.5,1X))
*
*     check to make sure all of the parameters are greater than their
*     specified minimum values.  If improper parameter values are
*     proposed, return predicted concentrations of zero to discourage
*     their use.
*
      IF (.NOT. OKPARAM(PAR,PV,N)) RETURN
*
*     prepare to compute the predicted concentrations
*
      CALL SETUP(ISTART,PAR,AREA2,DELTAX,AREA,ALPHA,DFACE,LASTSEG,
     &           JREACH,LAMBDA,LAMBDA2,NREACH,IFIXED,RHOLAM,KD,LAMHAT,
     &           LAMHAT2)
      IF (CHEM .NE. 'Steady-State') THEN
         IF (QSTEP .NE. 0)
     &      CALL SETUP2(QINVAL,CLVAL,QVALUE,AVALUE,NSOLUTE,NFLOW,IEND,Q,
     &                  AREA,QWT,QINDEX,QLATIN,CLATIN,DSDIST,USDIST)
         CALL SETUP3(IEND,NSOLUTE,JBOUND,TSTART,QSTEP,TIMEB,TSTOP,QSTOP,
     &               AREA2,DELTAX,Q,USTIME,AREA,ALPHA,QLATIN,DFACE,
     &               HPLUSF,HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,HADV,GAMMA,
     &               AFACE,A,C,USCONCN,USCONC,LAMBDA2,LAM2DT,BTERMS,
     &               AWORK,BWORK,STOPTYPE,LAMBDA,BCSTOP,IBOUND,NBOUND,
     &               QSTART,USBC,LAMHAT2,LHAT2DT,KD,TWOPLUS,LAMHAT,
     &               LHATDT,SGROUP,SGROUP2,RHOLAM,CSBACK,BN,TGROUP,
     &               IGROUP,CLATIN)
         CALL SAVEN(IEND,NSOLUTE,AFACE,AFACEN,AREA,AREAN,GAMMA,GAMMAN,Q,
     &              QLATIN,QLATINN,QN,CLATIN,CLATINN,AN,CN,A,C,BTERMS,
     &              BTERMSN,TIMEB,TGROUP,BN,TWOPLUS,ALPHA,IGROUP)
      ENDIF

      CALL STEADY(IEND,AREA2,DELTAX,Q,USCONC,AREA,ALPHA,DSBOUND,QLATIN,
     &            CLATIN,CONC,CONC2,NSOLUTE,LAMBDA,LAMBDA2,DFACE,HPLUSF,
     &            HPLUSB,HMULTF,HMULTB,HDIV,HDIVF,HADV,LAMHAT2,CSBACK,
     &            KD,SORB)
*
*     obtain the solution, thru the current reach (I=1,IEND).  In call
*     to DYNAMIC2, IMAX is replaced by IEND
*
      IF (CHEM .NE. 'Steady-State') THEN
         CALL DYNAMIC2(IEND,DELTAX,Q,USTIME,USCONC,USCONCN,AREA,DSBOUND,
     &                 QLATIN,CLATIN,TSTART,TSTEP,CONC,CONC2,QSTEP,
     &                 NSOLUTE,LAMBDA,QVALUE,AVALUE,QWT,QINDEX,NFLOW,
     &                 QINVAL,CLVAL,CHEM,DSDIST,USDIST,STOPTYPE,JBOUND,
     &                 TIMEB,TSTOP,QSTOP,DFACE,HPLUSF,HPLUSB,HMULTF,
     &                 HMULTB,HDIV,HDIVF,HADV,GAMMA,BTERMS,AFACE,A,C,
     &                 AWORK,BWORK,LAM2DT,IBOUND,USBC,NBOUND,BCSTOP,
     &                 SGROUP2,SGROUP,LHATDT,SORB,LHAT2DT,KD,CLATINN,QN,
     &                 AREAN,QLATINN,GAMMAN,AFACEN,AN,CN,BTERMSN,
     &                 TWOPLUS,AREA2,ALPHA,BN,TGROUP,IGROUP,TEND,XM,
     &                 IDATA,PINDEX,WT,PV)
      ELSEIF (CHEM .EQ. 'Steady-State') THEN
         CALL SETPV2(N,PV,CONC,INDEXPV,WTPV)
      ENDIF
*
*     At the user's request, revise the weights based on the predicted
*     values of concentration that have been returned from OTIS.
*
      IF (IWEIGHT .EQ. 1) THEN
         DO 10 I=1,N
            IF (PV(I)*PV(I) .NE. 0.0) OBSWT(I) = 1.0/(PV(I)*PV(I))
 10      CONTINUE 
      ENDIF
*
*     fill PARVAL w/ current values for the estimated parameters and
*     output.
*
      J = 0
      DO 20 I=1,NPAR
         IF (IFIXED(I) .EQ. 0) THEN
            J = J + 1
            PARVAL(J) = PAR(I)
         ENDIF
 20   CONTINUE
      WRITE (LDOUT,100) (PARVAL(I), I=1,J)

      RETURN
      END


