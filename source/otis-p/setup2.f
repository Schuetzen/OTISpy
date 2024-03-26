************************************************************************
*
*     Subroutine SETUP2              Called by: MAINRUN, OTISRUN
*
*     for the case of unsteady flow, rewind the flow file, read initial
*     flow values and initialize the arrays.
*
************************************************************************
      SUBROUTINE SETUP2(QINVAL,CLVAL,QVALUE,AVALUE,NSOLUTE,NFLOW,IEND,Q,
     &                  AREA,QWT,QINDEX,QLATIN,CLATIN,DSDIST,USDIST)
      INCLUDE 'fmodules.inc'
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 IEND,NSOLUTE,NFLOW,QINDEX(*)
      DOUBLE PRECISION QINVAL(*),QVALUE(*),AVALUE(*),Q(*),AREA(*),
     &                 QWT(*),QLATIN(*),DSDIST(*),USDIST(*)
      DOUBLE PRECISION CLVAL(MAXFLOWLOC+1,*),CLATIN(MAXSEG,*)
*
*     local variables
*
      INTEGER*4 I
      CHARACTER*500 BUFFER
*
*     rewind the flow file
*
      REWIND(LDFLOW)
*
*     skip over the top of the flow file -- no need to reread QSTEP etc.
*
      DO 10 I=1,NFLOW+2
         CALL GETLINE(LDFLOW,BUFFER)
 10   CONTINUE
*
*     Read lateral inflow, lateral outflow, lateral inflow
*     concentrations, flow and area values for each flow location.
*
      CALL READQ(QINVAL,CLVAL,QVALUE,AVALUE,NSOLUTE,NFLOW)
      CALL QAINIT(IEND,Q,AREA,QVALUE,AVALUE,QWT,QINDEX,QINVAL,CLVAL,
     &            QLATIN,CLATIN,DSDIST,USDIST,NSOLUTE)

      RETURN
      END

