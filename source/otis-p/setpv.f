************************************************************************
*
*     Subroutine SETPV             Called by: DYNAMIC2
*
*     test to see if we are near an observed data point.  The goal is to
*     determine the simulated value that corresponds to an observation.
*     Because the time of observation may not correspond directly to
*     the simulation timestep, interpolation is required.  Here we find
*     one of the two simulation time points that bracket the observation
*     time.  Once both points are determined, we interpolate to get a
*     simulated concentration at the time equal to the time of
*     observation
*
************************************************************************
      SUBROUTINE SETPV(FOUND,IOBS,XM,TIME,TSTEP,IDATA,PINDEX,CONC,WT,PV)
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 IOBS,IDATA,PINDEX(*)
      DOUBLE PRECISION PV(*),XM(*)
      DOUBLE PRECISION TIME,TSTEP,WT(*),CONC(MAXSEG,*)
      LOGICAL FOUND
*
*     local variables
*
      DOUBLE PRECISION FIRSTPT,FIRSTTIME,SECONDPT
      INTEGER*4 I
      SAVE FIRSTPT,FIRSTTIME
*
*     If the fist simulation point has not been found (the timestep
*     immediatly preceeding the data point) then
*        If the observation point lies between the current time and
*        the future time then
*            set the flag indicating the first point has been found
*            save the concentration and time corresponding to the 1st pt.
*        endif
*     else (the first point has been found and this is the second)
*        perform the temporal interpolation, set PV, re-initialize
*        and goto 10 (goto 10, because the current time could be the
*        first point for the next observation.)
*     endif
*
 10   IF (.NOT. FOUND) THEN
         IF (XM(IOBS) .GE. TIME .AND. XM(IOBS) .LT. TIME+TSTEP) THEN
            FOUND = .TRUE.
            FIRSTTIME = TIME
            I = PINDEX(IDATA)
            FIRSTPT = CONC(I,1) + WT(IDATA) * (CONC(I+1,1) - CONC(I,1))
         ENDIF
      ELSE
         I = PINDEX(IDATA)
         SECONDPT = CONC(I,1) + WT(IDATA) * (CONC(I+1,1)-CONC(I,1))
         PV(IOBS) = FIRSTPT + (SECONDPT - FIRSTPT) 
     &                      * (XM(IOBS)-FIRSTTIME) / (TIME - FIRSTTIME)
         IOBS = IOBS + 1
         FOUND = .FALSE.
         GOTO 10
      ENDIF

      RETURN
      END
