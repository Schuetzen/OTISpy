************************************************************************
*
*     Subroutine ERROR5               Called by: DATACHK, WEIGHTS2
*
*     print error messages related to parameter estimation
*
************************************************************************
      SUBROUTINE ERROR5(NUMBER,PARAM1,PARAM2)
*
*     dimensional parameters and logical devices
*
      INCLUDE 'fmodules.inc'
      INCLUDE 'lda.inc'

      INTEGER*4 NUMBER
      DOUBLE PRECISION PARAM1,PARAM2

 1000 FORMAT(//,2X,'Error:',//,2X,
     &'The time of the first observation must be greater than ',
     &'the simulation',/,2X,'start time plus the timestep.',//,2X,
     &'Time of First Observation           : ',D16.8,/,2X,
     &'Simulation Start Time plus Timestep : ',D16.8,//)
 1010 FORMAT(//,2X,'Error:',//,2X,
     &'Observations must be entered in order of ascending time',
     &//,2X,'Time of Observation I-1 : ',D16.8,/,2X,
     &'Time of Observation I   : ',D16.8,//)
 1020 FORMAT(//,2X,'Error:',//,2X,
     &'The time between successive observations must be ',
     &'greater than the timestep.',//,2X,
     &'Time of Observation I-1 : ',D16.8,/,2X,
     &'Time of Observation I   : ',D16.8,//)
 1030 FORMAT(//,2X,'Error:',//,2X,
     &'The distance specified exceeds the length of the stream.',
     &//,2X,'Distance Specified  : ',D16.8,
     & /,2X,'End of Stream       : ',D16.8,//)

      IF (NUMBER .EQ. 1) THEN
         WRITE(*,1000) PARAM1,PARAM2
         WRITE(LDECHO,1000) PARAM1,PARAM2
      ELSEIF (NUMBER .EQ. 2) THEN
         WRITE(*,1010) PARAM1,PARAM2
         WRITE(LDECHO,1010) PARAM1,PARAM2
      ELSEIF (NUMBER .EQ. 3) THEN
         WRITE(*,1020) PARAM1,PARAM2
         WRITE(LDECHO,1020) PARAM1,PARAM2
      ELSEIF (NUMBER .EQ. 4) THEN
         WRITE(*,1030) PARAM1,PARAM2
         WRITE(LDECHO,1030) PARAM1,PARAM2
      ENDIF

      WRITE(*,*)  '  **** Fatal Input Error, See file echo.out ****'
      STOP

      END
