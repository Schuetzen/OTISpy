************************************************************************
*
*     Subroutine ERROR4               Called by: REACHEST
*
*     print input error messages related to parameter estimation
*
************************************************************************
      SUBROUTINE ERROR4(NUMBER,PARAM1,PARAM2)

      INCLUDE 'fmodules.inc'
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
      INTEGER*4 NUMBER,PARAM1,PARAM2

 1000 FORMAT(//,2X,
     &'Error: The number of observations exceeds the maximum.',//,2X,
     &'Decrease the number of observations or increase the maximum.',/
     &,2X,'To increase the allowable number of observations, change'
     &,/,2X,'MAXOBS and recompile.',//,10X,
     &'Number of Observations Specified  : ',I5,/,10X,
     &'Maximum number allowed (MAXOBS)   : ',I5,//)

      IF (NUMBER .EQ. 1) THEN
         WRITE(*,1000) PARAM1,PARAM2
         WRITE(LDECHO,1000) PARAM1,PARAM2
      ENDIF

      WRITE(*,*)  '  **** Fatal Input Error, See file echo.out ****'
      STOP 

      END

