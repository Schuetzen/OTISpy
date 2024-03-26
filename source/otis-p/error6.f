************************************************************************
*
*     Subroutine ERROR6               Called by: STARINIT
*
*     print input error messages related to parameter estimation
*
************************************************************************
      SUBROUTINE ERROR6(NUMBER,PARAM)
*
*     dimensional parameters and logical devices
*
      INCLUDE 'fmodules.inc'
      INCLUDE 'lda.inc'

      INTEGER*4 NUMBER,PARAM

 1000 FORMAT(//,2X,'Error: The Weight Revision Option must equal 0 ',
     &       'or 1.',//,2X,'Weight Revision Option specified ',
     &       '(IWEIGHT) : ',I5,//)

      IF (NUMBER .EQ. 1) THEN
         WRITE(*,1000) PARAM
         WRITE(LDECHO,1000) PARAM
      ENDIF

      WRITE(*,*) '  **** Fatal Input Error, See file echo.out ****'
      STOP

      END


