************************************************************************
*
*     Subroutine OPENFILE            Called by: MAININIT
*
*     Open input files and the output file for parameter estimates
*
************************************************************************
      SUBROUTINE OPENFILE
*
*     dimensional paramters and logical devices
*
      INCLUDE 'fmodules.inc'
      INCLUDE 'lda.inc'
*
*     local variables
*
      CHARACTER*40 FILE
      CHARACTER*500 BUFFER
*
*     format statements
*
 1000 FORMAT(A40)
 2000 FORMAT(///,10X,'I/O Files',/,10X,9('-'))
 2010 FORMAT(10X,'PARAMETER FILE     : ',A40)
 2020 FORMAT(10X,'FLOW FILE          : ',A40)
 2030 FORMAT(10X,'DATA FILE          : ',A40)
 2040 FORMAT(10X,'STARPAC INPUT FILE : ',A40)
 2050 FORMAT(10X,'PARAMETER ESTIMATES: ',A40)
 2060 FORMAT(10X,'STARPAC OUTPUT FILE: ',A40,///)
*
*     Open the control file
*
      CALL OPENIN(LDCTRL,'control.inp')
*
*     write header, determine the parameter file and open
*
      WRITE (LDECHO,2000)
      CALL GETLINE(LDCTRL,BUFFER)
      READ (BUFFER,1000) FILE
      WRITE (LDECHO,2010) FILE
      CALL OPENIN(LDPARAM,FILE)
*
*     Determine the flow file and open
*
      CALL GETLINE(LDCTRL,BUFFER)
      READ (BUFFER,1000) FILE
      WRITE (LDECHO,2020) FILE
      CALL OPENIN(LDFLOW,FILE)
*
*     Determine the observed data file and open
*
      CALL GETLINE(LDCTRL,BUFFER)
      READ (BUFFER,1000) FILE
      WRITE (LDECHO,2030) FILE
      CALL OPENIN(LDDATA,FILE)
*
*     Determine the STARPAC parameter file and open
*
      CALL GETLINE(LDCTRL,BUFFER)
      READ (BUFFER,1000) FILE
      WRITE (LDECHO,2040) FILE
      CALL OPENIN(LDSTAR,FILE)
*
*     open output file for parameter estimates
*
      CALL GETLINE(LDCTRL,BUFFER)
      READ (BUFFER,1000) FILE
      OPEN (UNIT=LDOUT,FILE=FILE)
      WRITE(LDECHO,2050) FILE
*
*     open the STARPAC output file
*
      CALL GETLINE(LDCTRL,BUFFER)
      READ (BUFFER,1000) FILE
      OPEN (UNIT=LDOUT2,FILE=FILE)
      WRITE(LDECHO,2060) FILE

      RETURN
      END
