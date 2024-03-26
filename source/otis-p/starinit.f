************************************************************************
*
*     Subroutine STARINIT              Called by: MAININIT
*
*     input STARPAC parameters
*
************************************************************************
      SUBROUTINE STARINIT(IWEIGHT,MIT,IVAPRX,NPRT,IFIXED,DELTA,STOPSS,
     &                    STOPP,SCALE,NPAR,NPAREST,LABEL)
      INCLUDE 'fmodules.inc'
      INCLUDE 'fmodules2.inc'
      INCLUDE 'lda.inc'
*
*     subroutine arguments
* 
      INTEGER*4 MIT,IWEIGHT,IVAPRX,NPRT,NPAR,NPAREST,IFIXED(*)
      DOUBLE PRECISION DELTA,STOPSS,STOPP,SCALE(*)
      CHARACTER*(*) LABEL(MAXPAR,2)
*
*     local variables
*
      INTEGER*4 I
      CHARACTER*9 STATUS(MAXPAR)
      CHARACTER*10 PARLBL(MAXPAR,2)
      CHARACTER*500 BUFFER
*
*     initialize parameter labels
*
      DATA PARLBL /'Dispersion','  X-sect. ',' Storage  ',' Storage  ',
     &             'M. Channel',' S. Zone  ','Accessible',' Distrib. ',
     &             'M. Channel',' S. Zone  ','  Coeff.  ','   Area   ',
     &             'Zone Area ','   Rate   ','Decay Rate','Decay Rate',
     &             ' Sediment ','  Coeff.  ','Sorp. Rate','Sorp. Rate'/
*
*     input format statements
*
 1000 FORMAT(I5)
 1020 FORMAT(F13.0)
 1040 FORMAT(I5,F13.0)
*
*     Output Format Statements
*
 2000 FORMAT(//,10X,'STARPAC Input Parameters',/,10X,24('-'))
 2020 FORMAT(10X,'Weight Revision Option                 :   ',I5)
 2040 FORMAT(10X,'Variance/Covariance Option, IVAPRX     :   ',I5)
 2060 FORMAT(10X,'Maximum Number of Iterations, MIT      :   ',I5)
 2080 FORMAT(10X,'STARPAC Print Option, NPRT             :   ',I5)
 2100 FORMAT(10X,'Maximum Scaled Change, DELTA           : ',1PE13.5)
 2120 FORMAT(10X,'Convergence Test Stopping Value, STOPP : ',1PE13.5)
 2140 FORMAT(10X,'Convergence Test Stopping Value, STOPSS: ',1PE13.5)
 2160 FORMAT(1P,//,10X,'Model Parameters',//,10X,'Parameter',32X,
     &       'Status',13X,'SCALE',/,10X,68('-'),/,10X,
     &       'Dispersion Coefficient',19X,A9,5X,E13.5,/,10X,
     &       'Main Channel Cross-Sectional Area',8X,A9,5X,E13.5,/,10X,
     &       'Storage Zone Area',24X,A9,5X,E13.5,/,10X,
     &       'Storage Zone Exchange Coefficient',8X,A9,5X,E13.5,/,10X,
     &       'Main Channel First-Order Decay Rate',6X,A9,5X,E13.5,/,10X,
     &       'Storage Zone First-Order Decay Rate',6X,A9,5X,E13.5,/,10X,
     &       'Accessible Sediment/Volume Water',9X,A9,5X,E13.5,/,10X,
     &       'Distribution Coefficient',17X,A9,5X,E13.5,/,10X,
     &       'Main Channel Sorption Rate Coefficient',3X,A9,5X,E13.5,/,
     &       10X,'Storage Zone Sorption Rate Coefficient',3X,A9,5X,
     &       E13.5)
*
*     read the option for revising the weights (IWEIGHT), the indicator
*     for computation of the variance/covariance matrix (IVAPRX), the
*     maximum number of iterations (MIT), and the STARPAC parameter
*     controlling printed output (NPRT).
*
      WRITE (LDECHO,2000)
      CALL GETLINE(LDSTAR,BUFFER)
      READ (BUFFER,1000) IWEIGHT
      WRITE (LDECHO,2020) IWEIGHT
      IF (IWEIGHT .NE. 0 .AND. IWEIGHT .NE. 1) CALL ERROR6(1,IWEIGHT)
      CALL GETLINE(LDSTAR,BUFFER)
      READ (BUFFER,1000) IVAPRX
      WRITE (LDECHO,2040) IVAPRX
      CALL GETLINE(LDSTAR,BUFFER)
      READ (BUFFER,1000) MIT
      WRITE (LDECHO,2060) MIT
      CALL GETLINE(LDSTAR,BUFFER)
      READ (BUFFER,1000) NPRT
      WRITE (LDECHO,2080) NPRT
*
*     read the maximum scaled change allowed in the parameters on the
*     first iteration (DELTA), and the stopping values for the
*     convergence tests (STOPP & STOPSS).
*
      CALL GETLINE(LDSTAR,BUFFER)
      READ (BUFFER,1020) DELTA
      WRITE (LDECHO,2100) DELTA
      CALL GETLINE(LDSTAR,BUFFER)
      READ (BUFFER,1020) STOPP
      WRITE (LDECHO,2120) STOPP
      CALL GETLINE(LDSTAR,BUFFER)
      READ (BUFFER,1020) STOPSS
      WRITE (LDECHO,2140) STOPSS
*
*     For each of the ten model parameters, read IFIXED and SCALE
*     (IFIXED specifies whether or not a given parameter is to be
*     estimated. SCALE specifies the typical size of a given parameter.)
*     If a given parameter is to be estimated, increment the counter
*     denoting the number of estimated parameters (NPAREST) and
*     assign a LABEL for the parameter estimate output file.
*
      NPAREST = 0

      DO 10 I=1,NPAR
         CALL GETLINE(LDSTAR,BUFFER)
         READ (BUFFER,1040) IFIXED(I),SCALE(I)
         IF (IFIXED(I) .EQ. 0) THEN
            STATUS(I) = 'Estimated'
            NPAREST = NPAREST + 1
            LABEL(NPAREST,1) = PARLBL(I,1)
            LABEL(NPAREST,2) = PARLBL(I,2)
         ELSE
            STATUS(I) = 'Fixed'
         ENDIF
 10   CONTINUE

      WRITE (LDECHO,2160) (STATUS(I),SCALE(I),I=1,NPAR)

      RETURN
      END
