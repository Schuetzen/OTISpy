************************************************************************
*
*     Subroutine SETUP              Called by: REACHEST, OTISRUN
*
************************************************************************
      SUBROUTINE SETUP(ISTART,PAR,AREA2,DELTAX,AREA,ALPHA,DFACE,LASTSEG,
     &                 JREACH,LAMBDA,LAMBDA2,NREACH,IFIXED,RHOLAM,KD,
     &                 LAMHAT,LAMHAT2)
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 ISTART,JREACH,NREACH,IFIXED(*),LASTSEG(0:MAXREACH)
      DOUBLE PRECISION PAR(*)
      DOUBLE PRECISION AREA2(*),DELTAX(*),AREA(*),ALPHA(*),DFACE(*)
      DOUBLE PRECISION LAMBDA(MAXSEG,*),LAMBDA2(MAXSEG,*),
     &                 RHOLAM(MAXSEG,*),KD(MAXSEG,*),LAMHAT(MAXSEG,*),
     &                 LAMHAT2(MAXSEG,*)
*
*     local variable
*
      INTEGER*4 I
*
*     set the parameters for all of the segments w/i the reach.  If a
*     given parameter is fixed (IFIXED .NE. 0) allow it to remain at
*     the value initially specified in the parameter file.  (this allows
*     reaches w/o data to have fixed parameters that differ from the
*     parameters of the adjacent reaches)
*
      DO 10 I = ISTART,LASTSEG(JREACH)
         IF (IFIXED(1) .EQ. 0) DFACE(I) = PAR(1)
         IF (IFIXED(2) .EQ. 0) AREA(I) = PAR(2)
         IF (IFIXED(3) .EQ. 0) AREA2(I) = PAR(3)
         IF (IFIXED(4) .EQ. 0) ALPHA(I) = PAR(4)
         IF (IFIXED(5) .EQ. 0) LAMBDA(I,1) = PAR(5)
         IF (IFIXED(6) .EQ. 0) LAMBDA2(I,1) = PAR(6)
         IF (IFIXED(7) .EQ. 0) RHOLAM(I,1) = PAR(7)*PAR(9)
         IF (IFIXED(8) .EQ. 0) KD(I,1) = PAR(8)
         IF (IFIXED(9) .EQ. 0) LAMHAT(I,1) = PAR(9)
         IF (IFIXED(10) .EQ. 0) LAMHAT2(I,1) = PAR(10)
 10   CONTINUE
*
*     set the Dispersion coefficient at the interfaces of the upstream
*     and downstream ends of the current reach.  DFACE is the average of
*     the dispersion coefficient for the current reach and that for the
*     adjacent reach.  Don't compute DFACE at ISTART-1 if this is the
*     first reach.
*
      IF (ISTART .NE. 1) 
     &   DFACE(ISTART-1) = DELTAX(ISTART-1)/
     &   (DELTAX(ISTART)+DELTAX(ISTART-1)) * DFACE(ISTART) 
     &   + DELTAX(ISTART)/(DELTAX(ISTART)+DELTAX(ISTART-1))
     &   * DFACE(ISTART-2)

      I = LASTSEG(JREACH)
      IF (JREACH .NE. NREACH)
     &   DFACE(I) =  DELTAX(I)/(DELTAX(I+1)+DELTAX(I)) * DFACE(I+1) 
     &             + DELTAX(I+1)/(DELTAX(I+1)+DELTAX(I)) * DFACE(I-1) 

      RETURN
      END
