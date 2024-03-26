*PARCHK
      SUBROUTINE PARCHK(IV, N, NN, P, V)
C
C     LATEST REVISION  -  03/15/90  (JRD)
C
C
C  ***  CHECK NL2SOL (VERSION 2.2) PARAMETERS, PRINT CHANGED VALUES  ***
C
C
C  VARIABLE DECLARATIONS
C
C  SCALAR ARGUMENTS
      INTEGER
     +   N,NN,P
C
C  ARRAY ARGUMENTS
      DOUBLE PRECISION
     +   V(33)
      INTEGER
     +   IV(21)
C
C  LOCAL SCALARS
      DOUBLE PRECISION
     +   BIG,MACHEP,TINY,VK,ZERO
      INTEGER
     +   D0INIT,DTYPE,DTYPE0,EPSLON,I,ICH,INITS,IV1,JTINIT,JTOL0,
     +   JTOL1,JTOLP,K,L,M,NVDFLT,OLDN,OLDNN,OLDP,PARPRT,PARSV1,
     +   PRUNIT,PU
C
C  LOCAL ARRAYS
      DOUBLE PRECISION
     +   VM(27),VX(27)
      CHARACTER
     +   CNGD(12)*1,DFLT(12)*1,VN(8,27)*1,WHICH(12)*1
C
C  EXTERNAL FUNCTIONS
      DOUBLE PRECISION
     +   RMDCON
      EXTERNAL RMDCON
C
C  EXTERNAL SUBROUTINES
      EXTERNAL DFAULT,VCOPY
C
C     INTEGER IV(21), N, NN, P
C     DOUBLE PRECISION V(33)
C     DIMENSION IV(*), V(*)
C
C     EXTERNAL DFAULT, RMDCON, VCOPY
C     DOUBLE PRECISION RMDCON
C DFAULT -- SUPPLIES DFAULT PARAMETER VALUES.
C RMDCON -- RETURNS MACHINE-DEPENDENT CONSTANTS.
C VCOPY  -- COPIES ONE VECTOR TO ANOTHER.
C
C  ***  LOCAL VARIABLES  ***
C
C     INTEGER I, IV1, JTOLP, K, L, M, NVDFLT, PU
C     CHARACTER*1 CNGD(12), WHICH(12)
C     CHARACTER*1 DFLT(12), VN(8,27)
C     DOUBLE PRECISION BIG, MACHEP, TINY, VK, VM(27), VX(27), ZERO
C
C  ***  IV AND V SUBSCRIPTS  ***
C
C     INTEGER DTYPE, DTYPE0, D0INIT, EPSLON, INITS, JTINIT, JTOL0,
C    1        JTOL1, OLDN, OLDNN, OLDP, PARPRT, PARSV1, PRUNIT
C
      DATA BIG/0.0D0/, NVDFLT/27/, TINY/1.0D0/, ZERO/0.0D0/
C
      DATA DTYPE/16/, DTYPE0/29/, D0INIT/37/, EPSLON/19/,
     +     INITS/25/, JTINIT/39/, JTOL0/86/, JTOL1/87/,
     +     OLDN/45/, OLDNN/46/, OLDP/47/, PARPRT/20/,
     +     PARSV1/51/, PRUNIT/21/
C
      DATA
     + VN(1,1),VN(2,1),VN(3,1),VN(4,1),VN(5,1),VN(6,1),VN(7,1),VN(8,1)
     +   /'E',    'P',    'S',    'L',    'O',    'N',    '.',    '.'/
      DATA
     + VN(1,2),VN(2,2),VN(3,2),VN(4,2),VN(5,2),VN(6,2),VN(7,2),VN(8,2)
     +   /'P',    'H',    'M',    'N',    'F',    'C',    '.',    '.'/
      DATA
     + VN(1,3),VN(2,3),VN(3,3),VN(4,3),VN(5,3),VN(6,3),VN(7,3),VN(8,3)
     +   /'P',    'H',    'M',    'X',    'F',    'C',    '.',    '.'/
      DATA
     + VN(1,4),VN(2,4),VN(3,4),VN(4,4),VN(5,4),VN(6,4),VN(7,4),VN(8,4)
     +   /'D',    'E',    'C',    'F',    'A',    'C',    '.',    '.'/
      DATA
     + VN(1,5),VN(2,5),VN(3,5),VN(4,5),VN(5,5),VN(6,5),VN(7,5),VN(8,5)
     +   /'I',    'N',    'C',    'F',    'A',    'C',    '.',    '.'/
      DATA
     + VN(1,6),VN(2,6),VN(3,6),VN(4,6),VN(5,6),VN(6,6),VN(7,6),VN(8,6)
     +   /'R',    'D',    'F',    'C',    'M',    'N',    '.',    '.'/
      DATA
     + VN(1,7),VN(2,7),VN(3,7),VN(4,7),VN(5,7),VN(6,7),VN(7,7),VN(8,7)
     +   /'R',    'D',    'F',    'C',    'M',    'X',    '.',    '.'/
      DATA
     + VN(1,8),VN(2,8),VN(3,8),VN(4,8),VN(5,8),VN(6,8),VN(7,8),VN(8,8)
     +   /'T',    'U',    'N',    'E',    'R',    '1',    '.',    '.'/
      DATA
     + VN(1,9),VN(2,9),VN(3,9),VN(4,9),VN(5,9),VN(6,9),VN(7,9),VN(8,9)
     +   /'T',    'U',    'N',    'E',    'R',    '2',    '.',    '.'/
      DATA
     + VN(1,10),VN(2,10),VN(3,10),VN(4,10),VN(5,10),VN(6,10),VN(7,10),
     + VN(8,10)
     +   /'T',    'U',    'N',    'E',    'R',    '3',    '.',    '.'/
      DATA
     + VN(1,11),VN(2,11),VN(3,11),VN(4,11),VN(5,11),VN(6,11),VN(7,11),
     + VN(8,11)
     +   /'T',    'U',    'N',    'E',    'R',    '4',    '.',    '.'/
      DATA
     + VN(1,12),VN(2,12),VN(3,12),VN(4,12),VN(5,12),VN(6,12),VN(7,12),
     + VN(8,12)
     +   /'T',    'U',    'N',    'E',    'R',    '5',    '.',    '.'/
      DATA
     + VN(1,13),VN(2,13),VN(3,13),VN(4,13),VN(5,13),VN(6,13),VN(7,13),
     + VN(8,13)
     +   /'A',    'F',    'C',    'T',    'O',    'L',    '.',    '.'/
      DATA
     + VN(1,14),VN(2,14),VN(3,14),VN(4,14),VN(5,14),VN(6,14),VN(7,14),
     + VN(8,14)
     +   /'R',    'F',    'C',    'T',    'O',    'L',    '.',    '.'/
      DATA
     + VN(1,15),VN(2,15),VN(3,15),VN(4,15),VN(5,15),VN(6,15),VN(7,15),
     + VN(8,15)
     +   /'X',    'C',    'T',    'O',    'L',    '.',    '.',    '.'/
      DATA
     + VN(1,16),VN(2,16),VN(3,16),VN(4,16),VN(5,16),VN(6,16),VN(7,16),
     + VN(8,16)
     +   /'X',    'F',    'T',    'O',    'L',    '.',    '.',    '.'/
      DATA
     + VN(1,17),VN(2,17),VN(3,17),VN(4,17),VN(5,17),VN(6,17),VN(7,17),
     + VN(8,17)
     +   /'L',    'M',    'A',    'X',    '0',    '.',    '.',    '.'/
      DATA
     + VN(1,18),VN(2,18),VN(3,18),VN(4,18),VN(5,18),VN(6,18),VN(7,18),
     + VN(8,18)
     +   /'D',    'L',    'T',    'F',    'D',    'J',    '.',    '.'/
      DATA
     + VN(1,19),VN(2,19),VN(3,19),VN(4,19),VN(5,19),VN(6,19),VN(7,19),
     + VN(8,19)
     +   /'D',    '0',    'I',    'N',    'I',    'T',    '.',    '.'/
      DATA
     + VN(1,20),VN(2,20),VN(3,20),VN(4,20),VN(5,20),VN(6,20),VN(7,20),
     + VN(8,20)
     +   /'D',    'I',    'N',    'I',    'T',    '.',    '.',    '.'/
      DATA
     + VN(1,21),VN(2,21),VN(3,21),VN(4,21),VN(5,21),VN(6,21),VN(7,21),
     + VN(8,21)
     +   /'J',    'T',    'I',    'N',    'I',    'T',    '.',    '.'/
      DATA
     + VN(1,22),VN(2,22),VN(3,22),VN(4,22),VN(5,22),VN(6,22),VN(7,22),
     + VN(8,22)
     +   /'D',    'L',    'T',    'F',    'D',    'C',    '.',    '.'/
      DATA
     + VN(1,23),VN(2,23),VN(3,23),VN(4,23),VN(5,23),VN(6,23),VN(7,23),
     + VN(8,23)
     +   /'D',    'F',    'A',    'C',    '.',    '.',    '.',    '.'/
      DATA
     + VN(1,24),VN(2,24),VN(3,24),VN(4,24),VN(5,24),VN(6,24),VN(7,24),
     + VN(8,24)
     +   /'R',    'L',    'I',    'M',    'I',    'T',    '.',    '.'/
      DATA
     + VN(1,25),VN(2,25),VN(3,25),VN(4,25),VN(5,25),VN(6,25),VN(7,25),
     + VN(8,25)
     +   /'C',    'O',    'S',    'M',    'I',    'N',    '.',    '.'/
      DATA
     + VN(1,26),VN(2,26),VN(3,26),VN(4,26),VN(5,26),VN(6,26),VN(7,26),
     + VN(8,26)
     +   /'D',    'E',    'L',    'T',    'A',    '0',    '.',    '.'/
      DATA
     + VN(1,27),VN(2,27),VN(3,27),VN(4,27),VN(5,27),VN(6,27),VN(7,27),
     + VN(8,27)
     +   /'F',    'U',    'Z',    'Z',    '.',    '.',    '.',    '.'/
C
      DATA VM(1)/1.0D-3/, VM(2)/-0.99D0/, VM(3)/1.0D-3/, VM(4)/1.0D-2/,
     +     VM(5)/1.2D0/, VM(6)/1.0D-2/, VM(7)/1.2D0/, VM(8)/0.0D0/,
     +     VM(9)/0.0D0/, VM(10)/1.0D-3/, VM(11)/-1.0D0/, VM(15)/0.0D0/,
     +     VM(16)/0.0D0/, VM(19)/0.0D0/, VM(20)/-10.0D0/, VM(21)/0.0D0/,
     +     VM(23)/0.0D0/, VM(24)/1.0D10/, VM(27)/1.01D0/
      DATA VX(1)/0.9D0/, VX(2)/-1.0D-3/, VX(3)/1.0D1/, VX(4)/0.8D0/,
     +     VX(5)/1.0D2/, VX(6)/0.8D0/, VX(7)/1.0D2/, VX(8)/0.5D0/,
     +     VX(9)/0.5D0/, VX(10)/1.0D0/, VX(11)/1.0D0/, VX(14)/0.1D0/,
     +     VX(15)/1.0D0/, VX(16)/1.0D0/, VX(18)/1.0D0/, VX(22)/1.0D0/,
     +     VX(23)/1.0D0/, VX(25)/1.0D0/, VX(26)/1.0D0/, VX(27)/1.0D2/
C
      DATA CNGD(1), CNGD(2), CNGD(3), CNGD(4), CNGD(5), CNGD(6)
     +   /     '-',     '-',     '-',     'C',     'H',     'A'/
      DATA CNGD(7), CNGD(8), CNGD(9), CNGD(10), CNGD(11), CNGD(12)
     +   /     'N',     'G',     'E',     'D',     ' ',     'V'/
      DATA DFLT(1), DFLT(2), DFLT(3), DFLT(4), DFLT(5), DFLT(6)
     +   /     'N',     'O',     'N',     'D',     'E',     'F'/
      DATA DFLT(7), DFLT(8), DFLT(9), DFLT(10), DFLT(11), DFLT(12)
     +   /     'A',     'U',     'L',     'T',     ' ',     'V'/
C
C.......................................................................
C
      IF (IV(1) .EQ. 0) CALL DFAULT(IV, V)
      PU = IV(PRUNIT)
      IV1 = IV(1)
      IF (IV1 .NE. 12) GO TO 30
         IF (NN .GE. N .AND. N .GE. P .AND. P .GE. 1) GO TO 20
              IV(1) = 16
              IF (PU .NE. 0) WRITE(PU,10) NN, N, P
 10           FORMAT(30H0///// BAD NN, N, OR P... NN =,I5,5H, N =,I5,
     +               5H, P =,I5)
              GO TO 999
 20      K = IV(21)
         CALL DFAULT(IV(21), V(33))
         IV(21) = K
         IV(DTYPE0) = IV(DTYPE+20)
         IV(OLDN) = N
         IV(OLDNN) = NN
         IV(OLDP) = P
         DO 25 ICH = 1, 12
            WHICH(ICH) = DFLT(ICH)
 25      CONTINUE
         GO TO 80
 30   IF (N .EQ. IV(OLDN) .AND. NN .EQ. IV(OLDNN) .AND. P .EQ. IV(OLDP))
     +                       GO TO 50
         IV(1) = 17
         IF (PU .NE. 0) WRITE(PU,40) IV(OLDNN), IV(OLDN), IV(OLDP), NN,
     +                               N, P
 40      FORMAT('0///// (NN,N,P) CHANGED FROM (',I5,',',I5,',',I3,
     +          ') TO (',I5,',',I5,',',I3,').')
         GO TO 999
C
 50   IF (IV1 .LE. 11 .AND. IV1 .GE. 1) GO TO 70
         IV(1) = 50
         IF (PU .NE. 0) WRITE(PU,60) IV1
 60      FORMAT('0/////  IV(1) =',I5,' SHOULD BE BETWEEN 0 AND 12.')
         GO TO 999
C
 70   DO 75 ICH = 1, 12
         WHICH(ICH) = CNGD(ICH)
 75   CONTINUE
C
 80   IF (BIG .GT. TINY) GO TO 90
         TINY = RMDCON(1)
         MACHEP = RMDCON(3)
         BIG = RMDCON(6)
         VM(12) = MACHEP
         VX(12) = BIG
         VM(13) = TINY
         VX(13) = BIG
         VM(14) = MACHEP
         VM(17) = TINY
         VX(17) = BIG
         VM(18) = MACHEP
         VX(19) = BIG
         VX(20) = BIG
         VX(21) = BIG
         VM(22) = MACHEP
         VX(24) = RMDCON(5)
         VM(25) = MACHEP
         VM(26) = MACHEP
 90   M = 0
      IF (IV(INITS) .GE. 0 .AND. IV(INITS) .LE. 2) GO TO 110
         M = 18
         IF (PU .NE. 0) WRITE(PU,100) IV(INITS)
 100     FORMAT(25H0/////  INITS... IV(25) =,I4,20H SHOULD BE BETWEEN 0,
     +          7H AND 2.)
 110  K = EPSLON
      DO 140 I = 1, NVDFLT
         VK = V(K)
         IF (VK .GE. VM(I) .AND. VK .LE. VX(I)) GO TO 130
              M = K
           IF (PU .NE. 0) WRITE(PU,120) (VN(ICH, I), ICH=1, 8),
     +                                  (VN(ICH, I), ICH=1, 8),
     +                                  K, VK, VM(I), VX(I)
 120          FORMAT(8H0/////  ,8A1,5H.. V(,I2,3H) =,D11.3,7H SHOULD,
     +               ' BE BETWEEN',D11.3,4H AND,D11.3)
 130     K = K + 1
 140     CONTINUE
C
      IF (IV1 .EQ. 12 .AND. V(JTINIT) .GT. ZERO) GO TO 170
C
C  ***  CHECK JTOL VALUES  ***
C
      JTOLP = JTOL0 + P
      DO 160 I = JTOL1, JTOLP
         IF (V(I) .GT. ZERO) GO TO 160
         K = I - JTOL0
         IF (PU .NE. 0) WRITE(PU,150) K, I, V(I)
 150     FORMAT(12H0///// JTOL(,I3,6H) = V(,I3,3H) =,D11.3,
     +          20H SHOULD BE POSITIVE.)
         M = I
 160     CONTINUE
C
 170  IF (M .EQ. 0) GO TO 180
         IV(1) = M
         GO TO 999
C
 180  IF (PU .EQ. 0 .OR. IV(PARPRT) .EQ. 0) GO TO 999
      IF (IV1 .NE. 12 .OR. IV(INITS) .EQ. 0) GO TO 200
         M = 1
         WRITE(PU,190) IV(INITS)
 190     FORMAT(22H0NONDEFAULT VALUES..../20H INITS..... IV(25) =,I3)
 200  IF (IV(DTYPE) .EQ. IV(DTYPE0)) GO TO  210
         IF (M .EQ. 0) WRITE(PU,215) (WHICH(ICH), ICH=1, 12)
         M = 1
         WRITE(PU,205) IV(DTYPE)
 205     FORMAT(20H DTYPE..... IV(16) =,I3)
 210  K = EPSLON
      L = PARSV1
      DO 240 I = 1, NVDFLT
         IF (V(K) .EQ. V(L)) GO TO 230
              IF (M .EQ. 0) WRITE(PU,215) (WHICH(ICH), ICH = 1, 12)
 215          FORMAT ('0',12A1,'ALUES....'/)
              M = 1
              WRITE (PU,220) (VN(ICH, I), ICH = 1, 8), K, V(K)
 220          FORMAT (1X, 8A1, 5H.. V(, I2, 3H) =, D15.7)
 230     K = K + 1
         L = L + 1
 240     CONTINUE
      IV(DTYPE0) = IV(DTYPE)
      CALL VCOPY(NVDFLT, V(PARSV1), V(EPSLON))
      IF (IV1 .NE. 12) GO TO 999
         IF (V(JTINIT) .GT. ZERO) GO TO 260
              JTOLP = JTOL0 + P
              WRITE(PU,250) (V(I), I = JTOL1, JTOLP)
 250          FORMAT(24H0(INITIAL) JTOL ARRAY.../(1X,6D12.3))
 260     IF (V(D0INIT) .GT. ZERO) GO TO 999
              K = JTOL1 + P
              L = K + P - 1
              WRITE(PU,270) (V(I), I = K, L)
 270          FORMAT(22H0(INITIAL) D0 ARRAY.../1X,6D12.3)
C
 999  RETURN
C  ***  LAST CARD OF PARCHK FOLLOWS  ***
      END
