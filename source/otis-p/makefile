###########################################################################
# 
#  Makefile for OTIS-P - MOD40
# 
#  This makefile is accessed by ../../makefile to compile OTIS-P.
#
#############################################################################
#
#  Notes:
#
#
#  I. Fortran Definitions
#
#      1) Debug Flags
#
#         FFLAGS = -C -g -u
#
#      2) Sample Optimization Flags
#
#         OS/Hardware       Compiler                    FFLAGS
#         -------------------------------------------------------
#         SunOS/SUN         SPARCompiler Fortran        -fast -O3
#         Solaris/SUN       SPARCompiler Fortran        -fast -O3
#         Linux/Pentium II  GNU F77                     -O
#	  DG-UX/DG          Green Hills Fortran-88000   -OLM -u
#	  AIX/IBM           AIX XL FORTRAN Comp/6000    -O3
#	  Digital UNIX/DEC  Digital Fortran 77          -O4	 
#
#
#  II. OTIS-P Object files
#
#        OBJ  - source code that is local to OTIS-P
#        SHARE - source code that is shared with OTIS (../share)
#        STAR  - source code from STARPAC
#
###########################################################################
FFLAGS = -O -u
F77 = f77 $(FFLAGS)
SHARE = ../share/share.a
STAR = ../starpac/star.a
OBJ = closef.o datachk.o error4.o error5.o error6.o input.o ldainit.o \
      main.o maininit.o mainrun.o okparam.o openfile.o otisrun.o reachest.o \
      setpv.o setpv2.o setup.o setup2.o setup3.o starinit.o \
      weights2.o dynamic2.o
###########################################################################
#
#   OTIS-P
#
###########################################################################
otis-p:	$(OBJ) $(STAR)
	$(F77) $(FFLAGS) -o $@ $(OBJ) $(SHARE) $(STAR)
	mv -f $@ ../../$@
$(STAR):
	cd ../starpac; make
