###########################################################################
# 
#  Makefile for subroutines shared by OTIS and OTIS-P.
# 
#  This makefile is accessed by ../../makefile to create share.a library
#
#############################################################################
#
#  Fortran Definitions
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
###########################################################################
RM = rm -f
LN = ln -s
F77 = f77 $(FFLAGS)
FFLAGS = -O -u
SHARE = ../share/share.a
OBJS = bcchange.o conser.o decomp.o distance.o dynamic.o error.o error2.o \
       error3.o flowinit.o getline.o header.o init.o input1.o input2.o \
       input3.o input4.o input5.o input6.o input7.o inputq.o matrix.o \
       openfile2.o openin.o outinit.o output.o outputss.o preproc.o \
       preproc1.o preproc2.o preproc3.o qainit.o qchange.o qsteady.o \
       qunsteady.o qweights.o react.o readq.o setstop.o settype.o ssconc.o \
       ssdiff.o steady.o substit.o update.o weights.o wtavg.o yinterp.o \
       saven.o
###########################################################################
#
#   create share.a
#
#   Important Note:  share.a must be created seperately for OTIS and OTIS-P
#   as the include files differ for each model.  This is taken care of by
#   links, links-p, and clean (../../makefile and this makefile remove all
#   .o files and set up the links before creating/updating share.a)
#
###########################################################################
share.a:	$(OBJS) 
	ar ruv share.a $(OBJS) 

links:	clean
	$(LN) ../otis/fmodules.inc .; $(LN) ../otis/lda.inc .;
links-p:	clean
	$(LN) ../otis-p/fmodules.inc .; $(LN) ../otis-p/lda.inc .;
clean:
	$(RM) *.o *.inc

