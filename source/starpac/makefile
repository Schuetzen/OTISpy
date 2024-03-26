###########################################################################
# 
#  Makefile for STARPAC subroutines.
# 
#  This makefile is accessed by ../../makefile to create star.a library
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
OBJS = nlswc.o iprint.o nlcntn.o nler.o nlhdrn.o drv.o nldrvn.o prtcnt.o \
       dcopy.o i1mach.o stpcnt.o stkclr.o nlcnt.o stkst.o ldscmp.o stkget.o \
       icnti.o stkset.o stkrel.o setiv.o d1mach.o eisge.o cpyasf.o stpout.o \
       ervgt.o cpyvii.o fdump.o ervwt.o versp.o etamdl.o stpmn.o nlmn.o \
       seterr.o stpadj.o nl2itr.o ervgtp.o fixprt.o ehdr.o nlfin.o nlism.o \
       gmean.o nlinit.o relcom.o nlerr.o nlsupk.o nlitrp.o stpsel.o abscom.o\
       vcopy.o i8save.o covclc.o eprint.o dupdat.o qapply.o qrfact.o vaxpy.o\
       dotprd.o e9rint.o nlcmp.o slvmul.o rptmul.o lmstep.o gqtstp.o \
       parchk.o nlspk.o icopy.o stopx.o vscopy.o lstvcf.o nlout.o v2norm.o \
       dfault.o rmdcon.o itsmry.o slupdt.o assess.o linvrt.o imdcon.o \
       fitpt1.o inperl.o fitext.o vcvotf.o fitpt2.o s88fmt.o lsqrt.o \
       ltsqar.o ppft.o litvmu.o lsvmin.o livmul.o reldst.o nlskl.o ppfnml.o \
       matprf.o getpi.o obssum.o dnrm2.o cmpfd.o setrow.o eiveq.o mvchk.o
###########################################################################
#
#   create star.a library
#
###########################################################################
star.a:	$(OBJS) 
	ar ruv star.a $(OBJS) 





