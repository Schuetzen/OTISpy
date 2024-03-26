###########################################################################
# 
#  OTIS: One-dimensional Transport with Inflow and Storage
#  -------------------------------------------------------
#  Version: MOD40         Feb. 1998
#
#
#  This makefile may be used to create OTIS, OTIS-P and/or POSTPROC.  To
#  create all three executables, simply type 'make' on the command line.
#  To create a specific executable, type 'make otis', 'make otis-p' or
#  'make postproc'.
#
#  Note that this makefile accesses more detailed makefiles in source/otis,
#  source/otis-p, source/postproc, source/share and source/starpac.
#
#                                             -- R.L. Runkel, January 1997
# 
###########################################################################
all:	otis otis-p postproc

otis:	
	cd source/share; make links; make
	cd source/otis; make

otis-p:
	cd source/share; make links-p; make
	cd source/otis-p; make

postproc:
	cd source/postproc; make





