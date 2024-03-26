###########################################################################
# 
#  Makefile for POSTPROC - POST7
# 
#  This makefile is accessed by ../../makefile to compile POSTPROC
#
###########################################################################
CC = cc
CDEBUGFLAGS = -g 
CFLAGS = -O
OBJS = postproc.o open_files.o process.o input.o echo.o write_recs.o \
       skip_records2.o read_recs.o open_solute_file.o getline.o openfile.o \
       expfix.o open_xgraph_file.o xgraph_header.o setoptions.o
###########################################################################
#
#   POSTPROC
#
###########################################################################
postproc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)
	mv -f $@ ../../$@
