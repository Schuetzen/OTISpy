/*********************************************************************
 *
 *   Program: postproc7     Author:  R.L. Runkel & Zac Vohs
 *                                   USGS-WRD
 *                                   Mail Stop 415
 *                                   Denver Federal Center
 *                                   Lakewood, CO 80225
 *                                   runkel@usgs.gov
 *
 *    
 *   A program to process the output from OTIS so that it is in a format
 *   useable with Xgraph.
 *
 *   This version of postproc was developed for use with MOD37 of OTIS
 *   and OTIS-P.
 *
 *
 *   Bug
 *   ---
 *   This program uses the name of each 'solute output' file (as listed
 *   in the control file) to develop names for the 'xgraph' files.  The
 *   filename of each xgraph file is of the form:
 *
 *       solute-output-filename.999m.xgr
 *
 *   where '999' is the distance in meters of a given print location
 *   (Prtloc).  The distance used in the filename is the distance
 *   specified for the print location, rounded to the nearest integer
 *   meter.  As a result, print locations that are not integers will have
 *   'xgraph' filenames that are not exact (e.g. if Prtloc[i] = 10.3, the
 *   filename will be 'solute-output-filename.10m.xgr').
 *
 *   Input Variables & Constants
 *   ---------------------------
 *   prtopt        print option
 *   nprint        number of print locations
 *   nreach        number of reaches
 *   nsolute       number of solutes 
 *   num_records   number of records to process
 *   isorb         sorption flag
 *   tstep         time step of simulation
 *   prtloc        distance at which results are printed [m]
 *   title         simulation title
 *   data_filename file containing observed data (otis-p only)
 *
 *   Input/Output File pointers
 *   --------------------------
 *   control_file    Control input file (control.inp)
 *   summary_file    Summary output file (postproc.out)
 *
 *   Boolean variables  
 *   -----------------
 *   otis_p        input is from otis-p
 *   stor_opt      include storage zone concentrations
 *   no_data       exclude observed data (otis-p only)
 *   no_head       exclude xgraph header information
 *
 ************************************************************************/
#include <stdio.h>
/*
 * Define true and false to be used with logical type variables
 *  such as stor_opt and otis_p.
 */
#define TRUE 1
#define FALSE 0

/************************************************************************/
main(argc, argv)
     int argc;
     char *argv[];
{
  extern void open_files(), input(), process();
  extern long setoptions();
  
  long prtopt, nprint, nreach, nsolute, num_records, isorb;
  char title[80], data_filename[120];
  FILE *control_file, *summary_file;
  double tstep, *prtloc;
  long stor_opt=FALSE;
  long otis_p=FALSE;
  long no_data=FALSE;
  long no_head=FALSE;
/*
 * Set the appropriate option flags according to the command line arguments.
 * If an incorrect flag is specified or help is requested (-h), print
 * usage information and exit. 
 */
  if (setoptions (argc,argv,&stor_opt,&otis_p,&no_data,&no_head)) { exit(0); }

/*
 * process OTIS/OTIS-P output
 */
  open_files(&control_file,&summary_file);
  input(&prtopt,&tstep,&nprint,&nreach,&nsolute,&num_records,title,&isorb,
	&prtloc,otis_p,data_filename,control_file);
  echo(prtopt,tstep,nprint,nsolute,num_records,title,prtloc,summary_file);
  process(prtopt,tstep,nprint,nreach,nsolute,num_records,title,stor_opt,
          prtloc,isorb,otis_p,no_data,no_head,data_filename,control_file,
          summary_file);
  fclose(control_file);
  fclose(summary_file);
  printf ("The postprocessor is finished\n");
  exit(0);
}

