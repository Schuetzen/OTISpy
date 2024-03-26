/*************************************************************************
 *    Function: input			Called by: postproc
 *
 *    read variables from the parameter file
 *
 *    Local variables
 *    ---------------
 *    buffer           input buffer for reading from files
 *    param_filename   filename of parameter input file
 *    param_file       file pointer to the param_filename
 *    pstep            print step 
 *    tstart           simulation start time
 *    tfinal           simulation end time
 *    iprint           print interval
 *    idecay           decay flag; specifies whether decay is modeled
 *    nseg             number of segments in the simulation
 *    j                loop index
 *
 ************************************************************************/
#include <stdio.h>

void input(prtopt,tstep,nprint,nreach,nsolute,num_records,title,isorb,
           prtloc,otis_p,data_filename,control_file)

     long *prtopt,*nprint,*nreach,*nsolute,*num_records,*isorb;
     long otis_p;
     char title[], data_filename[];
     double *tstep, **prtloc;
     FILE *control_file;
{
  extern FILE *openfile();
  extern void skip_records2();
  char param_filename[80], buffer[500];
  FILE *param_file;
  long j, iprint, idecay, *nseg;
  double pstep, tstart, tfinal;
/*
 *  determine the name of the parameter file and open it
 */
  getline(control_file,buffer);
  sscanf(buffer, "%s", param_filename );
  param_file = openfile(param_filename, "r");
/*
 *  determine the name of the data file if appropriate
 */
  skip_records2(control_file, 1);
  if (otis_p) {
    getline(control_file,buffer);
    sscanf(buffer, "%s", data_filename);
  }

/*
 *  read the title and print option from the parameter file
 */
  getline(param_file,buffer);
  strcpy (title,buffer);
  getline(param_file,buffer);
  sscanf( buffer, "%d", prtopt );
/*
 *  read the print step, time step, start time, and final time
 *  so that the number of records in the output file may be computed
 */
  getline(param_file,buffer);
  sscanf( buffer, "%lf",&pstep);
  getline(param_file,buffer);
  sscanf( buffer, "%lf",tstep);
  getline(param_file,buffer);
  sscanf( buffer, "%lf",&tstart);
  getline(param_file,buffer);
  sscanf( buffer, "%lf",&tfinal);
/*
 *  skip 2 records, read nreach, read nseg for each reach
 */
  skip_records2(param_file,2);
  getline(param_file,buffer);
  sscanf( buffer, "%d", nreach );
  nseg = (long *) malloc ( *nreach * sizeof(long));
  for (j=0; j<*nreach; j++) {
    getline(param_file,buffer);
    sscanf(buffer,"%d",nseg+j);
  }
/*
 *  read the number of solutes and the flags idecay and isorb
 *  (nsolute should be 1 for otis-p)
 */
  getline(param_file,buffer);
  sscanf( buffer, "%d %d %d", nsolute, &idecay, isorb );
/* 
 *  Skip over decay and/or sorption parameters
 */
  if (idecay == 1) skip_records2(param_file,*nsolute * *nreach);
  if (*isorb == 1) skip_records2(param_file,*nsolute * *nreach);
/*
 *  read the number of print locations, allocate space in
 *  the print location vector, and read the print locations
 */
  getline(param_file,buffer);
  sscanf( buffer, "%d", nprint );
  *prtloc = (double *) malloc ( *nprint * sizeof(double));
  for  (j=0; j < *nprint; j++) {
    getline(param_file,buffer);
    sscanf( buffer,"%lf",*prtloc+j );
  }
/*
 * determine the number of records.  For a steady-state simulation
 * (tstep=0), the number of records equals the number of segments in the
 * system. (For a steady-state simulation nprint must be one.)  For a 
 * dynamic simulation, the number of records is a function of the print 
 * interval (iprint) and the time step (tstep).
 */
  if (*tstep == 0) {
    for (j=0; j<*nreach; j++) *num_records += nseg[j];
    *nprint = 1;
  } else {
    iprint = (long) (pstep / *tstep + 0.5);
    *num_records = (((int)((tfinal-tstart)/ *tstep ) + 1) / iprint + 1);
  }

  fclose( param_file );
  return;
}
