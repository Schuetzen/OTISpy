/************************************************************************
 *
 *  Function: process                     Called by: postproc
 *
 *  create the xgraph file for each solute
 *
 *
 *  local variables
 *  ------------------
 *  time              pointer to an array of times
 *  conc              pointer to an array of concentrations
 *  solute_filename   filename of output file created by OTIS
 *  solute_outfile    file pointer to solute_filename
 *  data_file         file pointer to data_filename
 *
 * 
 *  loop integers
 *  -------------
 *  j - solute
 *  k - prtloc
 *
 ***********************************************************************/
#include <stdio.h>

void  process(prtopt,tstep,nprint,nreach,nsolute,num_records,title,
              stor_opt,prtloc,isorb,otis_p,no_data,no_head,data_filename,
              control_file,summary_file)

     long prtopt,nprint,nreach,nsolute,num_records,stor_opt,isorb;
     char title[], data_filename[];
     double *prtloc, tstep;
     FILE *control_file, *summary_file;
     long otis_p, no_data, no_head;
{
  extern void open_solute_file(),read_recs(),write_recs(),skip_records2();
  extern FILE *openfile();
  long j, k;
  double *time,**conc;
  char solute_filename[80];
  FILE *solute_outfile, *data_file;

/*
 * skip unused records in OTIS-P control file.
 */
  if (otis_p) 
    skip_records2(control_file, 3);
/*
 *  allocate space for time and concentration arrays
 */
  time = (double *) malloc ( num_records * sizeof(double));
  conc = (double **) malloc ( nprint * prtopt * sizeof(double *));
  for (k=0;k<nprint*prtopt;k++)
    conc[k] = (double *) malloc(num_records * sizeof(double));
/*
 * process input and create xgraph files for each solute
 */
  for (j=0; j<nsolute; j++) {
    if (otis_p && !no_data) { data_file = openfile(data_filename,"r"); }
    open_solute_file(solute_filename,control_file,&solute_outfile);
    read_recs(prtopt,nprint,num_records,time,conc,solute_outfile);
    write_recs(prtopt,tstep,nprint,nreach,num_records,time,conc,title,
               solute_filename,stor_opt,prtloc,otis_p,no_data,no_head,
               data_filename,data_file,summary_file);
    if (otis_p && !no_data) { fclose(data_file); }
    fclose(solute_outfile);
  }

/*
 * if sorption is modeled (isorb=1) process input and create xgraph
 * files for sorbed concentration, C*.
 */
  if (isorb) {
    for (j=0; j<nsolute; j++) {
      if (otis_p && !no_data) { data_file = openfile(data_filename,"r"); }
      open_solute_file(solute_filename,control_file,&solute_outfile);
      read_recs(1,nprint,num_records,time,conc,solute_outfile);
      write_recs(1,tstep,nprint,nreach,num_records,time,conc,title,
	         solute_filename,stor_opt,prtloc,otis_p,no_data,no_head,
                 data_filename,data_file,summary_file);
      if (otis_p && !no_data) { fclose(data_file); }
      fclose(solute_outfile);
    }
  }
 
  return;
}

