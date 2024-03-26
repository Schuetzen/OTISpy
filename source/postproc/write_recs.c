/************************************************************************
 *
 *   Function: write_recs                     Called by: process
 *
 *   write the solute xgraph file
 *
 ************************************************************************/
#include <stdio.h>

void write_recs(prtopt,tstep,nprint,nreach,num_records,time,conc,title,
                solute_filename,stor_opt,prtloc,otis_p,no_data,no_head,
                data_filename,data_file,summary_file)
     long prtopt,nprint,nreach,num_records,stor_opt,otis_p,no_data,no_head;
     double *time, **conc;
     char title[], solute_filename[], data_filename[];
     double *prtloc, tstep;
     FILE *summary_file, *data_file;
{
  long j,k,num_data_pts,count;
  char loc[60], buffer[500];
  char *location;
  FILE *solute_xgrfile;

  location = loc;

/*
 * Write Solute xgraph files.  
 *
 * Dynamic Simulations:
 * Write one xgraph file for each of the nprint print locations
 *
 * Steady-State Simulations (tstep=0):
 * Write one xgraph file for the main channel and observed data, 
 * and one xgraph file for the storage zone if appropriate.
 * (nprint has been set to 1)
 * 
 */
  for (k=0; k<nprint; k++) {
    /*
     * write main channel or sorbed concentrations
     */
    strcpy (location, "\"stream" );
    if (tstep == 0) {
      open_xgraph_file(1,&solute_xgrfile,solute_filename,prtloc[k],
                       summary_file);
      if (!no_head) {
        xgraph_header("Distance",title,solute_xgrfile);
        fprintf(solute_xgrfile,"\n %s, File:%s \n",
                location,solute_filename);
      }
    } else {
      open_xgraph_file(0,&solute_xgrfile,solute_filename,prtloc[k],
                       summary_file);
      if (!no_head) {
        xgraph_header("Time [hours]",title,solute_xgrfile);
        fprintf(solute_xgrfile,"\n %s @%lf, File:%s \n",
                location,prtloc[k],solute_filename);
      }
    }
    for(j=0;j<num_records;j++)
      fprintf(solute_xgrfile,"%.6e  %.6e\n",time[j],conc[k][j]);
    fprintf(solute_xgrfile,"\n");
    /*
     * write the observed data from the data input file unless no_data
     * is specified.  (otis-p only.)  
     */
    if (!no_data && otis_p) {
      strcpy (location, "\"observed data" );
      if (tstep == 0) {
        for (count=1;count<=nreach;count++) {
          if (!no_head) 
            fprintf(solute_xgrfile," %s, reach %d, File:%s \n",
                    location,count,data_filename);
          getline(data_file,buffer);
          sscanf(buffer, "%d", &num_data_pts);
          if (!no_head) {
            for (j=1; j<=num_data_pts; j++) {
              getline(data_file,buffer);
              fprintf(solute_xgrfile,"move  %s", buffer);
              fprintf(solute_xgrfile,"      %s", buffer);
            }
          } else {
            for (j=1; j<=num_data_pts; j++) {
              getline(data_file,buffer);
              fprintf(solute_xgrfile,"%s", buffer);
            }
          }
          fprintf(solute_xgrfile,"\n");
        }
      } else {
        if (!no_head) 
          fprintf(solute_xgrfile," %s @%lf, File: %s \n",
                  location,prtloc[k],data_filename);
        getline(data_file,buffer);
        sscanf(buffer, "%d", &num_data_pts);
        if (!no_head) {
          for (j=1; j<=num_data_pts; j++) {
            getline(data_file,buffer);
            fprintf(solute_xgrfile,"move  %s", buffer);
            fprintf(solute_xgrfile,"      %s", buffer);
          }
        } else {
          for (j=1; j<=num_data_pts; j++) {
            getline(data_file,buffer);
            fprintf(solute_xgrfile,"%s", buffer);
          }
        }
        fprintf(solute_xgrfile,"\n");
      }
    }
    /*
     * if requested, write storage zone concentrations.  For steady-state
     * simulations, open a separate xgraph file.
     */
    if (stor_opt && prtopt == 2) {
      strcpy (location, "\"storage");
      if (tstep == 0) {
        fclose(solute_xgrfile);
        open_xgraph_file(2,&solute_xgrfile,solute_filename,prtloc[k],
                         summary_file);
        if (!no_head) {
          xgraph_header("Distance",title,solute_xgrfile);
          fprintf(solute_xgrfile," %s, File:%s \n",
                  location,solute_filename);
        }
      } else {
        if (!no_head) 
          fprintf(solute_xgrfile," %s @%lf, File:%s \n",
                  location,prtloc[k],solute_filename);
      }
      for(j=0;j<num_records;j++)
        fprintf(solute_xgrfile,"%.6e  %.6e\n",time[j],conc[k+nprint][j]);
      fprintf(solute_xgrfile,"\n");
    }
    fclose(solute_xgrfile);
  }
  return;
}

