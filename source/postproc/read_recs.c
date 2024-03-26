/************************************************************************
 *
 *   Function: read_recs                  Called by: process
 *
 *   read records from the solute output file
 *
 ************************************************************************/
#include <stdio.h>

void read_recs(prtopt,nprint,num_records,time,conc,solute_outfile)

     long prtopt,nprint,num_records;
     double *time, **conc;
     FILE *solute_outfile;
{
  long j,k,search_position;
  extern void skip_records2(), expfix();
  char conc_string[14];
  char *conc_pointer;

  for(j=0;j<num_records;j++) {
    fscanf(solute_outfile, "%lf", time+j);
    for (k=0; k<(nprint*prtopt);k++) {
      fscanf(solute_outfile,"%s",conc_string);

      /* Ensure that the string contains an 'e' */
      conc_pointer = conc_string;
      expfix (conc_pointer);

      sscanf(conc_string,"%lf",&conc[k][j]);
    }
    skip_records2(solute_outfile,1);
  }

  return;
}

