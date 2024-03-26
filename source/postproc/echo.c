/***********************************************************************
 *  Function: echo.c               Called by: postproc
 *
 *  echo input parameters to the summary file
 *
 ***********************************************************************/
#include <stdio.h>

extern void echo(prtopt,tstep,nprint,nsolute,num_records,title,prtloc,
		 summary_file)

     long prtopt,nprint,nsolute,num_records;
     char title[];
     double *prtloc;
     FILE *summary_file;
     double tstep;
{
  long j;

  fprintf(summary_file,"\n\nPost-processor to Create Xgraph Files\n\n");
  fprintf(summary_file,"Run Title                : %s",title);
  fprintf(summary_file,"Print Option             : %d\n",prtopt);
  fprintf(summary_file,"Number of Solutes        : %d\n",nsolute);
  fprintf(summary_file,"Number of Records        : %d\n",num_records);

  if (tstep > 0) {
    fprintf(summary_file,"Number of Print Locations: %d\n\n",nprint);
    for (j=0; j < nprint; j++)
      fprintf(summary_file,"Print Location %d @ %lf\n",j+1,*(prtloc+j));
  }
  return;
}

