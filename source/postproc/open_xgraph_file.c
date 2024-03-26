/*************************************************************************
 *
 *   Function: open_xgraph_file                Called by: write_recs
 *
 *   open the solute/site xgraph file
 *
 ************************************************************************/
#include <stdio.h>

void open_xgraph_file(prefixtype,solute_xgrfile,solute_filename,prtloc,
                      summary_file)
     char solute_filename[];
     double prtloc;
     FILE *summary_file, **solute_xgrfile;
     long prefixtype;
{
  extern FILE *openfile();
  char solute_xgr_filename[80];
  char xgrprefix[10];
/*
 *  open the solute xgraph file.  
 *
 *  For dynamic simulations (prefixtype=0), the 'xgraph' filename is based
 *  on the name of the 'solute output' file, with the printlocation and
 *  '.xgr' appended to the end.  Note that the print location is rounded to
 *  the nearest integer before being appended to the filename.  Decimal
 *  print locations (e.g. 10.3) will therefore (due to the rounding)
 *  have inexact 'xgraph' filenames (e.g. 10.3 is rounded to 10 for the
 *  filename)
 *
 *  For a steady state simulations, the output filenames are simply the
 *  'solute output' filename with an '.xgr' extension for the main channel
 *  (prefixtype=1) and a '.stor.xgr' for the storage zone (prefixtype=2).
 *
 */
  strcpy(solute_xgr_filename,solute_filename);
  if (prefixtype == 0)
    sprintf(xgrprefix,".%d",(long)(prtloc + 0.5));
  else if (prefixtype == 1)
    strcpy(xgrprefix,"");
  else if (prefixtype == 2)
    strcpy(xgrprefix,".stor");
  else
    printf("Error: open_xgraph_file: prefixtype not defined for this value.\n");
  strcat(solute_xgr_filename,xgrprefix);
  strcat(solute_xgr_filename,".xgr");
  *solute_xgrfile = openfile(solute_xgr_filename, "w");
/*
 *  write to summary file
 */
  fprintf(summary_file,"\nProcessing %s and creating %s\n",
          solute_filename,solute_xgr_filename);
  return;
}

