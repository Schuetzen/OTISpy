/************************************************************************
 *
 *   Function: skip_records2.c    Author:  R.L. Runkel
 *                                         USGS-WRD
 *                                         Mail Stop 415
 *                                         Denver Federal Center
 *                                         Lakewood, CO 80225
 *                                         runkel@otis.cr.usgs.gov
 *
 *                               Date  :   6 January 1995
 *
 *  A function to skip 'num_skip' records in an input file.
 *
 *  Modified version of 'skip_records.c' developed for use with the OTIS
 *  postprocessor, 'postproc3.c'.  This version skips the requested
 *  number of records plus any records with a '#' in column 1.  Records of
 *  this type are comment lines within the input files.
 *
 *  Function Arguments
 *  ------------------
 *  skip_file   - a file pointer to the input file
 *  num_skip    - the number of records to skip 
 *  
 ************************************************************************/
#include <stdio.h>

void skip_records2( skip_file, num_skip)

     FILE *skip_file; 
     long num_skip;
{
  long j;
  char unwanted_record[2000];
  for (j=0; j<num_skip; j++)
    {
      fgets( unwanted_record, 2000, skip_file);
      if (unwanted_record[0] == '#') j--;
    }
  return;
}
