/************************************************************************
 *
 *   Function: openfile.c    Author:  R.L. Runkel
 *                                    Center for Advanced Decision
 *                                    Support for Water and
 *                                    Environmental Systems (CADSWES)
 *                                    University of Colorado
 *                                    Boulder, CO 80301
 *
 *                           Date  :  17 January 1991
 *
 *   
 *   A C function to open a file.
 *
 *   Function Arguments
 *   ------------------
 *   filename             name of the file to be opened
 *   access_mode          access mode
 *
 ************************************************************************/
#include <stdio.h>

FILE *openfile( filename, access_mode )
     char *filename, *access_mode;
{
  FILE *fp;
  if ((fp = fopen( filename, access_mode )) == NULL)
    {  
      fprintf( stderr, "Error opening file %s with access mode %s\n",
	       filename, access_mode );
      exit(1);
    }
  return fp;
}
