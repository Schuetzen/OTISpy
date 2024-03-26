/************************************************************************
 *   Function: getline.c 			Author: Rob Runkel
 *
 *   read from the input file designated by input_file, discarding
 *   comment lines and returning the first line of valid input data.
 *
 *   Function Arguments
 *   ------------------
 *   input_file   -  pointer to the input_file
 *   buffer       -  charater buffer for input line
 *  
 ************************************************************************/
#include <stdio.h>
void getline(input_file,buffer)
     FILE *input_file;
     char buffer[];
{
  while(1) 
    {
      fgets(buffer,500,input_file);
      if (buffer[0] != '#') break;
    }
  return;
}
