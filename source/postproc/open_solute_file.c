/*************************************************************************
 *
 *   Function: open_solute_file           Called by: process
 *
 *   open solute files for input
 *
 ************************************************************************/
#include <stdio.h>

void open_solute_file(solute_filename,control_file,solute_outfile)
     char solute_filename[];
     FILE *control_file, **solute_outfile;
{
  extern FILE *openfile();
  char buffer[500];
/*
 *  determine the name of the solute output file and open it
 */
  getline(control_file,buffer);
  sscanf( buffer, "%s", solute_filename );
  *solute_outfile = openfile(solute_filename, "r");

  return;
}

