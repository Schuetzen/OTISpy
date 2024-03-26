/************************************************************************
 *   Function:   open_files              Called by: postproc
 *
 *   open the control and summary files
 * 
 ************************************************************************/
#include <stdio.h>

void open_files(control_file,summary_file)
     FILE **control_file, **summary_file;
{
  extern FILE *openfile();

  *control_file = openfile("control.inp", "r");
  *summary_file = openfile("postproc.out", "w");

  return;
}

