/*************************************************************************
 *
 *   Function: xgraph_header
 *
 *   write the heading for the xgraph file
 *
 ************************************************************************/
#include <stdio.h>

void xgraph_header(xlabel,title,solute_xgrfile)
     char *title, *xlabel;
     FILE *solute_xgrfile;
{
  fprintf(solute_xgrfile,"Device: Postscript\n");
  fprintf(solute_xgrfile,"Disposition: To Device\n");
  fprintf(solute_xgrfile,"FileorDev: ps\n");
  fprintf(solute_xgrfile,"Titletext: %s",title);
  fprintf(solute_xgrfile,"XUnitText: %s\n",xlabel);
  fprintf(solute_xgrfile,"YUnitText: Concentration\n");
  fprintf(solute_xgrfile,"Ticks: On\n");
  fprintf(solute_xgrfile,"Markers: On\n");
  fprintf(solute_xgrfile,"Geometry: 1000x575\n");

  return;
}

