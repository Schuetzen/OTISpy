/***********************************************************************
 *  Function: expfix.c   		Author: Zac Vohs
 *					Date:   May 24, 1996
 *
 *  Some output read in from the OTIS/OTEQ output files is improperly
 *  formatted for xgraph -- for example, output from OTIS running under
 *  Solaris may contain three digit exponenents which do not contain an
 *  'e' (e.g. 1.35289-100).  This function inserts an 'e' if appropriate
 *  (e.g. 1.35289e-100).
 *
 *  Function Arguments
 *  ------------------
 *  conc_pointer   -  pointer to the string containing the input number
 *
 ***********************************************************************/

expfix (conc_pointer)
  char *conc_pointer;
{
  int search_position;
  char temp_string[2];

/*
 *  search sequentially through the string containing the number.  If an
 *  'e' is found the string is returned unchanged.  If a '+' or '-' is
 *  found, the 'e' is missing and is therefore inserted.
 */

  search_position = 1;
  while (conc_pointer[search_position] != '\0')
  {
    if (conc_pointer[search_position] == 'e' ||
        conc_pointer[search_position] == 'E')
      break;
    if (conc_pointer[search_position] == '+' ||
        conc_pointer[search_position] == '-')
    {
      temp_string[0]='e';
      while(conc_pointer[search_position-1]!='\0')
      {
        temp_string[1] = conc_pointer[search_position];
        conc_pointer[search_position] = temp_string[0];
        temp_string[0] = temp_string[1];
        search_position++;
      }
      break;
    }
    search_position++;
  }
  return;
}
