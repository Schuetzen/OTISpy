/*************************************************************************
 *    Function: setoptions              Called by: postproc
 *
 *    parse the command line arguments to set the appropriate flags
 *
 ************************************************************************/

#define TRUE 1
#define FALSE 0

long setoptions( argc, argv, stor_opt, otis_p, no_data, no_head)
  int argc;
  char *argv[];
  long *stor_opt, *otis_p, *no_data, *no_head;
{
  int count;

/*
 * Parse command line arguments.  Display a detailed usage
 * message if the -h (help) flag is specified; display a
 * brief usage message if an undefined flag is specified.
 * If usage information was printed, return condition 1.
 */
  for (count=1; count < argc; count++) {
    if ( strcmp(argv[count], "-s") == 0) { *stor_opt=TRUE; }
    else if (strcmp(argv[count], "-p") == 0) { *otis_p=TRUE; }
    else if (strcmp(argv[count], "-nd") == 0) { *no_data=TRUE; }
    else if (strcmp(argv[count], "-nx") == 0) { *no_head=TRUE; }
    else if (strcmp(argv[count], "-h") == 0) {
      printf("Usage: \n");
      printf(" postproc [-h] [-s] [-p [-nd] ] [-nx] \n");
      printf("\t   -h  Display this help screen, then exit successfully.\n");
      printf("\t   -s  Print storage zone concentrations if available.\n");
      printf("\t   -p  Parameter estimation: Indicates input from OTIS-P.\n");
      printf("\t   -nx Do not include header information in output files.\n");
      printf("\t   -nd Do not include observed data points.  Valid only\n.");
      printf("\t        when the -p flag is specified.\n\n");
      return 1;
    } else {
      printf("Usage: postproc [-h] [-s] [-p [-nd] ] [-nx] \n");
      return 1;
    }
  }
  if ( *no_data && ! *otis_p) {
    printf("Invalid option: -nd.  Option ignored.\n");
  }
  return 0;
}
