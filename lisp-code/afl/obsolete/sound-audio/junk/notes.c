#include <stdio.h>
char *progname;
char prog_opts[] = "v:t:d:l:s:";
extern int errno;
extern char *getenv ();
/* Interface to playnotes */
main (argc, argv)
     int argc;
     char *argv[];
{
  extern char *optarg;
  extern int optind, opterr;
  int length, tone;
  float volume, decay;
  char octave[80];
  char *s;
  int c;
  int error_flag = 0;
  progname = argv[0];
/* default values to be used if not specified on the command line. */
  if ((s = getenv ("N_TONE")) != NULL)
    {
      sscanf (s, "%d", &tone);
    }
  else
    {
      tone = 0;
    }
  if ((s = getenv ("N_VOLUME")) != NULL)
    {
      sscanf (s, "%f", &volume);
    }
  else
    {
      volume = 2550;
    }
  if ((s = getenv ("N_DECAY")) != NULL)
    {
      sscanf (s, "%f", &decay);
    }
  else
    {
      decay = 0;
    }
  if ((s = getenv ("N_LENGTH")) != NULL)
    {
      sscanf (s, " %d", &length);
    }
  else
    {
      length = 10;
    }
  if ((s = getenv ("N_SCALE")) != NULL)
    {
      strcpy (octave, s);
    }
  else
    {
      strcpy (octave, "5c");
    }
  while ((c = getopt (argc, argv, prog_opts)) != EOF)
    {
      switch (c)
	{
	case 'v':
	  /* volume assigned */
	  sscanf (optarg, "%f", &volume);
	  break;

	case 'l':
	  /* Length of note */
	  sscanf (optarg, "%d", &length);
	  break;

	case 't':
	  /* Tone */
	  sscanf (optarg, "%d", &tone);
	  break;

	case 'd':
	  /* decay */
	  sscanf (optarg, "%f", &decay);
	  break;

	case 's':
	  /* scan octave */
	  sscanf (optarg, "%s", octave);
	  break;

	case '?':
	default:
	  error_flag++;
	}
    }
  printf (" Calling with \n\
volume = %f\n\
tone = %d \n\
length = %d \n\
decay = %f \n\
octave = %s \n", volume, tone, length, decay, octave);
  playnotes (volume, length, tone, decay, octave);
  exit (0);
}
