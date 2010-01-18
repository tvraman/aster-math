/* Modified to implement a naive notes function */
/* Same as playnotes just removing the underscore in the name in case
   lisp barfs at it */
/* Also renaming variable buf to octave */

#include <stdio.h>
#include <strings.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sun/audioio.h>
#define BUFLEN 256
static char buffer[BUFLEN];

playnotes (volume, length, tone, decay, octave)
     int tone, length;
     float volume, decay;
     char octave[];

{

  static int notes[9][12] =
  {
    {1002, 946, 893, 843, 795, 751, 709, 669, 631, 596, 562, 531},
    {501, 473, 446, 421, 398, 375, 354, 334, 316, 298, 281, 265},
    {250, 236, 223, 211, 199, 188, 177, 167, 158, 149, 141, 133},
    {125, 118, 112, 105, 99, 94, 89, 84, 79, 74, 70, 66},
    {63, 59, 56, 53, 50, 47, 44, 42, 39, 37, 35, 33},
    {31, 30, 28, 26, 25, 23, 22, 21, 20, 19, 18, 17},
    {16, 15, 14, 13, 12, 11, 11, 10, 10, 9, 9, 8},
    {7, 7, 7, 7, 6, 6, 6, 5, 5, 5, 4, 4},
    {4, 4, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2}
  };

  static int conv[7] =
  {9, 11, 0, 2, 4, 5, 7};

  int oct, n, f, period, j, i, k;

  f = open ("/dev/audio", O_WRONLY);
  if (f < 0)
    {
      fprintf (stderr, "play-notes: Cannot open /dev/audio \n");
      return (1);
    }

  oct = octave[0] - '0';
  n = conv[octave[1] - 'a'];
  if (octave[2] == 'b')
    --n;
  else if (octave[2] == '#')
    ++n;
  n = (n + tone) % 12;

  period = notes[oct][n];

  i = 0;
  k = 0;

  for (;;)
    {
      for (j = 0; j < period >> 1; ++j)
	{
	  buffer[i++] = 0;
	  if ((volume = volume - decay) < 0.0)
	    volume = 0.0;
	  if (i >= BUFLEN)
	    {

	      write (f, buffer, BUFLEN);

	      if (++k >= length)
		break;

	      i = 0;
	    }
	}
      for (j = 0; j < ((period >> 1) + (period & 0x1)); ++j)
	{
	  buffer[i++] = (int) volume;
	  if ((volume = volume - decay) < 0.0)
	    volume = 0.0;
	  if (i >= BUFLEN)
	    {
	      write (f, buffer, BUFLEN);

	      if (++k >= length)
		{
		  if (close (f))
		    {
		      printf ("closed /dev/audio before exiting \n");

		      printf ("Tried but failed \n");
		      return (1);
		    }
		  else
		    {

		      return;
		    }
		}
	      i = 0;
	    }
	}

    }
  /* close before exiting. */

  if (close (f))
    {
      printf ("closed /dev/audio before exiting \n");
      printf ("Tried but failed \n");
      return (1);
    }

  return;
}
