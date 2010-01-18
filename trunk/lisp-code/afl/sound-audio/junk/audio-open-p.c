
 /* created: thu may 13 11:11:58 edt 1993 */
/* predicate to check if audio device is open */
#ifndef lint
static char sccsid[] = "@(#)play.c 1.2 90/01/02 copyr 1989 sun micro";
#endif

/* g46
   compile as cc -c filename -lm -laudio */

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/signal.h>

#include <stropts.h>
#include <sys/ioctl.h>

#include </usr/demo/SOUND/multimedia/libaudio.h>
#include </usr/demo/SOUND/multimedia/audio_device.h>


#define	error		(void) fprintf


/* local variables */
char *prog;
char *audio_dev = "/dev/audio";
audio_info_t info;

int audio_fd = -1;		/* file descriptor for audio device */
Audio_hdr dev_hdr;		/* audio header for device */
/* global variables */

audio_open_p ()
     /* check if /dev/audio is open
        return 1 if open
        0  if available */
{
  audio_fd = open (audio_dev, O_WRONLY | O_NDELAY);
  if (audio_fd < 0)
    {
      return (1);
    }				/* device busy */
  else
    {
      /*device available and opened, so close it */
      if (audio_fd > 0)
	(void) close (audio_fd);
      return (0);
    }
}



/*
   main()
   {

   int flag;
   flag = audio_open_p();
   printf("%d = flag \n", flag);
   }
 */
