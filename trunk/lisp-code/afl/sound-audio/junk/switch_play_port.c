/* Switch output from the  headphones to the consul and conversely */

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

#define HEADPHONES 1
#define SPEAKER 0

#define AUDIO_DEVICE "/dev/audioctl"
extern char *progname;
set_port (play_port)
     int play_port;
{
  int Audio_fd;
  int err;
  audio_info_t info;

  Audio_fd = open (AUDIO_DEVICE, O_WRONLY);
  if (Audio_fd < 0)
    {
      fprintf (stderr, "%s: Cannot open audio device \n", progname);
      fprintf (stderr, "Within switch_port: %s \n", progname);
      exit (1);
    }
  if (play_port)
    {
      AUDIO_INITINFO (&info);
      info.play.port = AUDIO_HEADPHONE;
      err = ioctl (Audio_fd, AUDIO_SETINFO, &info);
      if (err < 0)
	{
	  perror ("ioctl error");
	  exit (1);
	}
    }
  else
    {
      AUDIO_INITINFO (&info);
      info.play.port = AUDIO_SPEAKER;
      err = ioctl (Audio_fd, AUDIO_SETINFO, &info);
      if (err < 0)
	{
	  perror ("ioctl error");
	  exit (1);
	}
    }
  close (Audio_fd);
}
