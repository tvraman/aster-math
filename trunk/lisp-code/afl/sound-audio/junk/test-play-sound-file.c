#ifndef lint
static char sccsid[] = "@(#)play.c 1.2 90/01/02 Copyr 1989 Sun Micro";
#endif
 /* Modified: Tue May 11 20:25:06 EDT 1993 */
/* something seems to have introduced a bug:
   play-sound-file when called from lucid no longer plays long files.
   This worked once upon a time, so backtrack through the changes */
 /* Modified: Sat Feb 20 09:17:27 EST 1993 */
/* Lucid does not handle blocking open */
 /* Modified: Thu May  6 20:12:23 EDT 1993 */
/* usleep for a few milliseconds for audio output to drain. */
/* Changing exits to returns */
/* removing sigint handler in C code */
/* compile as cc -c filename -lm -laudio */
 /* Modified: Sun Jan 10 12:59:28 EST 1993 */
/* Stripping out a function play_sound_file to call from lucid */

 /* Modified: Sun Jan 10 11:13:58 EST 1993 */
/* Adding -h option to play on the headphone port */
/* Copyright (c) 1989 by Sun Microsystems, Inc. */

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


#define	Error		(void) fprintf


/* Local variables */
char *prog;
 /* Modified: Tue May 11 15:32:40 EDT 1993 */
/* reducing buf size to 8k, used to be 64 k */
/* did not help, reverted to 64k */
unsigned char buf[1024 * 64];	/* size should depend on sample_rate */


#define	MAX_GAIN		(100)	/* maximum gain */

/*
 * This defines the tolerable sample rate error as a ratio between the
 * sample rates of the audio data and the audio device.
 */
#define	SAMPLE_RATE_THRESHOLD	(.01)


unsigned Volume = ~0;		/* output volume */
double Savevol;			/* saved volume level */
int Verbose = FALSE;		/* verbose messages */
int Immediate = FALSE;		/* don't hang waiting for */
					/* device */

char *Audio_dev = "/dev/audio";
audio_info_t info;

int Audio_fd = -1;		/* file descriptor for audio device */
Audio_hdr Dev_hdr;		/* audio header for device */
char *Ifile;			/* current filename */
Audio_hdr File_hdr;		/* audio header for file */


/* Global variables */

void 
sigint ()
{
  /* flush output queues before exiting */
  if (Audio_fd >= 0)
    {
      (void) audio_flush_play (Audio_fd);
      if (Volume != ~0)
	(void) audio_set_play_gain (Audio_fd, &Savevol);
      (void) close (Audio_fd);	/* close output */
    }
/* void return */
}

/*
 * Play an audio file. 
 */
play (intvolume, headphone, filename, immediate)
     int intvolume;
     int headphone;
     char *filename;
     int immediate;
/* headphone = 0 use speaker, 1 use headphone */
/* if immediate is non-0, then return without waiting for audio device */
 /* to become free */

{
  int i;
  int cnt;
  int err;
  int ifd;

  double vol;
  struct stat st;
  struct sigvec vec;


  /* Validate and open the audio device */
  err = stat (Audio_dev, &st);
  if (err < 0)
    {
      Error (stderr, "%s: cannot stat ", prog);
      perror (Audio_dev);
      return (1);
    }
  if (!S_ISCHR (st.st_mode))
    {
      Error (stderr, "%s: %s is not an audio device\n", prog,
	     Audio_dev);
      return (1);
    }
  /* Now hang until it's open */
  Audio_fd = open (Audio_dev, O_WRONLY);

  /* From lucid you need to keep trying to open the device since */
  /* it does not know about blocking opens */

  if ((Audio_fd < 0) && (errno == EINTR) && (immediate == 0))
    {
      /*loop until it is available */
      while ((Audio_fd < 0) && (errno == EINTR))
	{
	  Audio_fd = open (Audio_dev, O_WRONLY);
	}
    }
  /* open failed for some other reason, so catch it here */
  if (Audio_fd < 0)
    {
      Error (stderr,
	     "play-sound-file:   error opening \dev\audio ");
      perror (Audio_dev);
      return (1);
    }

  /* Get the device output encoding configuration */
  if (audio_get_play_config (Audio_fd, &Dev_hdr) != AUDIO_SUCCESS)
    {
      Error (stderr, " %s is not an audio device\n",
	     Audio_dev);
      return (1);
    }

  /* if headphone flag set headphone */
  if (headphone)
    {
      AUDIO_INITINFO (&info);
      info.play.port = AUDIO_HEADPHONE;
      err = ioctl (Audio_fd, AUDIO_SETINFO, &info);
      if (err < 0)
	{
	  perror ("ioctl error");
	  return (1);
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
	  return (1);
	}
    }

  /*  set the output volume now */

  vol = (double) intvolume / (double) MAX_GAIN;
  (void) audio_get_play_gain (Audio_fd, &Savevol);
  err = audio_set_play_gain (Audio_fd, &vol);
  if (err != AUDIO_SUCCESS)
    {
      Error (stderr,
	     " could not set output volume for \n");
      return (1);
    }


  /* Set up SIGINT handler to flush output */
  vec.sv_handler = sigint;
  vec.sv_mask = 0;
  vec.sv_flags = 0;
  (void) sigvec (SIGINT, &vec, (struct sigvec *) NULL);

  if ((ifd = open (filename, O_RDONLY, 0)) < 0)
    {
      Error (stderr, " cannot open ");
      perror (filename);
    }
  /* Check to make sure this is an audio file */
  err = audio_read_filehdr (ifd, &File_hdr, (char *) NULL, 0);

  if (err != AUDIO_SUCCESS)
    {
      Error (stderr, " %s is not a valid audio file\n",
	     filename);
      goto closeinput;
    }

  /* Check the device configuration */
  if (audio_cmp_hdr (&Dev_hdr, &File_hdr) != 0)
    {
      /*
         * The device does not match the input file.
         * Wait for any old output to drain, then attempt
         * to reconfigure the audio device to match the
         * input data.
       */
      if (audio_drain (Audio_fd, FALSE) != AUDIO_SUCCESS)
	{
	  Error (stderr, "play-sound-file: ");
	  perror ("AUDIO_DRAIN error");
	  return (1);
	}
      if (!reconfig ())
	goto closeinput;
    }

  /*
     * At this point, we're all ready to copy the data.
   */
  while ((cnt = read (ifd, (char *) buf, sizeof (buf))) >= 0)
    {
      /* If input EOF, write an eof marker */
      err = write (Audio_fd, (char *) buf, cnt);

      if (err != cnt)
	{
	  Error (stderr, "play-sound-file: output error:\
did not write all that was read. ");
	  perror ("");
	  break;
	}
      if (cnt == 0)
	break;
    }
  if (cnt < 0)
    {
      Error (stderr, "play-sound-file: error reading ");
      perror (filename);
    }

closeinput:
  (void) close (ifd);		/* close input file */



  /*
     * Though drain is implicit on close(), it's performed here
     * for the sake of completeness, and to ensure that the volume
     * is reset after all output is complete.
   */
  (void) audio_drain (Audio_fd, FALSE);
  if (Volume != ~0)
    {
      (void) audio_set_play_gain (Audio_fd, &Savevol);
    }

  (void) close (Audio_fd);	/* close output */
  return (0);
/*      exit(0); */
/*NOTREACHED */
}


/*
 * Try to reconfigure the audio device to match the file encoding.
 * If this fails, we should attempt to make the input data match the
 * device encoding.  For now, we give up on this file.
 *
 * Returns TRUE if successful.  Returns FALSE if not.
 */
reconfig ()
{
  int err;
  char msg[AUDIO_MAX_ENCODE_INFO];

  Dev_hdr = File_hdr;
  err = audio_set_play_config (Audio_fd, &Dev_hdr);

  switch (err)
    {
    case AUDIO_SUCCESS:
      return (TRUE);

    case AUDIO_ERR_NOEFFECT:
      /*
         * Couldn't change the device.
         * Check to see if we're nearly compatible.
         * audio_cmp_hdr() returns >0 if only sample rate difference.
       */
      if (audio_cmp_hdr (&Dev_hdr, &File_hdr) > 0)
	{
	  double ratio;

	  ratio = (double) abs ((int)
			     (Dev_hdr.sample_rate - File_hdr.sample_rate)) /
	    (double) File_hdr.sample_rate;
	  if (ratio <= SAMPLE_RATE_THRESHOLD)
	    {
	      if (Verbose)
		{
		  Error (stderr,
			 "%s: WARNING: %s sampled at %d, playing at %d\n",
			 prog, Ifile, File_hdr.sample_rate,
			 Dev_hdr.sample_rate);
		}
	      return (TRUE);
	    }
	  Error (stderr, "%s: %s sample rate %d not available\n",
		 prog, Ifile, File_hdr.sample_rate);
	  return (FALSE);
	}
      (void) audio_enc_to_str (&File_hdr, msg);
      Error (stderr, "%s: %s encoding not available: %s\n",
	     prog, Ifile, msg);
      return (FALSE);

    default:
      Error (stderr, "%s: i/o error (set config)\n");
      return (1);
/*NOTREACHED */
    }
}


/* Parse an unsigned integer */
parse_unsigned (str, dst, flag)
     char *str;
     unsigned *dst;
     char *flag;
{
  char x;

  if (sscanf (str, "%u%c", dst, &x) != 1)
    {
      Error (stderr, "%s: invalid value for %s\n", prog, flag);
      return (1);
    }
  return (0);
}

/* test 
   main()
   {
   play(1,1,"/usr/u/raman/sounds/cues/harp.au");
   play(1,0,"/usr/u/raman/sounds/cues/harp.au");
   }

 */


/* Parse an unsigned integer */
/*
   parse_unsigned(str, dst, flag)
   char         *str;
   unsigned     *dst;
   char         *flag;
   {
   char         x;

   if (sscanf(str, "%u%c", dst, &x) != 1) {
   Error(stderr, "%s: invalid value for %s\n", prog, flag);
   return (1);
   }
   return (0);
   }
 */

main ()
{
  play (1, 1, "/usr/u/raman/sounds/tunes/james.bond.au", 1);
  play (1, 0, "/usr/u/raman/sounds/tunes/james.bond.au", 0);
}
