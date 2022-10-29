;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Modified: Mon Feb 15 19:23:55 EST 1993
;;; Mon Feb 15 19:24:02 EST 1993
;;; Fixed the problem. I had failed to load the relevant library
;;; files, libaudio.a and libc.a
;;; <(bug  fix in mail message 6)>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; foreign function interface to play function in C not working,
;;; Even though the C function itself works.
;;; Keeping it here for the present.
;;; Sun Jan 10 14:35:58 EST 1993
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *play-sound-file-pathname*
  (concatenate 'string
               user::*lisp-code-directory* 
"/afl/sound-audio/play-sound-file.o")
  "play function in C")

(lcl:load-foreign-files *play-sound-file-pathname* (list "-laudio"
                                                         "-lc"))

(lcl:def-foreign-function (play-sound-file-internal 
                           (:name "_play")
                           (:return-type :signed-32bit))
    (intvolume :signed-32bit)
  (headphone :signed-32bit)
  (filename( :pointer  :character ))
  (immediate  :signed-32bit) 
  )

(defun malloc-foreign-string (string)
  (check-type string string)
  (let ((foreign-string
	 (lcl:malloc-foreign-pointer
	  :type `(:pointer (:array :character (,(1+ (length string))))))))
    (setf (lcl:foreign-string-value foreign-string) string)
    (setf (lcl:foreign-pointer-type foreign-string) `(:pointer :character))
    foreign-string))


  ;;; Function: PLAY-SOUND-FILE                                Author: raman
  ;;; Created: Sun Jan 10 13:39:22 1993
;;; Modified: Wed May 12 17:07:10 EDT 1993
;;; Wrapping call to play-sound-file-internal in a call to the macro
;;; without-sigalrm
;;; <( backed up old version)>
;;; <(refer to the comments at the top of sigalrm file)>
(defun play-sound-file (&key (volume 1) (port :h) (immediate nil) filename) 
  "Play sound file using a C function"
  (let
      ((port-id (if (eql :h port) 1 0))
       (immediate-flag  (if immediate  1 0)))
    (without-sigalrm
     (play-sound-file-internal  volume port-id
                                (malloc-foreign-string filename )
                                immediate-flag))
    )
  )


(lcl:load-foreign-files
(concatenate 'string 
*lisp-code-directory* 
"/afl/sound-audio/audio-open-p.o"))

(lcl:def-foreign-function (audio-open-p-internal  
                           (:name "_audio_open_p")
                           (:return-type :signed-32bit )))

(defun audio-busy-p()
  "Check if audio device busy"
  (let ((flag (audio-open-p-internal )))
    (cond
      ((= flag 0) nil)
      ((= flag 1) t)
      (t (error "~a returned by foreign function" flag ))
      )
    )
  )

