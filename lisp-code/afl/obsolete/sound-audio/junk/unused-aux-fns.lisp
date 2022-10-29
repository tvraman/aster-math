;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Mon Feb 15 20:20:29 EST 1993
;;; some auxillary sound functions that I am not using.
;;; Moved here when switching audio player to use play-sound-file
;;; These functions may come in handy later.
;;; Backing these up.
;;; { sound files
(export '( *play-cmd*))
(defvar *play-cmd* "play"  "play command")
(defparameter *play-args* '("-v" "1" "-h") "command line args to
play")
;;; For the present not using above variables since it is just as easy
;;; to change the function itself. 
                                        ;using run-program:
(proclaim '(inline play-file))
(defun play-file (filename)
  "use run-program to play file"
  (run-program   "play" 
                 :arguments  (list "-v" "1" "-h" filename)
                 :wait  t)
  )

(proclaim '(inline play-pathname))
(defun play-pathname (pathname)
  "play sound file pointed to by pathname"
  (assert (pathnamep pathname)  (pathname )
          "~a is not a pathname"
          pathname)
  (run-program "play"
               :arguments (list
                           "-v" "1" "-h"
                           (format nil "~a" pathname ))
               :wait  t)
  )

  ;;; Variable: *SOUND-DIRECTORY* Author: raman
  ;;; Created: Fri Jan  8 12:48:30 1993

(defvar *sound-directory*
  (pathname-directory (parse-namestring  "/usr/u/raman/sounds/cues/"))
  "Default directory for sounds")

  ;;; Function: MAKE-AUDIO-PATHNAME                            Author: raman
  ;;; Created: Fri Jan  8 12:46:58 1993

(defun make-audio-pathname (name &optional (directory *sound-directory*)) 
  "Make a pathname to an audio file in the default sound directory"
  (make-pathname :directory directory
                 :name name :type "au" )
  )

  ;;; Variable: *DEFAULT-SOUND-FILENAME* Author: raman
  ;;; Created: Thu Jan  7 14:25:59 1993

(defvar *default-sound-filename*
  '("/usr/u/raman/sounds/cues/soft-beep.au")
  "Default sound")


  ;;; Variable: *DEFAULT-SOUND-PATHNAME*                       Author: raman
  ;;; Created: Fri Jan  8 13:01:51 1993

(defvar *default-sound-pathname*
  (list (make-audio-pathname "soft-beep"))
  "Pathname to default sound")


  ;;; Variable: *SOUND-DIR-NAME*                               Author: raman
  ;;; Created: Fri Jan  8 13:21:15 1993
;;; external variable: 
(defvar *sound-dir-name*
  "/usr/u/raman/sounds/cues/"
  "sound directory as a string")


  ;;; Function: MAKE-AUDIO-FILENAME                            Author: raman
  ;;; Created: Fri Jan  8 13:22:08 1993

(defun make-audio-filename  (name) 
  "Make up audio filename "
  (concatenate 'string
               *sound-dir-name*
               name
               ".au")
  )
;;; }
