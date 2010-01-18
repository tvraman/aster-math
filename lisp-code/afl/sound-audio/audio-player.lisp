;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;; $Id: audio-player.lisp,v 1.1.1.1 2001/08/10 23:20:19 raman Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
;;; clisp porting note:
;;; clisp does not have processes. 
;;; Modified: Tue Mar 23 16:00:24 EST 1993
;;; { export 

;;; exporting
(export   '(
            switch-on switch-off toggle-switch
            select-sound audio-move-to
            make-audio-filename
            play-once synchronize-and-play play-repeatedly 
            initialize-audio-space
            activate-sound-audio deactivate-sound-audio
            *current-audio-state* *global-audio-state*
            *sound-dir-name*
            *sound-priority* 
            ))
(loop for dim in *audio-dimensions*  do
      (export dim ))

;;; }
;;; { comments: 

;;; Mon Feb 15 20:23:23 EST 1993
;;; Switching to using play-sound-file. <(Old version backed up. )>
;;; Thu Feb 11 10:09:48 EST 1993
;;; Moving audio-player to afl package.
;;; Thu Jan  7 13:54:08 EST 1993
;;; Non speech audio:
;;; <(Refer to notes. )>
;;; Modified: Sun Feb 21 14:21:27 EST 1993
;;; Introduced player-interval
;;; Defines an audio player as an object.
;;; An audio player has:
;;; A unique id for identification.
;;; A switch for turning it on and off.
;;;  A function that specifies  what it runs.
;;; A sound that is the argument to the above function.
;;;  The default function is to play a file,

;;; Later if we extend to csound,
;;; sound would be a command to send to csound.
;;; A process:
;;; Process is run as a separate process using the function and
;;; arguments.
;;; It is created by an after method on initialize-instance.
;;; It uses the switch, interval and the sound as special
;;; variables.

;;; User interface:
;;; make an audio player:
;;; Set its sound:
;;; Turn it on:
;;; Turn it off:
;;; Kill the player:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; }

;;; { *sound-priority*

  ;;; Variable: *SOUND-PRIORITY*                               Author: raman
  ;;; Created: Thu Jan  7 13:59:09 1993
;;; external variable: 
(defvar *sound-priority* 101  "Priority for scheduling sounds")
;;; *sound-priority* is assigned a high number. Lucid assigns a
;;; default scheduling priority of 100 to processes, and the manual
;;; looks confused as to whether high numbers mean high priority, but
;;; simple tests indicate that high numbers mean lower priority.
;;; Setting priority to a high number for sound players will allow the
;;; reader to run without introducing irritating pauses in the speech.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; }
;;; { run-program-play-file --lisp specific-- 

(defvar *play-program* "play"
  "Play executable ")


                                        ;;; using run-program:

(proclaim '(inline run-program-play-file))
#+(and lucid sparc) 
(defun run-program-play-file (&key (volume 1) (port  :h)
                                   (immediate nil) (wait nil)
                                   (filename *default-sound-filename*))
  "use run-program to play file"
  (declare (optimize(compilation-speed 0) (speed 3 ) (safety 1 )))
  (cond
    ((and immediate (audio-busy-p )) nil)
    (t (run-program   "play" 
                      :arguments (remove nil  (list "-v" (format nil "~a" volume)
                                                    (case port
                                                      (:h "-h" )
                                                      (:s nil))
                                                    (if immediate "-i"   nil )
                                                    filename))
                      :wait  wait))
    )
  )

#+lucid
;;; this function needs cleaning,
;;; it depends too closely on the command line interface used by *play-program*
(defun run-program-play-file (&key (volume 1) 
                                   (immediate nil) (wait nil)
                                   (filename *default-sound-filename*))
  "use run-program to play file"
  (declare (optimize(compilation-speed 0) (speed 3 ) (safety 1 )))
     (run-program    *play-program* 
                      :arguments
                      (remove nil  (list "-v" (format nil "~a" volume)
                                                    (if immediate "-i"   nil )
                                                    filename))
                      :wait  wait))
#+clisp
;;; clisp does not know about :wait 
(defun run-program-play-file (&key (volume 1) 
                                   (immediate nil) (wait nil)
                                   (filename *default-sound-filename*))
  "use run-program to play file"
  (declare (optimize(compilation-speed 0) (speed 3 ) (safety 1 )))
  (run-program    *play-program* 
                  :arguments
                  (remove nil  (list "-v" (format nil "~a" volume)
                                     filename ))
                  )
  )

;;; }
;;; { sound files

  ;;; Variable: *SOUND-DIR-NAME*                               Author: raman
  ;;; Created: Fri Jan  8 13:21:15 1993
;;; external variable: 
(defvar *sound-dir-name*
  (concatenate 'string user::*lisp-code-directory* "/"
               "sounds" )
  "sound directory as a string")

(defvar *default-sound-filename*
  (afl::make-audio-filename "soft-beep")
  "Default sound")

  ;;; Function: MAKE-AUDIO-FILENAME                            Author: raman
  ;;; Created: Fri Jan  8 13:22:08 1993

(defun make-audio-filename  (name &key (directory *sound-dir-name*))
  "Make up audio filename "
  (concatenate 'string
               directory  "/"
               name
               ".au")
  )

;;; }
;;; { misc variables for audio player

  ;;; Variable: *AUDIO-PLAYER-ID*                              Author: raman
  ;;; Created: Thu Jan  7 14:06:20 1993

(defvar *audio-player-id*  0 "Id for next audio player")


  ;;; Variable: *DEFAULT-SOUND*                                Author: raman
  ;;; Created: Tue Feb 16 09:14:58 1993

(defvar *default-sound*
  (list :filename
        (concatenate 'string
                     user::*lisp-code-directory* "/" "sounds/soft-beep"))
  "Default argument to play-sound-file")

  ;;; Parameter: *PLAY-FUNCTION*                               Author: raman
  ;;; Created: Wed Feb 17 09:40:35 1993

(defparameter *play-function*
  #'run-program-play-file
  "play function used by player")

(defvar *max-player-interval* 10 "maximum interval for player")
(deftype player-interval () `(float  0 (,*max-player-interval* )))


  ;;; Variable: *AUDIO-PLAYER-INTERVAL*                        Author: raman
  ;;; Created: Sun Apr 11 11:19:48 1993



(defvar *audio-player-interval* 0  "default interval")

;;; }

;;; { class audio-player --lisp specific-- 

(use-package :clos)
  ;;; Class: AUDIO-PLAYER                                      Author: raman
  ;;; Created: Thu Jan  7 14:01:55 1993
#+lucid 
(defclass audio-player ()
  ((id :initform (incf *audio-player-id* )
       :initarg :id :accessor audio-player-id)
   (switch :initform nil :initarg :switch
           :accessor audio-player-switch :accessor switch)
   (interval :initform *audio-player-interval*    :initarg :interval :type player-interval
             :accessor audio-player-interval :accessor interval)
   (sound :initform *default-sound*  :initarg :sound
          :accessor audio-player-sound :accessor player-sound :accessor sound)
   (function :initform *play-function*  :initarg :function
             :accessor audio-player-function :accessor player-function)
   (process :initform nil :initarg :process :accessor audio-player-process))
  (:documentation "An audio player"))

#+clisp
;;; clisp does not have processes 
(defclass audio-player ()
  ((id :initform (incf *audio-player-id* )
       :initarg :id :accessor audio-player-id)
   (switch :initform nil :initarg :switch
           :accessor audio-player-switch :accessor switch)
   (interval :initform *audio-player-interval*    :initarg :interval :type player-interval
             :accessor audio-player-interval :accessor interval)
   (sound :initform *default-sound*  :initarg :sound
          :accessor audio-player-sound :accessor player-sound :accessor sound)
   (function :initform *play-function*  :initarg :function
             :accessor audio-player-function :accessor player-function)
   ;(process :initform nil :initarg :process :accessor audio-player-process)
   )
  (:documentation "An audio player"))
(defun make-audio-player ()
  (let ((self (make-instance 'audio-player)))
    self))

(proclaim '(inline audio-player-p))

(defun audio-player-p (self)
  (typep self 'audio-player))

;;; Default print function for audio player:
;;; <(same as for document class )>
(defmethod print-object ((x audio-player) s)
  (let* ((class (class-of x))
	 (slots (class-slots class))
	 (l (mapcar #'(lambda (slot)
			(let* ((name (slot-definition-name slot))
			       (boundp (slot-boundp x name))
			       (val (and boundp
					 (slot-value x name))))
			  (list name boundp val)))
		    slots)))
    (format s "[[~s~%~:{ ~s:~20t~:[<unbound>~;~s~]~%~}]]"
	    (class-name class) l)))

;;; After the sound and switch slots have been created and assigned,
;;; we can generate the process object encapsulating these. This done
;;;  in an after method on initialize-instance


  ;;; Method: INITIALIZE-INSTANCE                              Author: raman
  ;;; Created: Thu Jan  7 14:13:38 1993
;;; Modified: Tue May 11 15:58:32 EDT 1993
;;; Using process-wait after discussing with jar.
;;;  <(old version used sleep in the loop)>
;;; process-wait needs to be thought about, returning to using sleep
;;; Restored from backup pointed to by above button
;;; for the present.
;;; For some reason it does not wake when the switch is turned,
;;; probably because the switch is present in the other process.
;;; Trying to use process-wait:
#+(and lucid sparc)
;;; audio-busy-p defined for sparc
(defmethod initialize-instance
    :after  ((audio-player audio-player) &rest initargs )
    "After method: Sets up process for audio player"
    (declare (ignore initargs ))
    (setf (audio-player-process audio-player )
          (make-process
           :name (format nil "audio-player-~d" (audio-player-id audio-player ))
           :priority *sound-priority*
           :function
           #'(lambda()
               (loop
                (process-wait "Waiting for switch and free device. "
                              #'(lambda(player)
                                  (and (switch player)
                                       (not (audio-busy-p ))))
                              audio-player)
                (apply (player-function audio-player)
                       (sound audio-player ))
                (process-allow-schedule) ;safety
                (unless (zerop (interval audio-player))
                  (sleep (interval  audio-player )))))
           ))
    )
#+lucid
;;; audio-busy-p not defined for lucid on all platforms 
(defmethod initialize-instance
  :after  ((audio-player audio-player) &rest initargs )
  "After method: Sets up process for audio player"
  (declare (ignore initargs ))
  (setf (audio-player-process audio-player )
        (make-process
         :name (format nil "audio-player-~d" (audio-player-id audio-player ))
         :priority *sound-priority*
         :function
         #'(lambda()
             (loop
              (process-wait "Waiting for switch and free device. "
                            #'(lambda(player)
                                (switch player))
                            audio-player)
              (apply (player-function audio-player)
                     (sound audio-player ))
              (process-allow-schedule)  ;safety
              (unless (zerop (interval audio-player))
                (sleep (interval  audio-player )))))
         ))
  )

;;; }
;;; { code using processes --lucid specific-- clisp has dummy functions

  ;;; Method: ACTIVATE-PLAYER                                  Author: raman
  ;;; Created: Thu Jan  7 14:34:13 1993
#+lucid 
(defmethod activate-player ((audio-player audio-player))
  "Activate this player"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (activate-process (audio-player-process audio-player ))
  )
#+clisp
;;; dummy:
(defmethod activate-player ((audio-player audio-player))
  "Activate this player"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  nil 
  )

  ;;; Method: DEACTIVATE-AUDIO-PLAYER                          Author: raman
  ;;; Created: Thu Jan  7 14:35:08 1993
#+lucid 
(defmethod deactivate-player ((audio-player audio-player))
  "Deactivate this player"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (deactivate-process (audio-player-process audio-player))
  )

#+clisp
;;; dummy:
(defmethod deactivate-player ((audio-player audio-player))
  "Deactivate this player"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  nil
  )

  ;;; Method: REMOVE-PLAYER                                    Author: raman
  ;;; Created: Thu Jan  7 14:36:06 1993
#+lucid 
(defmethod remove-player ((audio-player audio-player))
  "Kill this player"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (kill-process (audio-player-process audio-player ))
  )
#+clisp
;;; dummy
(defmethod remove-player ((audio-player audio-player))
  "Kill this player"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  nil
  )

(proclaim '(inline activate-sound-audio ))
#+lucid 
(defun activate-sound-audio ()
  "Activate sound audio hardware if necessary. "
  (when (eql (audio-player-state *audio-player* ) :inactive)
    (activate-player *audio-player*))
  )
#+clisp
;;; dummy
(defun activate-sound-audio ()
  "Activate sound audio hardware if necessary. "
  nil
  )

(proclaim '(inline deactivate-sound-audio))
#+lucid 
(defun deactivate-sound-audio ()
  "Deactivate sound audio"
  (when *audio-player* 
  (deactivate-player *audio-player*))
  )

#+clisp
;;; dummy
(defun deactivate-sound-audio ()
  "Deactivate sound audio"
  nil 
  )
#+lucid 
(defmethod kill-player ((audio-player audio-player))
  "Kill this player"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (kill-process (audio-player-process audio-player ))
  )
#+clisp
;;; dummy
(defmethod kill-player ((audio-player audio-player))
  "Kill this player"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  nil
  )

  ;;; Method: AUDIO-PLAYER-STATE                               Author: raman
  ;;; Created: Thu Jan  7 14:38:14 1993
#+lucid 
(defmethod audio-player-state ((audio-player audio-player))
  "State of audio player "
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (process-state (audio-player-process audio-player ))
  )

#+clisp
;;; dummy
(defmethod audio-player-state ((audio-player audio-player))
  "State of audio player "
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  nil
  )

;;; }
;;; { generic sound audio code

  ;;; Method: SWITCH-ON                                        Author: raman
  ;;; Created: Thu Jan  7 14:39:21 1993

(defmethod switch-on ((audio-player audio-player) &key (synchronize nil))
  "Turn on audio player,  synchronously by default"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (when synchronize (await-silence ))
  (setf (switch audio-player) t)
  )

(defmethod switch-off ((audio-player audio-player) &key(synchronize nil))
  "Turn off  audio player, synchronously by default"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (when synchronize (await-silence ))
  (setf (switch audio-player) nil )
  )


;;; Function: SYNCHRONIZE-AND-PLAY                           Author: raman
;;; Created: Mon Apr 27 12:30:49 1992
;;; Modified: Thu Jan  7 16:38:31 EST 1993
;;; Using run-program isntead of shell
(proclaim '(inline synchronize-and-play))
(defun synchronize-and-play (soundfile &key(background-flag  nil)) 
  "wait for dectalk to stop talking and then play soundfile."
  (declare  (optimize (compilation-speed 0) (safety 1) (speed 3)))
  (unless (or (switch *audio-player*)
              dectalk:*dribbling-to-file*)
    (await-silence )
    (when *play-program*
      (user::run-program *play-program* 
                         :arguments (list
                                     soundfile)
                         )
      )
    )
  )
  ;;; Function: PLAY-REPEATEDLY                                Author: raman
  ;;; Created: Mon Oct 25 16:26:38 1993

(defun play-repeatedly (soundfile count
&key (synchronize-flag t)
                                   (background-flag nil)) 
  "Play soundfile count times"
  (let ((soundfiles (loop for i from 1 to count
                          collect soundfile )))
    (when synchronize-flag
    (await-silence))
    (user::run-program "play"
                       :arguments   
`(,@soundfiles)
                           ;:wait   nil
                           )
    )
  )

(defun synchronize-and-multi-play (&rest soundfiles)
  "wait for dectalk to stop talking and then play multiple soundfiles."
  (declare  (optimize (compilation-speed 0) (safety 1) (speed 3)))
  (let
      ((play-args
        `("-v"  "1" "-h" "-i" 
          ,@soundfiles)))
    (await-silence )
    (user::run-program "play"
                       :arguments   play-args 
                       ;:wait  nil
                       ))
  )

  ;;; Method: PLAY-ONCE                                        Author: raman
  ;;; Created: Thu Jan  7 14:58:21 1993

(defmethod play-once ((audio-player audio-player))
  "Play the sound once"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (with-slots
      ((audio-player-function function )
       (audio-player-sound sound )) audio-player 
       (apply audio-player-function  audio-player-sound)
       )
  (values)  )

  ;;; Method: TOGGLE-SWITCH                                    Author: raman
  ;;; Created: Fri Jan  8 15:20:02 1993

(defmethod toggle-switch ((audio-player audio-player) &key(synchronize nil))
  "Toggle switch, synchronously by default"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (when synchronize (await-silence ))
  (setf (switch audio-player) (not (switch audio-player )))
  )

;;; }
;;; { setting up points in sound space 

  ;;; Structure: POINT-IN-AUDIO-SPACE                          Author: raman
  ;;; Created: Thu Feb 11 19:31:22 1993
#|
(defstruct (point-in-audio-space)
  (id nil)
  (switch nil)
  (port nil)
  (volume nil)
  (sound-name  nil )
  )
|#
  ;;; Variable: *AUDIO-PLAYER*                                 Author: raman
  ;;; Created: Thu Jan  7 15:12:16 1993

(defvar *audio-player* nil "Audio player used to read")

(proclaim '(inline audio-player))
(defun audio-player () *audio-player* )

  ;;; Variable: *GLOBAL-AUDIO-STATE*                           Author: raman
  ;;; Created: Fri Feb 12 17:02:37 1993

(defvar *global-audio-state* nil "global state of audio")

  ;;; Variable: *CURRENT-AUDIO-STATE*                          Author: raman
  ;;; Created: Thu Feb 11 19:49:10 1993

(defvar *current-audio-state* nil "Current audio state")


  ;;; Function: CREATE-POINT-IN-AUDIO-SPACE                    Author: raman
  ;;; Created: Thu Feb 11 20:17:48 1993
(proclaim '(inline create-point-in-audio-space))
(defun create-point-in-audio-space () 
  "Create a point in audio space and initialize it"
  (make-point-in-audio-space)
  )
  ;;; Function: INITIALIZE-AUDIO-SPACE                         Author: raman
  ;;; Created: Thu Feb 11 20:25:14 1993

(defun initialize-audio-space () 
  "Initialize audio space"
  (setf *current-audio-state* (create-point-in-audio-space))
  (setf *global-audio-state* *current-audio-state* )
  (when (or  (null *audio-player*)
             (eql :killed (audio-player-state *audio-player*  )))
    (setf *audio-player* (make-instance 'audio-player )))
  (set-audio-state *current-audio-state*) ;side-effect  audio player
  (setf (point-in-audio-space-id *current-audio-state*)
        (audio-player-id *audio-player* ))
  (afl::activate-sound-audio)
  *current-audio-state*)

;;; }
;;; { methods for points in sound space

  ;;; Method: SELECT-SOUND                                     Author: raman
  ;;; Created: Fri Feb 12 09:21:06 1993

(defmethod select-sound ((point point-in-audio-space) (sound string))
  "Return point with this sound selected"
  (let ((new-point (copy-point-in-audio-space point )))
    (setf (point-in-audio-space-sound-name  new-point)
          (make-audio-filename sound ))
    new-point)
  )

(defmethod select-sound ((sound string) (point point-in-audio-space))
  "Return point with this sound selected"
  (let ((new-point (copy-point-in-audio-space point )))
    (setf (point-in-audio-space-sound-name new-point)
          (make-audio-filename sound ))
    new-point)
  )

;;; these methods are also present for the audio player object.
;;; Defining analogous methods for point-in-audio-space
;;; Fri Feb 12 09:29:27 EST 1993

(defmethod switch-on ((point point-in-audio-space) &key (synchronize nil))
  "Turn on audio player,  synchronously by default"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (declare (ignore synchronize))
  (let ((new-point (copy-point-in-audio-space point )))
    (setf (point-in-audio-space-switch new-point) t)
    new-point)
  )

(defmethod switch-off ((point point-in-audio-space)  &key(synchronize nil))
  "Turn off  audio player, synchronously by default"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (declare (ignore synchronize))
  (let ((new-point (copy-point-in-audio-space point )))
    (setf (point-in-audio-space-switch new-point) nil )
    new-point)
  )

  ;;; Method: TOGGLE-SWITCH                                    Author: raman
  ;;; Created: Fri Jan  8 15:20:02 1993

(defmethod toggle-switch ((point point-in-audio-space) &key(synchronize nil))
  "Toggle switch, synchronously by default"
  (declare (optimize (compilation-speed 0) (speed 1) (safety 2)))
  (declare (ignore synchronize))
  (let ((new-point (copy-point-in-audio-space point )))
    (setf (point-in-audio-space-switch new-point)
          (not (reference-value
                (point-in-audio-space-switch point ))))
    new-point)
  )


  ;;; Function: AUDIO-POINT-TO-SOUND                           Author: raman
  ;;; Created: Tue Feb 16 10:11:26 1993
(proclaim '(inline audio-point-to-sound))
(defun audio-point-to-sound (point) 
  "Return a list appropriate for play-sound-file from point"
  (assert (point-in-audio-space-p point) nil
          "~a is not a point in audio space" point)
  (list
   :filename (reference-value
              (point-in-audio-space-sound-name point))
   :volume (reference-value
            (point-in-audio-space-volume point))
   :port (reference-value
          (point-in-audio-space-port point )))
  )

(defmethod play-once ((point point-in-audio-space ))
  "Play the sound once"
  (let
      ((audio-player-function (audio-player-function *audio-player* ))
       (sound(audio-point-to-sound point ))
       (switch (audio-player-switch *audio-player* )))
    (unwind-protect
         (progn 
           (and switch
                (switch-off *audio-player* ))
           (apply audio-player-function  sound))
      (setf (audio-player-switch *audio-player*) switch )))
  (values)  )

;;; }
;;; { generic afl code for sound audio. 

;;; contains afl operators and associated helper functions 
  ;;; Function: DIFFERENT-AUDIO-POINTS-P                           Author: raman
  ;;; Created: Tue Mar 23 14:56:53 1993

(defun different-audio-points-p (old-point new-point) 
  "Compare points, return t if different"
  (flet
      ((different-values (dim-1 dim-2)
         (not (equal
               (reference-value dim-1)
               (reference-value dim-2  )))
         ))
    (cond
      ((null old-point ) t)
      ((null new-point) t )
      (t (assert
          (and (point-in-audio-space-p old-point)
               (point-in-audio-space-p new-point)) nil
               "One of ~a or ~a is not a point in audio space."
               old-point new-point )
         (loop for dimension in *audio-dimensions*
               thereis(different-values
                       (audio-point-accessor old-point dimension)
                       (audio-point-accessor new-point dimension )))
         )
      ))
  )



  ;;; Variable: *AUDIO-HARDWARE-STATE*                         Author: raman
  ;;; Created: Tue Mar 23 14:51:33 1993

(defvar *audio-hardware-state* nil "State of audio hardware")


  ;;; Variable: *LAZY-AUDIO-SET*                               Author: raman
  ;;; Created: Tue Mar 23 16:27:16 1993

(defvar *lazy-audio-set* nil "If t then set-audio-state always sets ")

  ;;; Parameter: *DECTALK-IS-ON*                               Author: raman
  ;;; Created: Sun Jul 31 21:33:22 1994

(defparameter *dectalk-is-on* nil "Says if dectalk connected and turned on")
  ;;; Function: SET-AUDIO-STATE                                Author: raman
  ;;; Created: Fri Feb 12 09:45:18 1993

(defun set-audio-state (point) 
  "Set state of audio player as given by point"
  (assert (point-in-audio-space-p point) nil
          "~a is not a point in audio space. " point)
  (when(or *lazy-audio-set*
           (different-audio-points-p point *audio-hardware-state*))
                                        ;      (format t  "Changing state ~%")
    (let
        ((switch (reference-value (point-in-audio-space-switch point )))
         (interval  (reference-value (point-in-audio-space-interval  point )))
         (sound (audio-point-to-sound point )))
      (when *dectalk-is-on*
        (await-silence))
      (setf (audio-player-switch *audio-player*) switch)
      (setf ( interval *audio-player*) interval)
      (setf (audio-player-sound *audio-player*) sound)
      (setf *audio-hardware-state* point)
                                        ;    (format t "Switch is now ~a. ~%" (audio-player-switch *audio-player*) )
      (values )))
  )


  ;;; Method: LOCAL-SET-STATE                                  Author: raman
  ;;; Created: Fri Feb 12 09:54:05 1993

(defmethod local-set-state ((point point-in-audio-space))
  "Set local state of audio "
  (setf *current-audio-state* point)
  (setf (total-audio-state-sound  *current-total-audio-state*) point)
  (set-audio-state *current-audio-state* )
  )


  ;;; Method: GLOBAL-SET-STATE                                 Author: raman
  ;;; Created: Fri Feb 12 14:04:16 1993

(defmethod global-set-state ((point point-in-audio-space))
  "set global audio state"
  (loop for dim in *audio-dimensions* do
        (set-audio-global-value
         dim (reference-value (audio-point-accessor point  dim ))))
  (set-audio-state *current-audio-state*)
  )

(defmethod move-to ((point point-in-audio-space) dimension value
                    &key &allow-other-keys)
  "Move to a new value along dimension. "
  (assert (find dimension *audio-dimensions*) nil
          "~a is not a valid sound space dimension. "
          dimension) 
  (let
      ((new-point (copy-point-in-audio-space point  )))
    (setf (audio-point-accessor new-point dimension) value)
    new-point)
  )

;;; }
