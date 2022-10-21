;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Wed May 12 16:48:15 EDT 1993
 ;;; Contains some code sent by sayuri@lucid.
;;; Fixing problem where lucid sends an interrupt signal to a C write
;;; function call that is writing block sizes greater than 36K.
;;;

;;; Extract from message:
;;; 
;;; Please define the following macro without-sigalrm and call the
;;; play-sound-file-internal within a macro form such as:
;;; 
;;; 	(without-sigalrm (play-sound-file-internal ....))
;;; 
;;; and see if it helps. 
;;; 
;;; (defmacro without-sigalrm (&body body)
;;;   (let ((sigalrm (gensym)))
;;;     `(let ((,sigalrm (sys:get-lisp-interrupt-handler 14)))
;;;        (sys:setup-interrupt-handler 14 :ignore)
;;;        (unwind-protect
;;; 	   (progn ,@body)
;;; 	 (sys:setup-interrupt-handler 14 ,sigalrm)))))
;;; 
;;; 

;;; Trying it:

(defmacro without-sigalrm (&body body)
  (let ((sigalrm (gensym)))
    `(let ((,sigalrm (sys:get-lisp-interrupt-handler 14)))
      (sys:setup-interrupt-handler 14 :ignore)
      (unwind-protect
	   (progn ,@body)
        (sys:setup-interrupt-handler 14 ,sigalrm)))))


(defun new-play-sound-file (&key (volume 1) (port :h) (immediate nil) filename)
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

;;; Backing up old version.
;;; Above function that uses without-sigalrm works, so making it the
;;; current function.
(defun play-sound-file (&key (volume 1) (port :h) (immediate nil) filename) 
  "Play sound file using a C function"
  (let
      ((port-id (if (eql :h port) 1 0))
       (immediate-flag  (if immediate  1 0)))
    (play-sound-file-internal  volume port-id
                               (malloc-foreign-string filename )
                               immediate-flag)
    )
  )
