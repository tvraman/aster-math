;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package 'afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Sat Jan  9 11:07:27 EST 1993
;;; Since the csound interface from lisp is such a pain, using the
;;; hacked playnotes function that I wrote at PARC in summer 91.
;;; This function was used in the program notes, there it was called
;;; play_notes.
;;; I am renaming the C function playnotes and introducing a foreign
;;; function interface to it.
;;; <(Refer to C code)>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Foreign function interface written by using the code in Jim Davis'
;;; compatibility package as an example.
;;; <(refer to serial-stream.lisp )>
;;; Also refer to Lucid manual
;;; <(beyondlisp.tex )>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun malloc-foreign-string (string)
  (check-type string string)
  (let ((foreign-string
	 (lcl:malloc-foreign-pointer
	  :type `(:pointer (:array :character (,(1+ (length string))))))))
    (setf (lcl:foreign-string-value foreign-string) string)
    (setf (lcl:foreign-pointer-type foreign-string) `(:pointer :character))
    foreign-string))




(defvar *playnotes-pathname*
  (concatenate 'string
               *lisp-code-directory*
               "/afl/sound-audio/playnotes.o"))

(defvar *playnotes-loaded* nil "t if playnotes loaded")

(lcl:load-foreign-files *playnotes-pathname*)

(defun load-playnotes-if-not-loaded()
  (unless *playnotes-loaded*
    (lcl:load-foreign-files *playnotes-pathname*)
    (setf *playnotes-loaded* t))
  )

(lcl:def-foreign-function (play-notes-internal (:name "_playnotes")
                                               (:return-type :signed-32bit))
    (volume :double-float)
  (length :signed-32bit )
  (tone :signed-32bit)
  (decay :double-float)
  (octave ( :pointer :character))
  )

(defun play-notes (&key (volume 15.0)
                        (length 10)
                        (tone 0)
                        (decay 0.01)
                        (octave "5c"))
  (play-notes-internal volume length tone decay
                       (malloc-foreign-string octave ))
  )


