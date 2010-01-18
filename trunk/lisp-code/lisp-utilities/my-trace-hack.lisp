;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'user)

(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))
;;; Contains a trace hack for audio formatting output.


;;; Variable: *MY-TRACING-SWITCH*                            Author: raman
;;; Created: Mon Nov  2 09:40:35 1992

(defvar *my-tracing-switch* nil "Toggle my trace")


;;; Macro: MY-AUDIO-TRACE                                    Author: raman
;;; Created: Mon Nov  2 09:40:56 1992

(defmacro my-audio-trace (&body body) 
  "evaluate body, audio format result and return result"
  `(progn
    (let
        ((result  nil))
      (setf result
            (progn ,@body))
      (read-aloud result)
      result) 
    ))

