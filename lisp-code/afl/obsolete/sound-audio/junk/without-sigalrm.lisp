;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Wed May 12 17:10:42 EDT 1993
 ;;; Contains the macro without-sigalrm.
;;; This macro prevents lucid from sending a sigalrm to the C write
;;; function.
;;; Use this in play-sound-file.
;;; If this not done, lucid sends a sigalrm to the write function
;;; after it has written 36K.
;;;
(defmacro without-sigalrm (&body body)
  (let ((sigalrm (gensym)))
    `(let ((,sigalrm (sys:get-lisp-interrupt-handler 14)))
      (sys:setup-interrupt-handler 14 :ignore)
      (unwind-protect
	   (progn ,@body)
        (sys:setup-interrupt-handler 14 ,sigalrm)))))
