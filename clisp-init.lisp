;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-

(in-package :user)
;;; Init file for clisp.
;;; global variables
(setq EXT:APPEASE-CERRORS t)
(setf *print-array* t)
(setf *print-circle* t)
;;; print arrays so make-array can read it.
;;;
(setf *print-pretty* t)
;;; Some useful functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cf (&rest args) (apply #'compile-file args))

(defun cfl (&rest args) (load (apply #'compile-file args)))

(defun ld (&rest args) (apply #'load args))

(defun ql (&rest args)
  (let ((*redefinition-action* nil))
    (apply #'load args)))

;;; Variable: *LISP-CODE-DIRECTORY*                          Author: raman
;;; Created: Mon Mar 30 19:55:16 1992

(defvar *lisp-code-directory*
  "/usr/local/google/home/raman/emacs/lisp/aster/lisp-code"
  "directory under which lisp code is organized")

;;; load defsystem
(defvar *defsystem-directory*
  (concatenate 'string
               *lisp-code-directory* "/"  "lisp-utilities")
  "make for common lisp")

(load (concatenate 'string
		   *defsystem-directory*
		   "/"
		   "defsystem.lisp"))

(setf make:*compile-during-load* t)
(setf make:*central-registry*
      (concatenate 'string *lisp-code-directory*
                   "/" "system-definitions"))
(defun announce (msg) (format t "~&~a~%" msg )) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun featurep (feature) (find feature *features* ))
(setf *aster-directory* *lisp-code-directory*)
(setf *lisp-binaries* "")

(use-package :clos)
#+clisp (pushnew :express   *features*)
 ;(pushnew :emacspeak *features*)

#+lucid (pushnew :multivoice *features*)
(use-package :make)


(defun delete-feature (feature)
(setf *features* 
(delete feature *features*))
)

(load-system :read-aloud)
(afl:initialize-speech-space)
(afl:initialize-total-space)
