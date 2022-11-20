;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :aster)

(export '(aster-file aster-text))

;;; Function: PARSE-ARTICLE                                 Author: raman
;;; Created: Fri Feb 21 09:10:37 1992

(defun aster-file (filename)
  "Aster a  Latex article "
  (let ((process
          (run-program
            (namestring  (merge-pathnames "lexer/lispify" *lisp-dir*)) nil
            :input (open filename) :wait t :output  :stream)))
    (read-aloud (create-article (read (process-output process))))))

(defun aster-text (latex-string) 
  "Aster  a Latex article passed as a string."
  (let ((process
          (run-program
           (namestring  (merge-pathnames "lexer/lispify" *lisp-dir*)) nil
           :input (make-string-input-stream latex-string)
           :wait t :output  :stream)))
    (read-aloud (create-article (read (process-output process))))))
