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
  (let* ((doc nil)
         (process
           (run-program
            (namestring  (merge-pathnames "lexer/lispify" *lisp-dir*)) nil
            :input (open filename) :wait t :output  :stream))
         (output (process-output process)))
    (setq doc (create-article (read output)))
    
    (read-aloud doc)))

(defun aster-text (latex-string) 
  "Aster  a Latex article passed as a string."
  (let ((doc nil)
        (process
          (sb-ext:run-program
           (namestring  (merge-pathnames "lexer/lispify" *lisp-dir*)) nil
           :input (make-string-input-stream latex-string)
           :wait t :output  :stream)))
    (setq doc (create-article (read (sb-ext:process-output process))))
    (read-aloud doc)))
