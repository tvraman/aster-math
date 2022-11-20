;;;   -*-   Mode: LISP -*-    ;;;
;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :aster)

(export '(aster-file aster-text))

(defvar *lexer*
  (namestring  (merge-pathnames "lexer/lispify" *lisp-dir*))
  "Lexical analyser.")

(defun doc-from-stream (input)
  "Return document parsed by reading  input stream."
  (let ((p (run-program *lexer* nil :input input  :wait t :output  :stream)))
    (create-article (read (process-output p)))))

(defun aster-file (filename)
  "Aster a  Latex article "
  (let ((doc (doc-from-stream (open filename))))
    (read-aloud doc)))

(defun aster-text (latex) 
  "Aster  a Latex article passed as a string."
  (let ((doc (doc-from-stream (make-string-input-stream latex))))
    (read-aloud doc)))
