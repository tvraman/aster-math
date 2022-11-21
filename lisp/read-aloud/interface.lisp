;;;   -*-   Mode: LISP -*-    ;;;
;;; Copyright (C) 1990, 1991, 1992, 1993, 1994 by T. V. Raman
;;; All Rights Reserved

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :aster)

(export '(aster-file aster-text))

(defvar *lexer*
  (namestring  (merge-pathnames "lexer/lispify" *lisp-dir*))
  "Lexical analyser.")

(defvar *docs-cache* (make-hash-table :test #'eq)
  "Hash table of cached documents read in this session.")

(defun doc-from-stream (input)
  "Return document parsed by reading  input stream."
  (let ((p (run-program *lexer* nil :input input  :wait t :output  :stream)))
    (create-article (read (process-output p)))))

(defun aster-file (filename &key  id)
  "Aster a  Latex article.
 Cache result using id. Also create a next/previous link with document
  that is current."
  (let ((current *document*)
        (doc (doc-from-stream (open filename))))
    (when id (setf (gethash id *docs-cache* ) doc))
    (when current                       ; link next/previous:
      (setf (next current) doc)
      (setf (previous doc) current))
    (read-aloud doc)))

(defun aster-text (latex &key id)
  "Aster  a Latex article passed as a string."
  (let ((current *document*)
        (doc (doc-from-stream (make-string-input-stream latex))))
    (when id (setf (gethash id *docs-cache* ) doc))
    (when current                       ; link next/previous:
      (setf (next current) doc)
      (setf (previous doc) current))
    (read-aloud doc)))
