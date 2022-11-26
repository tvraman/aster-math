;;;   -*-   Mode: LISP -*-    ;;;
;;; Copyright (C) 1990, 1991, 1992, 1993, 1994 by T. V. Raman
;;; All Rights Reserved

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :aster)

(export '(file text))

(defvar *lexer*
  (namestring
   (merge-pathnames
    "lexer/lispify"
    (namestring (uiop:pathname-directory-pathname   #.   *load-truename*))))
  "Lexical analyser.")

(defvar *docs-cache* (make-hash-table :test #'eq)
  "Hash table of cached documents read in this session.")

(defun doc-from-stream (input)
  "Return document parsed by reading  input stream."
  (let ((p (run-program *lexer* nil :input input  :wait t :output  :stream)))
    (create-article (read (process-output p)))))

(defun insert-doc-after-current (current doc)
  "Insert doc into the prev/next link chain for current."
  (let ((cache (next current)))
    (setf (previous doc) current)
    (setf (next current) doc)
    (when cache
      (setf (next doc) cache)
      (setf (previous cache) doc))))

(defun file (filename &key  id)
  "Aster a  Latex article.
 Cache result using id. Also create a next/previous link with document
  that is current."
  (let ((current *document*)
        (doc (doc-from-stream (open filename))))
    (when id (setf (gethash id *docs-cache* ) doc))
    (when current (insert-doc-after-current current doc))
    (read-aloud doc)))

(defun text (latex &key id title)
  "Aster  a Latex article passed as a string."
  (let ((current *document*)
        (doc (doc-from-stream (make-string-input-stream latex))))
    (when id (setf (gethash id *docs-cache* ) doc))
    (when title
      (setf (title doc) (format nil "~a: ~a"
                                title (or id id ""))))
    (when current (insert-doc-after-current current doc))
    (read-aloud doc)))
