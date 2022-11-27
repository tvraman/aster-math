;;;   -*-   Mode: LISP -*-    ;;;
;;; Copyright (C) 1990, 1991, 1992, 1993, 1994 by T. V. Raman
;;; All Rights Reserved

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :aster)

(export '(file text))

(defun doc-from-stream (input)
  "Return document parsed by reading  input stream."
  (let ((p (run-program (lexer) nil :input input  :wait t :output  :stream)))
    (create-article (read (process-output p)))))

(defun insert-doc-after-current (current doc)
  "Insert doc into the prev/next link chain for current."
  (let ((cache (next current)))
    (setf (previous doc) current)
    (setf (next current) doc)
    (when cache
      (setf (next doc) cache)
      (setf (previous cache) doc))))

(defun file (filename)
  "Aster a  Latex article.
  Also create a next/previous link with document
  that is current."
  (let ((current *document*)
        (doc (doc-from-stream (open filename))))
    (when current (insert-doc-after-current current doc))
    (read-aloud doc)))

(defun text (latex &key id title)
  "Aster  a Latex article passed as a string."
  (let ((current *document*)
        (doc (doc-from-stream (make-string-input-stream latex))))
    (when title
      (setf (title doc) (format nil "~a: ~a"
                                title (or id id ""))))
    (when current (insert-doc-after-current current doc))
    (read-aloud doc)))
