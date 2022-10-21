;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(export '(parse-article))
;;; CREATED:  FRI:  Feb 21 09:07:40 EST 1992
;;;  Interface lisp parser to Lex lexical analyser.
;;; uses run-program.
;;; This file will also include code for partial parsing of a article
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Variable: *LEX-DIR*                                      Author: raman
;;; Created: Fri Feb 21 09:14:15 1992
;;; external variable: 
(defvar *lex-dir* nil "Directory where the lexer resides")
;;; Now set it up:
(setf *lex-dir* (merge-pathnames "lexer/" *lisp-code-directory* ))


;;; Variable: *LEX-PROGRAM*                                  Author: raman
;;; Created: Fri Feb 21 09:15:19 1992
;;; external variable: 
(defvar *lex-program* nil "The program which does the lexical analysis")
;;; Now set it up:

(setf *lex-program* "lispify")




;;; Function: PARSE-ARTICLE                                 Author: raman
;;; Created: Fri Feb 21 09:10:37 1992

(defun parse-article (filename) 
  "Parses a Latex article "
  (print "Performing lexical analysis")
  (with-open-stream
      (in-stream (ext:run-program
                  (concatenate'string
                   *lex-dir* "/" *lex-program*)
                  :input filename
                  :output  :stream
                  ))
    (create-article (read in-stream nil))))
;;; lucid needs a :wait argument
#+lucid 
(defun parse-article (filename) 
  "Parses a Latex article "
  (print "Performing lexical analysis")
  (with-open-stream
      (in-stream (user:run-program
                  (concatenate'string
                   *lex-dir*
                   "/"
                   *lex-program*)
                  :input filename
                  :output  :stream
                  :wait nil 
                  ))
    (create-article
     (read in-stream nil))
    )
  )

