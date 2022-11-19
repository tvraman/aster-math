;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :aster)

(export '(parse-latex-file parse-latex-string))
;;; Variable: *LEX-PROGRAM*                                  Author: raman
;;; Created: Fri Feb 21 09:15:19 1992

;;; Function: PARSE-ARTICLE                                 Author: raman
;;; Created: Fri Feb 21 09:10:37 1992

(defun parse-latex-file (filename) 
  "Parses a Latex article "
  (format t "Lex: ~a~%" filename)
  (with-open-file (in-stream filename)
    (let ((process
            (sb-ext:run-program
             (namestring  (merge-pathnames "lexer/lispify" *lisp-dir*)) nil
             :input in-stream :wait t :output  :stream)))
      (create-article
       (read (sb-ext:process-output process) nil)))))

(defun parse-latex-string (string) 
  "Parses a Latex article passed as a string."
  (let ((process
          (sb-ext:run-program
           (namestring  (merge-pathnames "lexer/lispify" *lisp-dir*)) nil
           :input (make-string-input-stream string)
           :wait t :output  :stream)))
    (create-article (read (sb-ext:process-output process) nil))))
