;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

(export '(parse-article))
;;; Variable: *LEX-PROGRAM*                                  Author: raman
;;; Created: Fri Feb 21 09:15:19 1992

;;; Function: PARSE-ARTICLE                                 Author: raman
;;; Created: Fri Feb 21 09:10:37 1992

(defun parse-article (filename) 
  "Parses a Latex article "
  (format t "Performing lexical analysis on ~a" filename)
  (with-open-file (in-stream filename)
    (let ((process
            (run-program
             (namestring  (merge-pathnames "lexer/lispify" *lisp-code-directory*))
             nil
             :input in-stream :wait t :output  :stream)))
      (create-article
       (read
        (process-output process) nil)))))
