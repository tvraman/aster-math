;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;   Copyright (c) 1988 Cornell Apprentice Project, Cornell University   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'user)

(provide 'mexp)

;;; Macroexpand top-level loop. Expands one-level of macros. If the
;;; optional argument is given, will expand all levels of macros. 

;;; Written by Alberto Segre (1988).

(defun mexp (&optional fullexpand?)
  (prog (expr)
     (format t "~&Macroexpand top-level loop (:q or :a to quit).~%")
     loop 
     (format t "~%~A> " (cond (fullexpand? "mexp*")
			      (t "mexp")))
     (setq expr (read))
     (case expr
       ((:q :a) (return))
       (t (cond (fullexpand? (pprint (macroexpand expr)))
		(t (pprint (macroexpand-1 expr))))
	  (format t "~%")
	  (go loop))))
  (format t "~%"))
