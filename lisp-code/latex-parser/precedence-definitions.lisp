;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Contains precedence declarations for operators
;;; The declarations for  the binary operators  will themselves be 
;;; generated by a lisp  function. 
;;; { automatic generator: 

;;; Function: GENERATE-PRECEDENCE-DECLARATIONS               Author: raman
;;; Created: Thu Oct 29 10:21:52 1992
#+clisp 
(defun generate-precedence-declarations () 
  "Generate precedence declarations to a file"
  (let
      ((filename (progn
                   (print "Enter filename: ")
                   (read ))))
    (with-open-file 
        (out-stream filename :direction :output)
      (loop for value being the hash-values of *math-classification-table*
            using (hash-key math-symbol)
            when (and (equal 'binary-operator  value)
                      (not (precedence-defined-p math-symbol )))
            do
            (format t   "Define precedence for ~a y or n?"  math-symbol)
            (when  (equal 'y (read))
              (format t "Define precedence for ~a to be same as: " math-symbol)
              (format out-stream
                      "~& (define-precedence  \"~a\"  :same-as \"~a\"  ) ~%"
                      math-symbol 
                      (read))
              ))))
  )
#+lucid
(defun generate-precedence-declarations () 
  "Generate precedence declarations to a file"
  (let
      ((filename (progn
                   (print "Enter filename: ")
                   (read ))))
    (with-open-file 
        (out-stream filename :direction :output)
      (loop for value being the hash-values of *math-classification-table*
            using (key math-symbol)
            when (and (equal 'binary-operator  value)
                      (not (precedence-defined-p math-symbol )))
            do
            (format t   "Define precedence for ~a y or n?"  math-symbol)
            (when  (equal 'y (read))
              (format t "Define precedence for ~a to be same as: " math-symbol)
              (format out-stream
                      "~& (define-precedence  \"~a\"  :same-as \"~a\"  ) ~%"
                      math-symbol 
                      (read))
              ))))
  )

;;; }
;;; { precedence for binary operators. 

;;; First setup table by calling setup function.
;;; This defines precedence for a large number of operators.
(setup-precedence-table)
;;; Following generated by calling above function and yanking result:
(define-precedence "land"  :same-as 'logical-and)
(define-precedence "lor" :same-as  'logical-or) 
(define-precedence  "sqcap"  :same-as "land"  ) 
(define-precedence  "bigcirc"  :same-as "*"  ) 
(define-precedence  "bmod"  :same-as "+"  ) 
(define-precedence  "vee"  :same-as "lor"  ) 
(define-precedence  "times"  :same-as "*"  ) 
(define-precedence  "cup"  :same-as "lor"  ) 
(define-precedence  "ast"  :same-as "*"  ) 
(define-precedence  "otimes"  :same-as "*"  ) 
(define-precedence  "div"  :same-as "/"  ) 
(define-precedence  "mp"  :same-as "+"  ) 
(define-precedence  "ominus"  :same-as "+"  ) 
(define-precedence  "wedge"  :same-as "land"  ) 
(define-precedence  "star"  :same-as "*"  ) 
(define-precedence  "pmod"  :same-as "+"  ) 
(define-precedence  "sqcup"  :same-as "lor"  ) 
(define-precedence  "setminus"  :same-as "-"  ) 
(define-precedence  "uplus"  :same-as "+"  ) 
(define-precedence "&" :same-as "land")
(define-precedence  "oslash"  :same-as "/"  ) 
(define-precedence  "circ"  :same-as "*"  ) 
(define-precedence  "cdot"  :same-as "*"  ) 
(define-precedence  "cap"  :same-as "land"  ) 
(define-precedence  "oplus"  :same-as "+"  ) 
(define-precedence  "pm"  :same-as "+"  ) 
(define-precedence  "odot"  :same-as "*"  ) 

;;; }

;;; {redefining precedence for limit etc. 
;;; TeX classifies \lim the same as \sin but this is wrong for
;;; precedence, eg when considering \lim\
;;; }

                                        ;(redefine-precedence "lim" :same-as 'big-operator)
                                        ;(redefine-precedence "limsup" :same-as 'big-operator)
                                        ;(redefine-precedence "liminf" :same-as 'big-operator)

(define-precedence "atop" :same-as 'tex-infix-operator)
(define-precedence "over" :same-as 'tex-infix-operator)
(define-precedence "choose" :same-as 'tex-infix-operator )

(define-precedence  ":" :same-as 'conditional-operator)

(define-precedence 'negative :same-as 'unary-minus)
(define-precedence "neg" :same-as 'unary-minus)
(define-precedence "not" :same-as 'unary-minus)
(define-precedence "juxtaposition" :same-as 'juxtaposition)
(define-precedence "," :same-as 'math-list-operator)
(define-precedence "fraction" :same-as 'multiplication)
(define-precedence "bullet" :same-as 'multiplication)
