;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)


;;; Mon Dec 21 09:31:14 EST 1992
;;; Object definitions from cs611 notes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; { subst

;;; \newcommand{\subst}[3]{#1[#2/#3]}
(define-text-object :macro-name "subst"
  :number-args 3
  :processing-function subst-expand
  :precedence mathematical-function
  :object-name subst
  :supers (math-object)
  )


;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Thu Oct 15 14:40:31 1992
#|
(defmethod read-aloud ((subst subst))
  "read aloud a subst object"
  (afl:new-block
                   (afl:local-set-state 
                    (afl:multi-step-by afl:*current-speech-state*
                                       '(afl:average-pitch 1)))
                   (read-aloud (argument subst 1))
                   (read-aloud " with ")
                   (read-aloud (argument subst 3))
                   (read-aloud "replaced by ") 
                   (read-aloud (argument subst 2))
                   )
  )
|#

#|
(defmethod read-aloud ((subst subst))
  "read aloud a subst object"
  (afl:new-block 
                   (afl:local-set-state 
                    (afl:multi-step-by afl:*current-speech-state*
                                       '(afl:average-pitch 1)))
                   (read-aloud (argument subst 1))
                   (read-aloud " with ")
                   (read-aloud (argument subst 2))
                   (read-aloud " for  ") 
                   (read-aloud (argument subst 3))
                   )
  )
|#


(defmethod read-aloud ((subst subst))
  "Read like a tree "
  (read-aloud "substitution ")
  (with-reading-state (reading-state 'children)
    (read-aloud (argument subst 1))
    (read-aloud (argument subst 2 ))
    (read-aloud (argument subst 3)))
  )

#|
(defmethod read-aloud ((subst subst))
  " quick read subst"
  (read-aloud  (argument subst 1))
  (with-reading-state (reading-state 'superscript)
    (read-aloud  (argument subst 2))
    (read-aloud " slash " )
    (read-aloud (argument subst 3)))
  )
|#
;;; }
;;; { abstraction

;;; \renewcommand{\abs}[2]{\lambda(#1. #2)}
(define-text-object     :macro-name "abs" 
  :number-args  2
  :processing-function  abs-expand
  :object-name abstraction
  :supers (math-object)
  )


;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Thu Oct 15 20:24:33 1992

(defmethod read-aloud  ((abstraction abstraction))
  "Read abstraction like a tree "
  (read-aloud " lambda " )
  (with-reading-state (reading-state 'children)
    (read-aloud (argument abstraction 1))
    (read-aloud  (argument abstraction 2)))
  )

#|
(defmethod read-aloud ((abstraction abstraction))
  "read aloud abstraction"
                                          (read-aloud "abstraction")
  (read-aloud  "lambda ")
  (afl:new-block
   (afl:local-set-state
    (afl:step-by
     afl:*current-speech-state*
     'afl:smoothness 1))
   (read-aloud  (argument abstraction 1))
   (read-aloud "dot")
   (read-aloud (argument abstraction 2))
   )
  )
|#

;;; }
;;; { application

(define-text-object     :macro-name "ap" 
  :number-args 2
  :processing-function ap-expand 
  :object-name application
  :supers (math-object)
  )

(defmethod  read-aloud ((application application ))
  "Read application like a tree "
  (read-aloud " application " )
  (with-reading-state (reading-state  'children )
    (read-aloud (argument application 1 ))
    (read-aloud (argument application 2 )))
  )

#|
(defmethod read-aloud  (( application application )) 
  "Read aloud method for object application "
  (read-aloud (argument application 1))
  (read-aloud " applied to ")
  (with-reading-state   (reading-state 'argument) 
  (read-aloud (argument application 2)))
  )
|#

(define-reading-state 'argument
    #'(lambda(state)
        (afl:step-by state
                     'afl:average-pitch 1)))

;;; }

