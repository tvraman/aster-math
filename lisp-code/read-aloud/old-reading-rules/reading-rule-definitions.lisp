;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))
;;; Wed Nov 11 15:13:25 EST 1992
;;; Contains reading rules defined using define-reading-rule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file is being folded.
;;; Each fold will contain all the rules for a particular object
;;; The fold marker shows the object class and the rule names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; {subst: tree-like linear english-active english-passive 

(define-reading-rule (:rule-name tree-like :class subst)
  (read-aloud "substitution ")
  (with-reading-state (reading-state 'children)
    (read-aloud (argument-1 subst))
    (read-aloud (argument-2 subst))
    (read-aloud (argument-3 subst)))
  )
(define-reading-rule (:rule-name linear :class subst)
  (read-aloud  (argument-1 subst))
  (with-reading-state (reading-state 'superscript)
    (read-aloud  (argument-2 subst))
    (read-aloud " slash " )
    (read-aloud (argument-3 subst)))
  )

(define-reading-state 'subst
    #'(lambda(state)
        (afl:step-by state 'afl:average-pitch 2)))

(define-reading-rule (:rule-name english-active :class subst)
    (with-reading-state (reading-state 'subst) 
                   (read-aloud (argument-1 subst))
                   (read-aloud " with ")
                   (read-aloud (argument-2 subst))
                   (read-aloud " for  ") 
                   (read-aloud (argument-3 subst))
                   )
  )

(define-reading-rule (:rule-name english-passive :class subst) 
    (with-reading-state (reading-state 'subst) 
      (read-aloud (argument-1 subst))
      (read-aloud " with ")
      (read-aloud (argument-3 subst))
      (read-aloud "replaced by ") 
      (read-aloud (argument-2 subst))
      )
  )

;;; }
;;; {application: tree-like linear 

(define-reading-rule (:rule-name tree-like :class application)
  (read-aloud " application " )
  (with-reading-state (reading-state  'children )
    (read-aloud (argument-1 application ))
    (read-aloud (argument-2 application )))
  )

(define-reading-rule (:rule-name linear :class application)
    (read-aloud " application  ")
    (with-reading-state (reading-state 'children)
      (read-aloud (argument-1 application))
      (read-aloud  "  semi colon  ")
      (read-aloud (argument-2 application )))
  )

;;; }
;;; {abstraction: tree-like linear

(define-reading-rule (:rule-name tree-like :class abstraction) 
(read-aloud " lambda ")
(with-reading-state (reading-state 'children)
  (read-aloud (argument-1 abstraction ))
  (read-aloud (argument-2 abstraction )))
  )

(define-reading-rule (:rule-name linear :class abstraction)
    (read-aloud " lambda ")
  (with-reading-state (reading-state 'children)
    (read-aloud (argument-1 abstraction ))
    (read-aloud " dot ")
    (read-aloud (argument-2 abstraction )))
  )

;;; }

;;; {verbatim: quiet verbose

(define-reading-rule (:rule-name quiet :class verbatim)
    )

(define-reading-rule (:rule-name verbose :class verbatim)
    (with-reading-state (reading-state 'verbatim-voice)
      (read-aloud(verbatim-contents verbatim )))
)

;;; }
;;; {derivation-ii  layout english inverted

(define-reading-rule (:rule-name layout :class derivation-ii)
                                 (read-aloud " derivation ")
  (dectalk:synchronize-and-play *paragraph-cue*)
  (read-aloud (argument-1 derivation-ii ))
  (dectalk:synchronize-and-play   *item-cue*)
  (read-aloud (argument-2 derivation-ii ))
  (dectalk:synchronize-and-play *newline-cue*)
  (with-reading-state (reading-state 'yields)
    (read-aloud (argument-3 derivation-ii )))
  )

(define-reading-rule (:rule-name english :class derivation-ii )
    (dectalk:synchronize-and-play *paragraph-cue*)
    (read-aloud " the assumptions ")
    (read-aloud (argument-1 derivation-ii ))
    (dectalk:synchronize-and-play *item-cue* :background-flag t)
    (read-aloud " and " )
    (read-aloud (argument-2 derivation-ii ))
(dectalk:synchronize-and-play *newline-cue* :background-flag t)
(read-aloud " yield ")
(with-reading-state (reading-state 'yields)
  (read-aloud (argument-3 derivation-ii )))
)

(define-reading-rule (:rule-name inverted :class derivation-ii)
    (dectalk:synchronize-and-play *newline-cue* :background-flag t) 
    (read-aloud " we have ")
    (read-aloud (argument-3 derivation-ii ))
    (dectalk:synchronize-and-play *newline-cue* :background-flag t)
    (read-aloud " because ")
    (read-aloud (argument-1 derivation-ii ))
    (dectalk:synchronize-and-play *item-cue* :background-flag t)
    (read-aloud " and ")
    (read-aloud (argument-2 derivation-ii ))
    )

;;; }
;;; { derivation-i layout english inverted

(define-reading-rule (:rule-name layout :class derivation-i)
  (dectalk:synchronize-and-play *paragraph-cue*)
  (read-aloud (argument-1 derivation-i ))
  (dectalk:synchronize-and-play *newline-cue*)
  (read-aloud (argument-2 derivation-i ))
  )

(define-reading-rule (:rule-name inverted-verbose   :class derivation-i)
    (dectalk:synchronize-and-play *newline-cue* :background-flag t)
  (read-aloud " we know ")
  (read-aloud (argument-2 derivation-i ))
  (dectalk:synchronize-and-play *newline-cue* :background-flag t)
  (read-aloud "  because ")
  (read-aloud (argument-1 derivation-i ))
  (dectalk:synchronize-and-play *newline-cue* :background-flag t)
  (read-aloud " is true ")
  )
(define-reading-rule (:rule-name inverted   :class derivation-i)
    (dectalk:synchronize-and-play *newline-cue* :background-flag nil)
  (read-aloud (argument-2 derivation-i ))
  (dectalk:synchronize-and-play *newline-cue* :background-flag t)
  (read-aloud "  because ")
  (read-aloud (argument-1 derivation-i ))
  (dectalk:synchronize-and-play *newline-cue* :background-flag nil)
  )


(define-reading-rule (:rule-name english-verbose  :class derivation-i)
    (dectalk:synchronize-and-play *paragraph-cue* :background-flag t)
  (read-aloud "From the fact that ")
  (read-aloud (argument-1 derivation-i ))
  (dectalk:synchronize-and-play *newline-cue* :background-flag t)
  (read-aloud " we know that ")
  (read-aloud (argument-2 derivation-i ))
  (read-aloud " is true ")
  )

(define-reading-rule (:rule-name english  :class derivation-i)
    (dectalk:synchronize-and-play *paragraph-cue* :background-flag t)
  (read-aloud (argument-1 derivation-i ))
  (dectalk:synchronize-and-play *newline-cue* :background-flag t)
  (read-aloud "  derives ")
  (read-aloud (argument-2 derivation-i ))
  )

;;; }
;;; { fraction: fraction inference

(define-reading-rule (:rule-name fraction :class fraction)
    (afl:named-block fraction
                     (afl:local-set-state
                      (reading-state 'fraction) 
                      )
                     (read-aloud "Fraction with numerator: ")
                     (afl:named-block fraction-numerator 
                                      (afl:local-set-state
                                       (reading-state 'fraction-numerator))
                                      (read-aloud (fraction-numerator fraction))
                                      )
                     (read-aloud "And denominator: ")
                     (afl:named-block fraction-denominator
                                      (afl:local-set-state
                                       (reading-state 'fraction-denominator))
                                      (read-aloud
                                       (fraction-denominator fraction))
                                      )
                     (read-aloud  "end of fraction. ")
                     )
  )

(define-reading-rule (:rule-name inference :class fraction )
    (read-aloud " from ")
    (read-aloud (fraction-numerator fraction))
  (dectalk:synchronize-and-play *item-cue* :background-flag t)
    (read-aloud  " we infer ")
  (read-aloud (fraction-denominator fraction))
  )



;;; }
;;; {itemize: quiet default
(define-reading-rule (:rule-name quiet :class itemized-list)
    nil)

(define-reading-rule (:rule-name default :class itemized-list)
    (call-next-method))
;;; }
