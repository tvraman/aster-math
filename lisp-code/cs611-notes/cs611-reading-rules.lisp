;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;; Reading rules for objects from cs611 notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; { a-subst : tree-like linear english-active english-passive

(defmethod weight  (( a-subst  a-subst ))
  (round (+ 1
            (/ (weight (argument  a-subst  1 )) 3)
            (/ (weight (argument  a-subst  2)) 3)
            (/ (weight (argument  a-subst  3 )) 3) ))
  )

(def-reading-rule (a-subst  tree-like)
  " tree-like reading rule for object  a-subst "
  (read-aloud "substitution ")
  (with-reading-state (reading-state 'children)
    (read-aloud (argument  a-subst  1))
    (read-aloud (argument  a-subst  2))
    (read-aloud (argument  a-subst  3)))
  )
(def-reading-rule (a-subst  linear)
  " linear reading rule for object  a-subst "
  (read-aloud  (argument  a-subst  1))
  (with-reading-state (reading-state 'superscript)
    (read-aloud  (argument  a-subst  2))
    (read-aloud " slash " )
    (read-aloud (argument  a-subst  3)))
  )

(define-reading-state ' a-subst 
    #'(lambda(state)
        (afl:step-by state 'afl:average-pitch 2)))

;;; Using a new afl state when using verbose readings makes it sound
;;; awful.
(def-reading-rule (a-subst  english-active)
  " english-active reading rule for object  a-subst "
  (let*  ((object-weight (weight  a-subst ))
          (pause-amount   (if (> object-weight 1)
                              (* object-weight *pause-around-child*)
                              0)))
    (afl:with-surrounding-pause  (* pause-amount *pause-around-child*)
      (read-aloud (argument  a-subst  1 ))
      (read-aloud " with ")
      (read-aloud (argument  a-subst  2))
      (read-aloud " for  ")
      (read-aloud (argument  a-subst  3 ))))
  )
(def-reading-rule (a-subst  english-passive)
  " english-passive reading rule for object  a-subst "
  (let*  ((object-weight (weight  a-subst ))
          (pause-amount   (if (> object-weight 1)
                              (* object-weight *pause-around-child*)
                              0)))
    (afl:with-surrounding-pause  (* pause-amount *pause-around-child*)
      (read-aloud (argument  a-subst  1))
      (read-aloud " with ")
      (read-aloud (argument  a-subst  3))
      (read-aloud "replaced by ")
      (read-aloud (argument  a-subst  2 ))))
  )
(activate-rule  ' a-subst  'tree-like)

;;; }
;;; {application: tree-like linear english

(def-reading-rule (application tree-like)
  " tree-like reading rule for object application"
  (read-aloud " application " )
  (with-reading-state (reading-state  'children )
    (read-aloud (argument application 1 ))
    (read-aloud (argument application 2 )))
  )
(def-reading-rule (application linear)
  " linear reading rule for object application"
  (read-aloud " application  ")
  (with-reading-state (reading-state 'children)
    (read-aloud (argument application 1))
    (read-aloud  "  semi colon  ")
    (read-aloud (argument application 2 )))
  )

(def-reading-rule (abstraction english)
  "English rule for abstraction. "
  (let ((pause-amount (compute-pause abstraction )))
    (afl:new-block
      (afl:with-surrounding-pause pause-amount
        (afl:tts-queue "lambda of, ")
        (read-aloud (argument 1 abstraction))
        (afl:tts-queue "is ")
        (read-aloud (argument 2 abstraction))))))
(activate-rule 'application 'tree-like)

;;; }
;;; {abstraction: tree-like linear english

(def-reading-rule (abstraction tree-like)
  " tree-like reading rule for object abstraction"
  (read-aloud " lambda ")
  (with-reading-state (reading-state 'children)
    (read-aloud (argument abstraction 1  ))
    (read-aloud (argument abstraction 2 )))
  )
(def-reading-rule (abstraction linear)
  " linear reading rule for object abstraction"
  (read-aloud " lambda ")
  (with-reading-state (reading-state 'children)
    (read-aloud (argument abstraction 1 ))
    (read-aloud " dot ")
    (read-aloud (argument abstraction 2 )))
  )

(def-reading-rule (application english)
  "English reading rule for application. "
  (let ((pause-amount (compute-pause application )))
    (afl:new-block
      (afl:with-surrounding-pause pause-amount
        (afl:tts-queue "Application of ")
        (read-aloud (argument 1 application ))
        (afl:tts-queue "to, ")
        (read-aloud (argument 2 application ))))))

(activate-rule 'abstraction 'tree-like)

;;; }
;;; {derivation-ii  layout english inverted
(def-reading-rule (derivation-ii layout)
  " layout reading rule for object derivation-ii"
  (read-aloud " derivation ")
  (afl:tts-icon *paragraph-cue*)
  (read-aloud (argument derivation-ii 1 ))
  (afl:tts-icon   *item-cue*)
  (read-aloud (argument derivation-ii 2 ))
  (afl:tts-icon *newline-cue*)
  (with-reading-state (reading-state 'yields)
    (read-aloud (argument derivation-ii 3 )))
  )

(def-reading-rule (derivation-ii english)
  " english reading rule for object derivation-ii"
  (afl:tts-icon *paragraph-cue*)
  (read-aloud " the assumptions ")
  (read-aloud (argument derivation-ii 1 ))
  (afl:tts-icon *item-cue* )
  (read-aloud " and " )
  (read-aloud (argument derivation-ii 2 ))
  (afl:tts-icon *newline-cue* )
  (read-aloud " yield ")
  (with-reading-state (reading-state 'yields)
    (read-aloud (argument derivation-ii 3 )))
  )
(def-reading-rule (derivation-ii inverted)
  " inverted reading rule for object derivation-ii"
  (afl:tts-icon *newline-cue* )
  (read-aloud " we have ")
  (read-aloud (argument derivation-ii 3 ))
  (afl:tts-icon *newline-cue* )
  (read-aloud " because ")
  (read-aloud (argument derivation-ii 1 ))
  (afl:tts-icon *item-cue* )
  (read-aloud " and ")
  (read-aloud (argument derivation-ii 2 ))
  )

;;; }
;;; { derivation-i layout english inverted
(def-reading-rule (derivation-i layout)
  " layout reading rule for object derivation-i"
  (afl:tts-icon *paragraph-cue*)
  (read-aloud (argument derivation-i 1 ))
  (afl:tts-icon *newline-cue*)
  (read-aloud (argument derivation-i 2 ))
  )

(def-reading-rule (derivation-i inverted-verbose)
  " inverted-verbose reading rule for object derivation-i"
  (afl:tts-icon *newline-cue* )
  (read-aloud " we know ")
  (read-aloud (argument derivation-i 2 ))
  (afl:tts-icon *newline-cue* )
  (read-aloud "  because ")
  (read-aloud (argument derivation-i 1 ))
  (afl:tts-icon *newline-cue* )
  (read-aloud " is true ")
  )

(def-reading-rule (derivation-i   inverted)
  " inverted reading rule for object derivation-i"
  (afl:tts-icon *newline-cue* )
  (read-aloud (argument derivation-i 2 ))
  (afl:tts-icon *newline-cue* )
  (read-aloud "  because ")
  (read-aloud (argument derivation-i 1 ))
  (afl:tts-icon *newline-cue* )
  )

(def-reading-rule (derivation-i english-verbose)
  " english-verbose reading rule for object derivation-i"
  (afl:tts-icon *paragraph-cue* )
  (read-aloud "From the fact that ")
  (read-aloud (argument derivation-i 1 ))
  (afl:tts-icon *newline-cue* )
  (read-aloud " we know that ")
  (read-aloud (argument derivation-i 2 ))
  (read-aloud " is true ")
  )
(def-reading-rule (derivation-i english)
  " english reading rule for object derivation-i"
  (afl:tts-icon *paragraph-cue* )
  (read-aloud (argument derivation-i 1 ))
  (afl:tts-icon *newline-cue* )
  (read-aloud "  derives ")
  (read-aloud (argument derivation-i 2 ))
  )

;;; }
