;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;; {cn15a.tex

;;;  Contains the equivalent audio macros for cn15 which looks like it
;;;  has been well written.

;;;
(define-text-object     :macro-name "alphaeq"
  :number-args 0
  :processing-function alphaeq-expand
  :object-name alpha-equality
  :precedence relational-operator
  :supers (relational-operator)
  )

(def-reading-rule (alpha-equality simple)
  "Read aloud method for object alpha-equality "
  (let*
      ((pause-duration  (compute-pause alpha-equality ))
       (children (children alpha-equality)))
    (afl:with-surrounding-pause pause-duration
      (cond
        ((leaf-p alpha-equality)
         (read-aloud "  equals  ")
         (with-reading-state (reading-state 'subscript)
           (read-aloud " alpha ")))
        (t
         (read-math-child   (first children))
         (afl:tts-queue " ")
         (loop for child in (rest children)
               do
                  (read-aloud "  equals  ")
                  (with-reading-state (reading-state 'subscript)
                    (read-aloud " alpha "))
                  (read-math-child  child ))))
      )))

(defmethod summarize ((alpha-equality alpha-equality))
  (afl:new-block
    (afl:local-set-state :math)
    (read-aloud "  equals  ")
    (with-reading-state (reading-state 'subscript)
      (read-aloud " alpha "))))

(define-text-object     :macro-name "walksa"
  :number-args 0
  :processing-function walksa-expand
  :object-name walksa
  :supers (math)
  )

(defmethod read-aloud  (( walksa walksa ))
  "Read aloud method for object walksa "
  (read-aloud "walks  ")
  (with-reading-state (reading-state 'subscript)
    (read-aloud " alpha "))
  )

(define-text-object     :macro-name "alphaone"
  :number-args 0
  :processing-function alphaone-expand
  :object-name alphaone
  :supers (math)
  )

(defmethod read-aloud  (( alphaone alphaone ))
  "Read aloud method for object alphaone "
  (read-aloud "  equivalent ")
  (with-reading-state (reading-state 'subscript)
    (read-aloud " alpha one "))
  )

(define-text-object     :macro-name "betaone"
  :number-args 0
  :processing-function betaone-expand
  :object-name betaone
  :supers (math)
  )

(defmethod read-aloud  (( betaone betaone ))
  "Read aloud method for object betaone "
  (read-aloud "  reduces ")
  (with-reading-state (reading-state 'subscript)
    (read-aloud " beta one "))
  )

(define-text-object     :macro-name "abmany"
  :number-args 0
  :processing-function abmany-expand
  :object-name abmany
  :supers (math)
  )

(defmethod read-aloud  (( abmany abmany ))
  "Read aloud method for object abmany "
  (read-aloud " by  a many step reduction ")
  )

(define-text-object     :macro-name "displaystyle"
  :number-args 1
  :processing-function displaystyle-expand
  :object-name displaystyle
  :supers (math)
  )

(defmethod read-aloud  (( displaystyle displaystyle ))
  "Read aloud method for object displaystyle "
  (read-aloud (argument displaystyle 1 ))
  )

;;; }

;;; { cn24 macros.

;;; \newcommandDerivII[3]...
(define-text-object     :macro-name "DerivII"
  :number-args 3
  :processing-function derivation-ii-expand
  :object-name derivation-ii
  :supers (math)
  )

;;; Use slots argument ... 1 argument-3 in                         read-aloud
(defmethod read-aloud  (( derivation-ii derivation-ii ))
  "Read aloud method for object derivation-ii "
  (read-aloud " derivation ")
  (afl:tts-icon *paragraph-cue*)
  (read-aloud (argument derivation-ii 1 ))
  (afl:tts-icon   *item-cue*)
  (read-aloud (argument derivation-ii 2 ))
  (afl:tts-icon *newline-cue*)
  (with-reading-state (reading-state 'yields)
    (read-aloud (argument derivation-ii 3 )))
  )

(define-reading-state 'yields
    #'(lambda(state)
        (afl:step-by state 'afl:average-pitch 1)))

;;; \newcommand[2]DerivI...
(define-text-object     :macro-name "DerivI"
  :number-args 2
  :processing-function derivation-i-expand
  :object-name derivation-i
  :supers (math)
  )

;;; Use slots argument-1 ... argument-2 in                         read-aloud
(defmethod read-aloud  (( derivation-i derivation-i ))
  "Read aloud method for object derivation-i "
  (afl:tts-icon *paragraph-cue*)
  (read-aloud (argument derivation-i 1 ))
  (afl:tts-icon *newline-cue*)
  (read-aloud (argument derivation-i 2 ))
  )

;;; }
;;; { pred from solution to problem set 2 cs611

(define-text-object     :macro-name "pred"
  :number-args 0
  :processing-function pred-expand
  :object-name pred
  :supers (math)
  )

;;; Object has 0 slots
(defmethod read-aloud  (( pred pred ))
  "Read aloud method for object pred "
  (read-aloud " right arrow ")
  (with-reading-state (reading-state 'subscript)
    (read-aloud " beta "))
  )

;;; }
