;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)


;;; Modified: Mon Dec 21 09:32:07 EST 1992
;;; User defined objects now being moved into the separate systems to
;;; which they belong. ie: Macros from cs611 notes are being put under
;;; the system cs611-notes.system etc.
;;; Eventually this file may completely go away, or just hold object
;;; definitions for standard Latex objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modified: Fri Dec 11 11:38:50 EST 1992
;;; Modifying read-aloud methods to match new class definitions for
;;; user  defined objects. ie: use (argument object position) instead
;;; of argument-position object.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define some new objects using the macro define-text-object.
;;; define associated read methods here as well for the present.
;;; each fold contains a tex macro along with the associated read
;;; method for the defined object.

;;; Modified: Wed Apr  7 17:55:42 EDT 1993
;;; defining fraction here:

  (define-text-object :macro-name "frac" 
  :number-args 2
  :processing-function frac-expand 
  :precedence  nil
  :children-are-called (list 'numerator 'denominator )
  :object-name fraction
  :supers (math-object)
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Fri Oct  9 14:01:50 1992

(defmethod read-aloud ((fraction fraction))
  "Read aloud fraction "
  (afl:new-block
   (afl:local-set-state
    (reading-state 'fraction) 
    )
   (read-aloud "Fraction with numerator: ")
   (afl:new-block
    (afl:local-set-state
     (reading-state 'fraction-numerator))
    (read-aloud (numerator-of fraction))
    )
   (read-aloud "And denominator: ")
   (afl:new-block
    (afl:local-set-state
     (reading-state 'fraction-denominator))
    (read-aloud
     (denominator-of fraction))
    )
   (read-aloud "end of fraction. ")
   )
  )
(defmethod  numerator-of ((fraction fraction))
  "Return numerator"
  (first (children fraction ))
  )

(defmethod denominator-of ((fraction fraction ))
  "Return denominator"
  (second (children fraction ))
  )



;;; { mbox

;;; mbox should be handled differently from ordinary text blocks.
;;; This is because they way tex works, new blocks inherit from the
;;; sorrounding environment, whereas mbox introduced a new
;;; environment.  The earlier simple approach of handling mbox like
;;; any other block will therefore cause problems

(define-text-object     :macro-name "mbox" 
  :number-args 1
  :processing-function mbox-expand 
  :object-name text-box
  :supers (document)
  )

(defmethod read-aloud  (( text-box text-box )) 
  "Read aloud method for object text-box "
  (afl:new-block
   (afl:local-set-state
    (afl:generalized-afl-operator  afl:*current-speech-state* 
                                   '(afl:scale-by  afl:average-pitch
                                     .166 
                                     :slot afl:step-size)
                                   '(afl:scale-by afl:head-size .33
                                     :slot afl:step-size )))
   (read-aloud (argument text-box 1))
   )
  )

;;; }
;;; {fbox
;;; For the present treating fbox like mbox, will change reading rule
;;; later to add some bells and whistles.

(define-text-object     :macro-name "fbox" 
  :number-args 1
  :processing-function fbox-expand 
  :object-name text-frame-box 
  :supers (text-box) 
  )

(defmethod read-aloud  (( text-frame-box text-frame-box )) 
  "Read aloud method for object text-frame-box "
  (afl:new-block
   (afl:local-set-state
    (afl:get-point-in-speech-space 'afl:paul))
   (afl:synchronize-and-play *section-cue* :background-flag t)
   (read-aloud (argument text-frame-box 1))
   )
  )




;;; }
;;; {label

#|
(define-text-object     :macro-name "label" 
  :number-args 1
  :processing-function label-default-expand 
  :object-name label
  :supers (document)
  )
;;; automatically generated expander for label will not currently
;;; work. The class has been separately defined. 
|#

;;; Use slots argument-1 ... argument-1 in                         read-aloud 
(defmethod read-aloud  (( label label )) 
  "Read aloud method for object label "
  (read-aloud (contents label ))
  )

;;; }
;;; {sqrt Not handle optional latex argument
(define-text-object     :macro-name "sqrt" 
  :number-args 1
  :processing-function sqrt-expand
  :children-are-called 'radical
  :precedence mathematical-function
  :object-name square-root
  :supers (math-object)
  )


;;; Use slots argument-1 ... argument-1 in                         read-aloud 
(defmethod read-aloud  (( square-root square-root )) 
  "Read aloud method for object square-root "
  (read-aloud " square root ")
  (cond
    ((and (math-object-subtype-p (argument square-root 1))
          (leaf-p (argument square-root 1 )))
     (read-aloud (argument square-root 1 )))
    (t(read-aloud " of ")
      (with-reading-state (reading-state 'children) 
        (read-aloud (argument square-root 1 ))))
    )
  )






;;; }
;;; { integral delimiter as a macro

(define-text-object     :macro-name "varint" 
  :number-args 1
  :processing-function d-expand 
  :object-name integral-delimiter
  :supers (integral-d)
  )

(defmethod read-aloud  (( integral-delimiter integral-delimiter )) 
  "Read aloud method for object integral-delimiter "
  (read-aloud " d ")
  (read-aloud (argument integral-delimiter 1))
  (read-aloud "[)]")
  )

                                        ;(activate-rule 'integral-delimiter 'default)



;;; }
;;; {\ie
(define-text-object     :macro-name "ie" 
  :number-args 0
  :processing-function ie-expand 
  :object-name ie
  :supers (document)
  )

(defmethod read-aloud  (( ie ie )) 
  "Read aloud method for object ie "
  (read-aloud "that is, ")
  )




;;; }
;;; { overbrace, underbrace etc. 

;;; {overbrace 

(define-text-object     :macro-name "overbrace" 
  :number-args 1
  :processing-function overbrace-expand 
  :object-name overbrace
  :supers (math-object)
  )

(defmethod read-aloud  (( overbrace overbrace )) 
  "Read aloud method for object overbrace "
  (with-reading-state (reading-state 'superscript)
    (read-aloud " Begin overbrace "))
  (read-aloud (argument overbrace 1))
  (with-reading-state (reading-state 'superscript )
    (read-aloud " end  overbrace "))
  (read-attributes overbrace)
  )








;;; }
;;; {overline

(define-text-object     :macro-name "overline" 
  :number-args 1
  :processing-function overline-expand 
  :object-name overline
  :supers (math-object)
  )

(defmethod read-aloud  (( overline overline )) 
  "Read aloud method for object overline "
  (with-reading-state (reading-state 'superscript)
    (read-aloud " Begin overline "))
  (read-aloud (argument overline 1))
  (with-reading-state (reading-state 'superscript )
    (read-aloud " end  overline "))
  (read-attributes overline)
  )

(define-reading-state 'overline
    #'(lambda(state)
        (afl:multi-move-to state
                           '(afl:left-volume 50)
                           '(afl:right-volume 0))
        )
  )


;;; }
;;; {underbrace

(define-text-object     :macro-name "underbrace" 
  :number-args 1
  :processing-function underbrace-expand 
  :object-name underbrace
  :supers (math-object)
  )

(defmethod read-aloud  (( underbrace underbrace )) 
  "Read aloud method for object underbrace "
  (with-reading-state (reading-state 'subscript)
    (read-aloud " Begin underbrace "))
  (read-aloud (argument underbrace 1))
  (with-reading-state (reading-state 'subscript )
    (read-aloud " end  underbrace "))
  (read-attributes underbrace)
  )

;;; }
;;; {underline

(define-text-object     :macro-name "underline" 
  :number-args 1
  :processing-function underline-expand 
  :object-name underline
  :supers (math-object)
  )

(defmethod read-aloud  (( underline underline )) 
  "Read aloud method for object underline "
  (with-reading-state (reading-state 'subscript)
    (read-aloud " Begin underline "))
  (read-aloud (argument underline 1))
  (with-reading-state (reading-state 'superscript )
    (read-aloud " end  underline "))
  (read-attributes underline)
  )

;;; }

;;; }
;;; {hspace
(define-text-object     :macro-name "hspace" 
  :number-args 1
  :processing-function hspace-expand 
  :object-name h-space
  :supers (document)
  )

;;; Use slots argument-argument-1 in                         read-aloud 
(defmethod read-aloud  (( h-space h-space )) 
  "Read aloud method for object h-space "
  (tts:queue "[_<300>]")
  )




;;; }



(define-text-object :macro-name "tex" 
  :number-args 0
  :processing-function tex-logo-expand 
  :precedence  nil 
  :object-name tex-logo
  :supers (document)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( tex-logo tex-logo )) 
  "Read aloud method for object tex-logo "
  (afl:send-text "[t`ehkhx_<10>q]")
  )



(define-text-object :macro-name "subgroup" 
  :number-args 0
  :processing-function subgroup-expand 
  :precedence  relational-operator
  :object-name sub-group
  :supers (binary-operator)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( sub-group sub-group )) 
  "Read aloud method for object sub-group "
  (call-next-method) 
  )




(define-text-object :macro-name "divides" 
  :number-args 2
  :processing-function divides-expand 
  :precedence  nil 
  :object-name divides
  :children-are-called (list "divisor" "dividend")
  :supers (math-object)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud 
(defmethod read-aloud  (( divides divides )) 
  "Read aloud method for object divides "
  (read-aloud (argument 1 divides))
  (read-aloud " divides ")
  (read-aloud (argument 2 divides ))
  )

                                        ;(activate-rule 'divides 'default)

(define-text-object :macro-name "gcd" 
  :number-args 2
  :processing-function gcd-expand 
  :precedence  nil 
  :object-name gcd
  :supers (math)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud 
(defmethod read-aloud  (( gcd gcd )) 
  "Read aloud method for object gcd "
  (read-aloud  "gcd of, ")
  (read-math-child  (argument 1 gcd))
  (read-aloud "and ")
  (read-math-child  (argument 2 gcd ))
  )



(define-text-object :macro-name "nonumber" 
  :number-args 0
  :processing-function nonumber-expand 
  :precedence  nil 
  :object-name no-number
  :supers (document)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( no-number no-number )) 
  "Read aloud method for object no-number "
  nil
  )

(define-text-object :macro-name "phantom" 
  :number-args 1
  :processing-function phantom-expand 
  :precedence  nil 
  :object-name phantom 
  :supers (ordinary)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud ((phantom phantom))
  "Read aloud method for object phantom"
  nil
  )

(define-text-object :macro-name "vphantom" 
  :number-args 1
  :processing-function vphantom-expand 
  :precedence  nil 
  :object-name v-phantom 
  :supers (ordinary)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud ((v-phantom v-phantom))
  "Read aloud method for object v-phantom"
  nil
  )


(define-text-object :macro-name "induction" 
  :number-args 2
  :processing-function induction-expand 
  :precedence  nil 
  :object-name induction
  :supers (math-object)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud 
(defmethod read-aloud ((induction induction))
  "Read aloud method for object induction "
  (read-aloud "by induction, ")
  (read-aloud (argument 1 induction ))
  (read-aloud "leads to ")
  (read-aloud (argument 2 induction )))











(define-text-object :macro-name "contentsline" 
  :number-args 3
  :processing-function contentsline-expand 
  :precedence  nil 
  :object-name contents-line
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 3)  in                         read-aloud 
(defmethod read-aloud  (( contents-line contents-line )) 
  "Read aloud method for object contents-line "
  (read-aloud (argument 1 contents-line))
  (read-aloud (argument 2  contents-line ))
  (afl:comma-intonation)
  (read-aloud "page ")
  (read-aloud (argument 3 contents-line ))
  (afl:force-speech)
  (afl:synchronize-and-play *newline-cue*)
  )
;(activate-rule 'contents-line 'default)




(define-text-object :macro-name "numberline" 
  :number-args 1
  :processing-function numberline-expand 
  :precedence  nil 
  :object-name number-line
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud  (( number-line number-line )) 
  "Read aloud method for object number-line "
  (read-aloud (argument 1 number-line ))
(afl:comma-intonation)
  )
;(activate-rule 'number-line 'default )


(define-text-object :macro-name "pagenumbering" 
  :number-args 1
  :processing-function pagenumbering-expand 
  :precedence  nil 
  :object-name pagenumbering
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud  (( pagenumbering pagenumbering )) 
  "Read aloud method for object pagenumbering "
nil
  )


(define-text-object :macro-name "ddots" 
  :number-args 0
  :processing-function ddots-expand 
  :precedence  nil 
  :object-name diagonal-dots
  :supers (ordinary)
  :children-are-called nil
  )

;;; Object has 0 slots 
(defmethod read-aloud  ((diagonal-dots diagonal-dots))
  "Read aloud method for object diagonal-dots "
  (afl:new-block
   (loop for i from 1 to 3 do 
         (afl:send-text "and so on, ")
         (afl:force-speech)
         (afl:local-set-state (afl:multi-step-by afl:*current-speech-state*
                                                 '(afl:left-volume -2.5)
                                                 '(afl:right-volume -2.5)))))
  )

(define-text-object :macro-name "vdots" 
  :number-args 0
  :processing-function v-dots-expand 
  :precedence  nil 
  :object-name vertical-dots
  :supers (ordinary)
  :children-are-called nil
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( vertical-dots vertical-dots )) 
  "Read aloud method for object vertical-dots "
  (afl:send-text "dot, dot, dot. ")
(afl:force-speech)
  )






(define-text-object :macro-name "setlength" 
  :number-args 2
  :processing-function setlength-expand 
  :precedence  nil 
  :object-name setlength
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud 
(defmethod read-aloud  (( setlength setlength )) 
  "Read aloud method for object setlength "
nil
  )



(define-text-object :macro-name "caption" 
  :number-args 1
  :processing-function caption-expand 
  :precedence  nil 
  :object-name caption
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud  (( caption caption )) 
  "Read aloud method for object caption "
(with-reading-state (reading-state 'annotation-voice )
  (read-aloud  (argument 1 caption )))
  )







;;; {latex2e objects 
(define-text-object :macro-name "emph" 
  :number-args 1
  :processing-function emph-expand 
  :precedence  nil 
  :object-name emph
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud  (( emph emph )) 
  "Read aloud method for object emph "
(with-reading-state (reading-state 'emphasize )
  (read-aloud  (argument 1 emph )))
  )




(define-text-object :macro-name "texttt" 
  :number-args 1
  :processing-function texttt-expand 
  :precedence  nil 
  :object-name texttt
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud  (( texttt texttt )) 
  "Read aloud method for object texttt "
(with-reading-state (reading-state 'verbatim-voice )
  (read-aloud  (argument 1 texttt )))
  )










(define-text-object :macro-name "textsf" 
  :number-args 1
  :processing-function textsf-expand 
  :precedence  nil 
  :object-name textsf
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud  (( textsf textsf )) 
  "Read aloud method for object textsf "
(with-reading-state (reading-state 'sans-seriph )
  (read-aloud  (argument 1 textsf )))
  )




(define-text-object :macro-name "textit" 
  :number-args 1
  :processing-function textit-expand 
  :precedence  nil 
  :object-name textit
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud  (( textit textit )) 
  "Read aloud method for object textit "
(with-reading-state (reading-state 'emphasize )
  (read-aloud  (argument 1 textit )))
  )

(define-text-object :macro-name "textbf" 
  :number-args 1
  :processing-function textbf-expand 
  :precedence  nil 
  :object-name textbf
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud  (( textbf textbf )) 
  "Read aloud method for object textbf "
(with-reading-state (reading-state 'bold )
  (read-aloud  (argument 1 textbf )))
  )









 ;;; }
