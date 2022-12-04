;;;   -*-   Mode: LISP -*-    ;;;

(in-package :aster)
;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Fri Oct  9 14:01:50 1992

(defmethod read-aloud ((fraction fraction))
  "Read aloud fraction "
  (afl:new-block
    (afl:local-set-state (reading-state 'fraction))
    (read-aloud "Fraction with numerator: ")
    (afl:new-block
      (afl:local-set-state (reading-state 'fraction-numerator))
      (read-aloud (numerator-of fraction)))
    (read-aloud "And denominator: ")
    (afl:new-block
      (afl:local-set-state (reading-state 'fraction-denominator))
      (read-aloud
       (denominator-of fraction)))
    (read-aloud "end of fraction. ")))

(defmethod  numerator-of ((fraction fraction))
  "Return numerator"
  (first (children fraction )))

(defmethod denominator-of ((fraction fraction ))
  "Return denominator"
  (second (children fraction )))

;;{{{ mbox

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

;;}}}
;;{{{fbox
;;; For the present treating fbox like mbox, will change reading rule
;;; later to add some bells and whistles.

(defmethod read-aloud  (( text-frame-box text-frame-box ))
  "Read aloud method for object text-frame-box "
  (afl:new-block
    (afl:local-set-state (gethash 'afl:paul (afl:standard-voices)))
    (read-aloud (argument text-frame-box 1))
    )
  )

;;}}}
;;{{{label

#|

;;; automatically generated expander for label will not currently
;;; work. The class has been separately defined.
|#

;;; Use slots argument-1 ... argument-1 in                         read-aloud
(defmethod read-aloud  (( label label ))
  "Read aloud method for object label "
  (read-aloud (contents label ))
  )

;;}}}
;;{{{sqrt Not handle optional latex argument

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

;;}}}
;;{{{ integral delimiter as a macro

(defmethod read-aloud  (( integral-delimiter integral-delimiter ))
  "Read aloud method for object integral-delimiter "
  (read-aloud " d ")
  (read-aloud (argument integral-delimiter 1))
  (read-aloud "[)]")
  )

                                        ;(activate-rule 'integral-delimiter 'default)

;;}}}
;;{{{\ie

(defmethod read-aloud  (( ie ie ))
  "Read aloud method for object ie "
  (read-aloud "that is, ")
  )

;;}}}
;;{{{ overbrace, underbrace etc.

;;{{{overbrace

(defmethod read-aloud  (( overbrace overbrace ))
  "Read aloud method for object overbrace "
  (with-reading-state (reading-state 'superscript)
    (read-aloud " Begin overbrace "))
  (read-aloud (argument overbrace 1))
  (with-reading-state (reading-state 'superscript )
    (read-aloud " end  overbrace "))
  (read-attributes overbrace)
  )

;;}}}
;;{{{overline

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
                                        ;'(afl:left-volume 50)
                                        ;'(afl:right-volume 0)
                           )
        )
  )

;;}}}
;;{{{underbrace

(defmethod read-aloud  (( underbrace underbrace ))
  "Read aloud method for object underbrace "
  (with-reading-state (reading-state 'subscript)
    (read-aloud " Begin underbrace "))
  (read-aloud (argument underbrace 1))
  (with-reading-state (reading-state 'subscript )
    (read-aloud " end  underbrace "))
  (read-attributes underbrace)
  )

;;}}}
;;{{{underline

(defmethod read-aloud  (( underline underline ))
  "Read aloud method for object underline "
  (with-reading-state (reading-state 'subscript)
    (read-aloud " Begin underline "))
  (read-aloud (argument underline 1))
  (with-reading-state (reading-state 'superscript )
    (read-aloud " end  underline "))
  (read-attributes underline)
  )

;;}}}

;;}}}
;;{{{hspace

;;; Use slots argument-argument-1 in                         read-aloud
(defmethod read-aloud  (( h-space h-space ))
  "Read aloud method for object h-space "
  (afl:tts-queue "[_<300>]")
  )

;;}}}


(defmethod read-aloud  (( tex-logo tex-logo ))
  "Read aloud method for object tex-logo "
  (afl:tts-queue "[t`ehkhx_<10>q]")
  )


(defmethod read-aloud  (( sub-group sub-group ))
  "Read aloud method for object sub-group "
  (call-next-method)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud
(defmethod read-aloud  (( divides divides ))
  "Read aloud method for object divides "
  (read-aloud (argument divides 1))
  (read-aloud " divides ")
  (read-aloud (argument divides 2 ))
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud
(defmethod read-aloud  (( a-gcd a-gcd ))
  "Read aloud method for object gcd "
  (read-aloud  "gcd of, ")
  (read-math-child  (argument a-gcd 1))
  (read-aloud "and ")
  (read-math-child  (argument a-gcd 2 ))
  )


(defmethod read-aloud  (( no-number no-number ))
  "Read aloud method for object no-number "
  nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud ((phantom phantom))
  "Read aloud method for object phantom"
  nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud ((v-phantom v-phantom))
  "Read aloud method for object v-phantom"
  nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud
(defmethod read-aloud ((induction induction))
  "Read aloud method for object induction "
  (read-aloud "by induction, ")
  (read-aloud (argument induction 1 ))
  (read-aloud "leads to ")
  (read-aloud (argument induction 2 )))

;;; Use  (argument object)  1 ...( argument
                        ;;; object 3)  in                         read-aloud
(defmethod read-aloud  (( contents-line contents-line ))
  "Read aloud method for object contents-line "
  (read-aloud (argument 1 contents-line))
  (read-aloud (argument contents-line  2 ))
  (afl:tts-queue "[_,]")
  (read-aloud "page ")
  (read-aloud (argument contents-line 3 ))
  (afl:tts-force)
  )
                                        ;(activate-rule 'contents-line 'default)

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( number-line number-line ))
  "Read aloud method for object number-line "
  (read-aloud (argument number-line 1 ))
  (afl:tts-queue "[_,]")
  )
                                        ;(activate-rule 'number-line 'default )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( pagenumbering pagenumbering ))
  "Read aloud method for object pagenumbering "
  nil
  )


(defmethod read-aloud  ((diagonal-dots diagonal-dots))
  "Read aloud method for object diagonal-dots "
  (afl:new-block
    (loop for i from 1 to 3 do
      (afl:tts-queue "and so on, ")
      (afl:tts-force)
      (afl:local-set-state
       (afl:multi-step-by afl:*current-speech-state*))))
  )


(defmethod read-aloud  (( vertical-dots vertical-dots ))
  "Read aloud method for object vertical-dots "
  (afl:tts-queue "dot, dot, dot. ")
  (afl:tts-force)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud
(defmethod read-aloud  (( setlength setlength ))
  "Read aloud method for object setlength "
  nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( caption caption ))
  "Read aloud method for object caption "
  (with-reading-state (reading-state 'annotation-voice )
    (read-aloud  (argument caption 1 )))
  )

;;{{{latex2e objects

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( emph emph ))
  "Read aloud method for object emph "
  (with-reading-state (reading-state 'emphasize )
    (read-aloud  (argument emph 1 )))
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( texttt texttt ))
  "Read aloud method for object texttt "
  (with-reading-state (reading-state 'verbatim-voice )
    (read-aloud  (argument texttt 1 )))
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( textsf textsf ))
  "Read aloud method for object textsf "
  (with-reading-state (reading-state 'sans-seriph )
    (read-aloud  (argument textsf 1 )))
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( textit textit ))
  "Read aloud method for object textit "
  (with-reading-state (reading-state 'emphasize )
    (read-aloud  (argument textit 1 )))
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( textbf textbf ))
  "Read aloud method for object textbf "
  (with-reading-state (reading-state 'bold )
    (read-aloud  (argument textbf 1 )))
  )

;;}}}
