;;;   -*-   Mode: LISP -*-    ;;;
 
 

(in-package :aster)


;;; Contains some text objects that are not always around.
;;; Serves as a clearing house before definitions moved into their own
;;; system.

;;{{{ for demos:

(define-text-object     :macro-name "descriptive"
  :number-args 0
  :processing-function descr-expand
  :precedence  nil
  :object-name descriptive
  :supers (document)
  )

 
(defmethod read-aloud  (( descriptive descriptive ))
  "Read aloud method for object descriptive "
  (activate-style 'descriptive)
  (activate-style 'use-special-pattern)
  (activate-rule 'integral 'descriptive )
  )

(define-text-object     :macro-name "layout"
  :number-args 0
  :processing-function layout-expand
  :precedence  nil
  :object-name layout
  :supers (document)
  )

 
(defmethod read-aloud  (( layout layout ))
  "Read aloud method for object layout "
  (deactivate-style 'use-special-pattern)
  (deactivate-rule 'superscript )
  (deactivate-style 'descriptive)
  )

(define-text-object     :macro-name "simplelayout"
  :number-args 0
  :processing-function simplelayout-expand
  :precedence  nil
  :object-name simple-layout
  :supers (document)
  )

 
(defmethod read-aloud  (( simple-layout simple-layout ))
  "Read aloud method for object simple-layout "
  (activate-rule 'integral 'simple)
  )

(define-text-object :macro-name "endsimplelayout"
  :number-args 0
  :processing-function endsimplelayout-expand
  :precedence  nil
  :object-name endsimplelayout-expand
  :supers (document)
  )

 
(defmethod read-aloud  (( endsimplelayout-expand endsimplelayout-expand ))
  "Read aloud method for object endsimplelayout-expand "
  (deactivate-rule 'integral)
  )

;;}}}

(define-text-object :macro-name "uselongsummation"
  :number-args 0
  :processing-function uselongsummation-expand
  :precedence  nil
  :object-name use-long-summation
  :supers (document)
  )

 
(defmethod read-aloud  (( use-long-summation use-long-summation ))
  "Read aloud method for object use-long-summation "
  (activate-rule 'summation 'long-descriptive)
  )

(define-text-object :macro-name "endlongsummation"
  :number-args 0
  :processing-function endlongsummation-expand
  :precedence  nil
  :object-name end-long-summation
  :supers (document)
  )

 
(defmethod read-aloud  (( end-long-summation end-long-summation ))
  "Read aloud method for object end-long-summation "
  (when (equal 'long-descriptive (active-rule
                                  (make-instance 'summation )))
    (deactivate-rule 'summation))
  )

(define-text-object :macro-name "term"
  :number-args 1
  :processing-function term-expand
  :precedence  nil
  :object-name term
  :supers (document)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(def-reading-rule (term simple)
  "REnder a term"
  (afl:new-block
    (afl:local-set-state
     (afl:generalized-afl-operator
      afl:*current-speech-state*
      '( afl:move-to afl:pitch-range 0)
      '(afl:move-to afl:richness 100)
      '(afl:move-to afl:smoothness 0)
      )
     )
    (read-aloud (argument term 1 )))
  )

(define-text-object :macro-name "amstex"
  :number-args 0
  :processing-function amstex-expand
  :precedence  nil
  :object-name ams-tex
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( ams-tex ams-tex ))
  "Read aloud method for object ams-tex "
  (afl:tts-queue "[ey] m s tech ")
  )

(define-text-object :macro-name "small"
  :number-args 0
  :processing-function small-expand
  :precedence  nil
  :object-name small
  :supers (document)
  )

 
(defmethod read-aloud  (( small small ))
  "Read aloud method for object small "
  nil
  )

(define-text-object :macro-name "Large"
  :number-args 0
  :processing-function cap-large-expand
  :precedence  nil
  :object-name cap-large
  :supers (document)
  )

 
(defmethod read-aloud  (( cap-large cap-large ))
  "Read aloud method for object cap-large "
  nil
  )

(define-text-object :macro-name "slidetitle"
  :number-args 1
  :processing-function slidtetitle-expand
  :precedence  nil
  :object-name slide-title
  :supers (document)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( slide-title slide-title ))
  "Read aloud method for object slide-title "
  (afl:new-block
    (afl:local-set-state
     (reading-state 'bold))
    (read-aloud (argument slide-title 1 ))
    )
  )

(define-text-object :macro-name "vspace"
  :number-args 2
  :processing-function vspace-expand
  :precedence  nil
  :object-name v-space
  :supers (document)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud
(defmethod read-aloud  (( v-space v-space ))
  "Read aloud method for object v-space "
  nil
  )



 

 




 

(define-text-object :macro-name "voicemail"
  :number-args 1
  :processing-function voicemail-expand
  :precedence  nil
  :object-name voice-mail
  :supers (document))

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( voice-mail voice-mail))
  "Read aloud method for object document "
  (with-reading-state (reading-state 'annotation-voice)
    (read-aloud "Voice mail: "))
  (read-aloud (argument 1  )))

(define-text-object :macro-name "email"
  :number-args 1
  :processing-function email-expand
  :precedence  nil
  :object-name e-mail
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( e-mail e-mail ))
  "Read aloud method for object e-mail "
  (with-reading-state (reading-state 'annotation-voice)
    (read-aloud "e mail: "))
  (read-aloud (argument e-mail 1 ))
  )

(define-text-object :macro-name "activatevariablesubstitution"
  :number-args 0
  :processing-function activate-variable-substitution-expand
  :precedence  nil
  :object-name activate-variable-substitution
  :supers (document)
  )

 
(defmethod read-aloud  (( activate-variable-substitution activate-variable-substitution ))
  "Read aloud method for object activate-variable-substitution "
  (activate-style 'variable-substitution)
  )

(define-text-object :macro-name "deactivatevariablesubstitution"
  :number-args 0
  :processing-function deactivate-variable-substitution-expand
  :precedence  nil
  :object-name deactivate-variable-substitution
  :supers (document)
  )

 
(defmethod read-aloud  (( deactivate-variable-substitution deactivate-variable-substitution ))
  "Read aloud method for object deactivate-variable-substitution "
  (deactivate-style 'variable-substitution)
  )

(define-text-object :macro-name "id"
  :number-args 1
  :processing-function id-expand
  :precedence  nil
  :object-name identifier
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(def-reading-rule (identifier simple)
  "Read aloud method for object identifier "
  (with-reading-state (reading-state 'verbatim-voice )
    (read-aloud (argument 1 identifier  ))))

(define-text-object :macro-name "nonterm"
  :number-args 1
  :processing-function nonterm-expand
  :precedence  nil
  :object-name non-term
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(def-reading-rule (non-term simple)
  "Read aloud method for object non-term "
  (with-reading-state (reading-state 'emphasize)
    (read-aloud (argument 1 non-term ))))

(define-text-object :macro-name "addtocounter"
  :number-args 2
  :processing-function add-to-counter-expand
  :precedence  nil
  :object-name add-to-counter
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud
(defmethod read-aloud  (( add-to-counter add-to-counter ))
  "Read aloud method for object add-to-counter "
  nil
  )
(activate-rule 'add-to-counter 'default)

(define-text-object :macro-name "thanks"
  :number-args 1
  :processing-function thanks-expand
  :precedence  nil
  :object-name thanks
  :supers (footnote)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( thanks thanks ))
  "Read aloud method for object thanks "
  (afl:new-block
    (with-reading-state (reading-state 'footnote)
      (read-aloud (argument 1 thanks))))
  )

(activate-rule 'thanks 'default)

 

 







(define-text-object :macro-name "talktitle"
  :number-args 0
  :processing-function talktitle-expand
  :precedence  nil
  :object-name talk-title
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( talk-title talk-title ))
  "Read aloud method for object talk-title "
  (afl:tts-queue "Recognition, ")
  (afl:tts-force)
  (afl:tts-icon  *newline-cue*)
  (afl:tts-queue "and audio formatting, ")
  (afl:tts-force)
  (afl:tts-icon  *newline-cue*)
  (afl:tts-queue "of structured information objects. ")
  (afl:tts-force)
  )

(define-text-object :macro-name "mdash"
  :number-args 0
  :processing-function m-dash-expand
  :precedence  nil
  :object-name m-dash
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( m-dash m-dash ))
  "Read aloud method for object m-dash "
  (afl:comma-intonation)
  )

(def-reading-rule (l-verbatim summarize)
  "summarize verbatim object. "
  (afl:tts-queue "displayed verbatim text. ")
  (afl:tts-force))

(def-reading-rule (l-verbatim simple)
  "Simple reading rule for l-verbatim text."
  (with-reading-state (reading-state 'verbatim-voice)
    (read-aloud (contents l-verbatim))))

(def-reading-rule (mini-page simple)
  "Simple rendering rule for environment minipage."
  (read-aloud (contents mini-page ))
  )

(def-reading-rule (lisp-documentation simple)
  "Simple reading rule for lisp documentation. "
  (let* ((contents (contents lisp-documentation ))
         (name (first contents ))
         (type (second contents ))
         (args (third contents ))
         (doc (nthcdr 3 contents )))
    (read-aloud type)
    (afl:comma-intonation)
    (afl:tts-force)
    (with-reading-state (reading-state 'bold)
      (read-aloud name)
      (afl:comma-intonation)
      (afl:tts-force))
    (with-reading-state (reading-state 'emphasize)
      (read-aloud args))
    (afl:period-intonation)
    (afl:tts-force)
    (afl:tts-pause 1)
    (read-aloud doc)
    (afl:tts-force)))

(define-text-object :macro-name "lisparg"
  :number-args 1
  :processing-function lisp-arg-expand
  :precedence  nil
  :object-name lisp-arg
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(def-reading-rule (lisp-arg simple)
  "Simple reading rule for object lisp-arg. "
  (with-reading-state
      (reading-state 'emphasize)
    (read-aloud (argument 1 lisp-arg ))))

(define-text-object :macro-name "lispname"
  :number-args 1
  :processing-function lisp-name-expand
  :precedence  nil
  :object-name lisp-name
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(def-reading-rule (lisp-name simple)
  "Simple reading rue for object lisp name. "
  (with-reading-state (reading-state 'bold)
    (read-aloud (argument 1 lisp-name ))))

;;{{{ Thesis add ons:
(define-text-object :macro-name "makeabstitle"
  :number-args 0
  :processing-function make-abs-title-expand
  :precedence  nil
  :object-name make-abs-title
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( make-abs-title make-abs-title ))
  "Read aloud method for object make-abs-title "
  )
(define-text-object :macro-name "contentspage"
  :number-args 0
  :processing-function contents-page-expand
  :precedence  nil
  :object-name contents-page
  :supers (document)
  :children-are-called nil
  )

 

(activate-rule 'contents-page 'float)

(define-text-object :macro-name "tablelistpage"
  :number-args 0
  :processing-function table-list-page-expand
  :precedence  nil
  :object-name table-list-page
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( table-list-page table-list-page ))
  "Read aloud method for object table-list-page "
  )

(define-text-object :macro-name "figurelistpage"
  :number-args 0
  :processing-function figure-list-page-expand
  :precedence  nil
  :object-name figure-list-page
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( figure-list-page figure-list-page ))
  "Read aloud method for object figure-list-page "
  )

(define-text-object :macro-name "singlespacing"
  :number-args 0
  :processing-function single-spacing-expand
  :precedence  nil
  :object-name single-spacing
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( single-spacing single-spacing ))
  "Read aloud method for object single-spacing "
  )





(def-reading-rule (copyright-page  simple)
  "Read aloud a  copyright page ."
  (afl:tts-pause 1)
  (read-aloud "Copyright, 1994, T V Raman. ")
  (read-aloud "All rights reserved. ")
  )
(def-reading-rule (dedication simple)
  "Render the dedication page. "
  (read-aloud "dedication. ")
  (afl:tts-pause 5)
  (read-aloud (contents dedication )))

(def-reading-rule (biographical-sketch simple)
  "Read aloud a  biographical sketch. "
  (read-aloud " Biographical sketch. ")
  (afl:tts-pause 1)
  (read-aloud (contents biographical-sketch))
  )
(define-text-object :macro-name "makecopyright"
  :number-args 0
  :processing-function make-copyright-expand
  :precedence  nil
  :object-name make-copyright
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( make-copyright make-copyright ))
  "Read aloud method for object make-copyright "
  )

(define-text-object :macro-name "pagestyle"
  :number-args 1
  :processing-function pagestyle-expand
  :precedence  nil
  :object-name page-style
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( page-style page-style ))
  "Read aloud method for object page-style "
  )

(define-text-object :macro-name "newpage"
  :number-args 0
  :processing-function newpage-expand
  :precedence  nil
  :object-name newpage
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( newpage newpage ))
  "Read aloud method for object newpage "
  )



 


(define-text-object :macro-name "bs"
  :number-args 0
  :processing-function bs-expand
  :precedence  nil
  :object-name tt-backslash
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( tt-backslash tt-backslash ))
  "Read aloud method for object tt-backslash "
  (afl:tts-queue "backslash" )
  )

(define-text-object :macro-name "tableofcontents"
  :number-args 0
  :processing-function table-of-contents-expand
  :precedence  nil
  :object-name table-of-contents-list
  :supers (document)
  :children-are-called nil
  )

 

(def-reading-rule (afl simple)
  "Simple reading rule for AFL. "
  (afl:tts-queue "[ey]  f l "))

(define-text-object :macro-name "thesistitle"
  :number-args 0
  :processing-function thesis-title-macro-expand
  :precedence  nil
  :object-name thesis-title-macro
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( thesis-title-macro thesis-title-macro ))
  "Read aloud method for object thesis-title-macro "
  (afl:tts-queue "Audio system for Technical Readings. ")
  )

(def-reading-rule (display-text simple)
  "Simple reading rule for displayed text. "
  (afl:new-block
    (afl:local-set-state
     (afl:step-by afl:*current-speech-state*
                  'afl:average-pitch .5))
    (read-aloud (contents display-text ))))

(define-text-object :macro-name "minipagesize"
  :number-args 0
  :processing-function minipagesize-expand
  :precedence  nil
  :object-name mini-page-size
  :supers (document)
  :children-are-called nil
  )

 
(defmethod read-aloud  (( mini-page-size mini-page-size ))
  "Read aloud method for object mini-page-size "
  nil
  )

;;}}}
