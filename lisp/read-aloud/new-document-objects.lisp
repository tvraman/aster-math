;;;   -*-   Mode: LISP -*-    ;;;
 
 

(in-package :aster)
;;{{{ mbox

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

;;}}}
;;{{{fbox

;;; For the present treating fbox like mbox, will change reading rule
;;; later to add some bells and whistles.

(define-text-object     :macro-name "fbox"
  :number-args 1
  :processing-function fbox-expand
  :object-name text-frame-box
  :supers (text-box)
  )

;;}}}
;;{{{sqrt Not handle optional latex argument

(define-text-object     :macro-name "sqrt"
  :number-args 1
  :processing-function sqrt-expand
  :children-are-called 'radical
  :precedence mathematical-function
  :object-name square-root
  :supers (math-object))

;;}}}
;;{{{ integral delimiter as a macro

(define-text-object     :macro-name "varint"
  :number-args 1
  :processing-function d-expand
  :object-name integral-delimiter
  :supers (integral-d))

;;}}}
;;{{{\ie

(define-text-object     :macro-name "ie"
  :number-args 0
  :processing-function ie-expand
  :object-name ie
  :supers (document))

;;}}}
;;{{{ overbrace, underbrace etc.

;;{{{overbrace

(define-text-object     :macro-name "overbrace"
  :number-args 1
  :processing-function overbrace-expand
  :object-name overbrace
  :supers (math-object))

;;}}}
;;{{{overline

(define-text-object     :macro-name "overline"
  :number-args 1
  :processing-function overline-expand
  :object-name overline
  :supers (math-object))

(define-reading-state 'overline
    #'(lambda(state)
        (afl:multi-move-to state
                                        ;'(afl:left-volume 50)
                                        ;'(afl:right-volume 0)
                           )
        ))

;;}}}
;;{{{underbrace

(define-text-object     :macro-name "underbrace"
  :number-args 1
  :processing-function underbrace-expand
  :object-name underbrace
  :supers (math-object))

;;}}}
;;{{{underline

(define-text-object     :macro-name "underline"
  :number-args 1
  :processing-function underline-expand
  :object-name underline
  :supers (math-object))

;;}}}

;;}}}
;;{{{hspace

(define-text-object     :macro-name "hspace"
  :number-args 1
  :processing-function hspace-expand
  :object-name h-space
  :supers (document))

;;}}}

(define-text-object :macro-name "tex"
  :number-args 0
  :processing-function tex-logo-expand
  :precedence  nil
  :object-name tex-logo
  :supers (document))

(define-text-object :macro-name "subgroup"
  :number-args 0
  :processing-function subgroup-expand
  :precedence  relational-operator
  :object-name sub-group
  :supers (binary-operator))

(define-text-object :macro-name "divides"
  :number-args 2
  :processing-function divides-expand
  :precedence  nil
  :object-name divides
  :children-are-called (list "divisor" "dividend")
  :supers (math-object))

(define-text-object :macro-name "nonumber"
  :number-args 0
  :processing-function nonumber-expand
  :precedence  nil
  :object-name no-number
  :supers (document))

(define-text-object :macro-name "phantom"
  :number-args 1
  :processing-function phantom-expand
  :precedence  nil
  :object-name phantom
  :supers (ordinary))

(define-text-object :macro-name "vphantom"
  :number-args 1
  :processing-function vphantom-expand
  :precedence  nil
  :object-name v-phantom
  :supers (ordinary))

(define-text-object :macro-name "induction"
  :number-args 2
  :processing-function induction-expand
  :precedence  nil
  :object-name induction
  :supers (math-object)
  :children-are-called nil)

(define-text-object :macro-name "contentsline"
  :number-args 3
  :processing-function contentsline-expand
  :precedence  nil
  :object-name contents-line
  :supers (document)
  :children-are-called nil)

(define-text-object :macro-name "numberline"
  :number-args 1
  :processing-function numberline-expand
  :precedence  nil
  :object-name number-line
  :supers (document)
  :children-are-called nil)

(define-text-object :macro-name "pagenumbering"
  :number-args 1
  :processing-function pagenumbering-expand
  :precedence  nil
  :object-name pagenumbering
  :supers (document)
  :children-are-called nil)

(define-text-object :macro-name "ddots"
  :number-args 0
  :processing-function ddots-expand
  :precedence  nil
  :object-name diagonal-dots
  :supers (ordinary)
  :children-are-called nil)

(define-text-object :macro-name "vdots"
  :number-args 0
  :processing-function v-dots-expand
  :precedence  nil
  :object-name vertical-dots
  :supers (ordinary)
  :children-are-called nil)

(define-text-object :macro-name "setlength"
  :number-args 2
  :processing-function setlength-expand
  :precedence  nil
  :object-name setlength
  :supers (document)
  :children-are-called nil)

(define-text-object :macro-name "caption"
  :number-args 1
  :processing-function caption-expand
  :precedence  nil
  :object-name caption
  :supers (document)
  :children-are-called nil)

;;{{{latex2e objects
(define-text-object :macro-name "emph"
  :number-args 1
  :processing-function emph-expand
  :precedence  nil
  :object-name emph
  :supers (document)
  :children-are-called nil
  )

(define-text-object :macro-name "texttt"
  :number-args 1
  :processing-function texttt-expand
  :precedence  nil
  :object-name texttt
  :supers (document)
  :children-are-called nil)

(define-text-object :macro-name "textsf"
  :number-args 1
  :processing-function textsf-expand
  :precedence  nil
  :object-name textsf
  :supers (document)
  :children-are-called nil)

(define-text-object :macro-name "textit"
  :number-args 1
  :processing-function textit-expand
  :precedence  nil
  :object-name textit
  :supers (document)
  :children-are-called nil)

(define-text-object :macro-name "href"
  :number-args 2
  :processing-function href-expand
  :precedence  nil
  :object-name href
  :supers (document)
  :children-are-called '(url text))

(define-text-object :macro-name "textbf"
  :number-args 1
  :processing-function textbf-expand
  :precedence  nil
  :object-name textbf
  :supers (document)
  :children-are-called nil)


(define-text-object :macro-name "mathrm"
  :number-args 1
  :processing-function mathrm-expand
  :precedence  nil
  :object-name mathrm
  :supers (math-object)
  :children-are-called 'variable)
;;}}}
