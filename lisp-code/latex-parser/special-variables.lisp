;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
;;; Modified: Mon Mar  2 21:52:02 EST 1992
;;; used define-parsing-function to set up table of parsing functions.
;;; Modified: Sat Feb  8 11:47:12 EST 1992
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Holds all special variables used by the parser.
;;; Convention:
;;; Special variables are named as *foo*:
;;; They are set to nil initially:
;;; there are macros with names starting with define
;;; that are used to add table entries.
;;; At present new items are pushed on to the list which holds the table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Table for holding the parsing functions for different
;;; node types  in a article.
;;; Modified: Fri Dec 25 08:29:58 EST 1992
;;; Switching to using hash table.
;;; <(list version backed up)>

;;; Variable: *PROCESSING-FUNCTION-TABLE*                    Author: raman
;;; Created: Sun Jan 26 11:17:17 1992

(defvar *processing-function-table*
  (make-hash-table :test #'equal) 
  "Holds the table of node types and their associated processing functions.")


;;; Function: DEFINE-PARSING-FUNCTION                           Author: raman
;;; Created: Mon Mar  2 20:57:04 1992

(defun define-parsing-function (object parser) 
  "Installs a parsing function for object by adding
a suitable entry to the table *processing-function-table*"
  (setf  (gethash object *processing-function-table*)  parser)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now define the entries in the table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-parsing-function 'word'process-word)
(define-parsing-function 'text-number 'process-text-number)
(define-parsing-function 'math-number 'process-math-number)
(define-parsing-function 'comment'process-comment)
(define-parsing-function 'newline'process-newline)
(define-parsing-function 'field-separator 'process-field-separator)
(define-parsing-function 'abstract'process-abstract)
(define-parsing-function 'center'process-center)
(define-parsing-function 'block'process-text-block)
(define-parsing-function 'inline-quote'process-inline-quote)
(define-parsing-function 'quote'process-quote)
(define-parsing-function 'quotation'process-quotation)
(define-parsing-function 'inline-math'process-inline-math)
(define-parsing-function 'display-math 'process-display-math)
(define-parsing-function 'cs 'process-cs)
(define-parsing-function 'array'process-array)
(define-parsing-function 'tabular'process-tabular)
(define-parsing-function 'cases  'process-cases)
(define-parsing-function 'enumerate'process-enumerate)
(define-parsing-function 'description'process-description)
(define-parsing-function 'itemize'process-itemize)
(define-parsing-function 'item'process-item)
(define-parsing-function 'equation'process-equation)
(define-parsing-function 'eqnarray'process-eqnarray)
(define-parsing-function 'eqalign 'process-eqalign) 
(define-parsing-function 'slide 'process-slide)
(define-parsing-function 'verbatim 'process-verbatim)
(define-parsing-function 'new-environment'process-new-environment)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; parsing functions for math mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-parsing-function 'math-cs 'process-math-cs)
(define-parsing-function 'subformula 'process-subformula)
(define-parsing-function  'math-string 'process-math-string)
(define-parsing-function 'ordinary 'process-ordinary)
(define-parsing-function 'negation-operator 'process-negation-operator)
(define-parsing-function 'quantifier 'process-quantifier)
(define-parsing-function 'big-operator 'process-big-operator)
(define-parsing-function 'binary-operator 'process-binary-operator)
(define-parsing-function 'special-binary-operator
    'process-binary-operator)
;;; closed and open delimiters that are not caught by
;;; process-delimited-expression should be treated as ordinary
;;; symbols.
(define-parsing-function 'open-delimiter 'process-ordinary)
(define-parsing-function 'close-delimiter 'process-ordinary)
(define-parsing-function 'relational-operator 'process-relational-operator)
(define-parsing-function 'arrow-operator 'process-arrow-operator)
(define-parsing-function
    'mathematical-function-name 'process-mathematical-function-name)
(define-parsing-function 'accent 'process-accent)
(define-parsing-function 'underbar 'process-underbar)
(define-parsing-function 'number 'process-number)
(define-parsing-function 'superscript 'process-superscript)
(define-parsing-function 'subscript 'process-subscript)
(define-parsing-function 'unknown-construct'process-unknown-construct)


;;; Variable: *TEX-MACRO-TABLE*                              Author: raman
;;; Created: Thu Jan 30 08:56:15 1992
;;; Modified: Fri Dec 25 09:01:50 EST 1992
;;; Converting to using hash tables.
;;; <( old version using list backed up)>

(defvar *tex-macro-table*
  (make-hash-table :test #'equal)
  "table to hold entries for known macros.")


;;; Modified: Mon Mar  2 22:11:36 EST 1992
;;; introducing define macro for setting up table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Function: DEFINE-TEX-MACRO                                  Author: raman
;;; Created: Mon Mar  2 22:12:55 1992
;;; Modified: Fri Dec 25 09:04:01 EST 1992
;;; Converting to using hash tables.  <(old version  backed up. )>
(defun define-tex-macro  (macro-name macro-n-args macro-def) 
  "Add entry for macro-name to *tex-macro-table*
the table of known tex macros "
  (setf (gethash macro-name *tex-macro-table*) 
        (make-tex-macro
         :name macro-name
         :number-of-args macro-n-args
         :expand macro-def)
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now set up the table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macros that change font:
(define-tex-macro  "it" 0  'it-expand)
(define-tex-macro  "sc" 0  'sc-expand)
(define-tex-macro  "rm" 0  'rm-expand)
(define-tex-macro  "sl" 0  'sl-expand)
(define-tex-macro  "bf" 0  'bf-expand)
(define-tex-macro  "em" 0  'em-expand)
(define-tex-macro  "large" 0  'large-expand)
(define-tex-macro  "Large" 0  'large-expand)
(define-tex-macro  "LARGE" 0  'large-expand)
(define-tex-macro  "huge" 0  'huge-expand)
(define-tex-macro  "Huge" 0  'huge-expand)
(define-tex-macro  "tt" 0  'tt-expand)
(define-tex-macro  "sf" 0  'sf-expand)

(define-tex-macro "latex" 0 'latex-expand)
(define-tex-macro "LaTeX" 0 'latex-expand)
(define-tex-macro "eg" 0 'eg-expand)
(define-tex-macro  "cite" 1  'cite-expand)
(define-tex-macro  "footnote" 1  'footnote-expand)
(define-tex-macro "index" 1 'index-expand)
(define-tex-macro  "label" 1  'label-expand)
(define-tex-macro "ref" 1 'ref-expand )
(define-tex-macro "eqnref" 1 'ref-expand)
(define-tex-macro "centerline" 1  'centerline-expand)
;;; macros that are used in the header of a document:
(define-tex-macro "title" 1 'title-expand)
(define-tex-macro "author" 1 'author-expand)
(define-tex-macro "date" 1 'date-expand)
(define-tex-macro "address" 1 'address-expand)

(define-tex-macro "mathrel" 1 'mathrel-expand) 
(define-tex-macro "root" 3 'root-expand)
(define-tex-macro  'default 0  'default)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Variable: *TABLE-OF-MATH-DELIMITERS*                     Author: raman
;;; Created: Tue Feb  4 18:35:59 1992

(defvar *table-of-math-delimiters* nil "table of mathematical delimiters.")

;;; Macro: DEFINE-MATH-DELIMITER                             Author: raman
;;; Created: Fri Mar  6 10:31:33 1992

(defmacro define-math-delimiter (open close delimiter-name) 
  "define a math delimiter"
  `(push
    (make-math-delimiter
     :open ,open
     :close ,close
     :name ,delimiter-name)
    *table-of-math-delimiters*)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Now set up the table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-math-delimiter  "{" "}"  "braces") 
(define-math-delimiter  "(" ")"  "paren") 
(define-math-delimiter  "[" "]"  "brackets") 
                                        ;(define-math-delimiter  "|" "|"  'pipe)
(define-math-delimiter '(math-cs  "langle") '(math-cs  "rangle")
  "angle brackets")

(define-math-delimiter '(math-cs "lbrack")  '(math-cs "rbrack" ) "brackets")

(define-math-delimiter '(math-cs "lbrace")  '(math-cs "rbrace" )
  "braces") 
(define-math-delimiter '(math-cs "lfloor")  '(math-cs "rfloor" )
  "floor-brackets")
(define-math-delimiter '(math-cs "lceil")   '(math-cs "rceil" )
  "ceiling-brackets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; boolean variables.
;;; Affect default behaviour
;;; Can be toggled with (toggle *var*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Variable: *SIGNAL-ERROR-ON-UNKNOWN-TEX-MACRO*            Author: raman
;;; Created: Thu Jan 30 11:54:06 1992
;;; external variable: 
(defvar *do-not-signal-error-on-unknown-tex-macro*  nil
  "tell parser to signal error or continue when undefined tex macro
  seen.")

;;; Set it to true for now:
(setf *do-not-signal-error-on-unknown-tex-macro* t)
;;; You can toggle it by using (toggle var)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Variable: *RETURN-A-DEFAULT-PARSER*                      Author: raman
;;; Created: Sat Feb 29 13:10:37 1992

(defvar *return-a-default-parser* nil
  "switch indicating if a default parser is to be returned if none found in parse table")
;;; Set it to true.
(setf *return-a-default-parser* t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



