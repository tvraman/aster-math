;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
;;; Created: Tue Feb 25 16:46:13 EST 1992
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Contains the classification of symbols in math mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Variable: *MATH-CLASSIFICATION-TABLE*                    Author: raman
;;; Created: Tue Feb 25 14:51:57 1992

(defvar *math-classification-table* (make-hash-table   :test #'equal )
  "Hash table holding classification of symbols in math mode.")

;;; Modified: Wed Sep 16 08:50:11 EDT 1992
;;; made inline:
(proclaim '(inline lookup-math-classification))
;;; Function: LOOKUP-MATH-CLASSIFICATION                     Author: raman
;;; Created: Tue Feb 25 14:53:02 1992

(defun lookup-math-classification (token) 
  "lookup classification in hash table."
  (gethash token *math-classification-table*)
  )


;;; Function: DEFINE-MATH-SYMBOL-CLASSIFICATION                      Author: raman
;;; Created: Tue Feb 25 14:55:33 1992
(proclaim '(inline define-math-symbol-classification))
(defun  define-math-symbol-classification (symbol classification) 
  "Classify symbol as of type classification for math mode"
  (setf (gethash symbol *math-classification-table*)
        classification)
  )

;;; Function: REMOVE-MATH-SYMBOL-CLASSIFICATION              Author: raman
;;; Created: Sun Nov  8 10:46:34 1992

(defun remove-math-symbol-classification (symbol) 
  "Remove current classification"
  (remhash symbol *math-classification-table*)
  )

;;; Function: REDEFINE-MATH-SYMBOL-CLASSIFICATION            Author: raman
;;; Created: Sun Nov  8 10:47:17 1992

(defun redefine-math-symbol-classification (symbol classification) 
  "Redefine classification"
  (remove-math-symbol-classification symbol)
  (define-math-symbol-classification symbol classification )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Classification of symbols in math mode:
;;; This classification is taken from Appendix F of the tex book.
;;; Refer to the file "~raman/books/texdocs/tex3.0/ch35"
;;; <( link to appendix f of tex book)>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Current classification types:
;;; ordinary
;;; large-operator
;;; binary-operator
;;; relational-operator
;;; negation-operator
;;; arrow-operator
;;; open-delimiter
;;; close-delimiter
;;; mathematical-function-name
;;; accent
;;; underbar
(define-math-symbol-classification "imath" 'ordinary)
(define-math-symbol-classification "jmath" 'ordinary)
(define-math-symbol-classification  "@" 'ordinary)
(define-math-symbol-classification "alpha" 'ordinary)
(define-math-symbol-classification "iota" 'ordinary)
(define-math-symbol-classification "varrho"  'ordinary)
(define-math-symbol-classification "beta"  'ordinary)
(define-math-symbol-classification "kappa"  'ordinary)
(define-math-symbol-classification "sigma"  'ordinary)
(define-math-symbol-classification "gamma"  'ordinary)
(define-math-symbol-classification "lambda"  'ordinary)
(define-math-symbol-classification "varsigma"  'ordinary)
(define-math-symbol-classification " delta"  'ordinary)
(define-math-symbol-classification "mu"  'ordinary)
(define-math-symbol-classification "tau"  'ordinary)
(define-math-symbol-classification "epsilon"  'ordinary)
(define-math-symbol-classification "nu"  'ordinary)
(define-math-symbol-classification "upsilon"  'ordinary)
(define-math-symbol-classification "varepsilon"  'ordinary)
(define-math-symbol-classification "xi"  'ordinary)
(define-math-symbol-classification "phi"  'ordinary)
(define-math-symbol-classification "zeta"  'ordinary)
(define-math-symbol-classification "varphi"  'ordinary)
(define-math-symbol-classification "eta"  'ordinary)
(define-math-symbol-classification "pi"  'ordinary)
(define-math-symbol-classification "chi"  'ordinary)
(define-math-symbol-classification "theta"  'ordinary)
(define-math-symbol-classification "varpi"  'ordinary)
(define-math-symbol-classification "psi"  'ordinary)
(define-math-symbol-classification "vartheta"  'ordinary)
(define-math-symbol-classification "rho"  'ordinary)
(define-math-symbol-classification "omega"  'ordinary)

;;; Upper case greek letters.

(define-math-symbol-classification "Gamma" 'ordinary)
(define-math-symbol-classification "Xi" 'ordinary)
(define-math-symbol-classification "Phi" 'ordinary)
(define-math-symbol-classification "Delta" 'ordinary)
(define-math-symbol-classification "Pi" 'ordinary)
(define-math-symbol-classification "Psi" 'ordinary)
(define-math-symbol-classification "Theta" 'ordinary)
(define-math-symbol-classification "Sigma" 'ordinary)
(define-math-symbol-classification "Omega" 'ordinary)
(define-math-symbol-classification "Lambda" 'ordinary)
(define-math-symbol-classification "Upsilon" 'ordinary)

;;; English letters.
(define-math-symbol-classification "a" 'ordinary)
(define-math-symbol-classification "b" 'ordinary)
(define-math-symbol-classification "c" 'ordinary)
(define-math-symbol-classification "d" 'ordinary)
(define-math-symbol-classification "e" 'ordinary)
(define-math-symbol-classification "f" 'ordinary)
(define-math-symbol-classification "g" 'ordinary)
(define-math-symbol-classification "h" 'ordinary)
(define-math-symbol-classification "i" 'ordinary)
(define-math-symbol-classification "j" 'ordinary)
(define-math-symbol-classification "k" 'ordinary)
(define-math-symbol-classification "l" 'ordinary)
(define-math-symbol-classification "m" 'ordinary)
(define-math-symbol-classification "n" 'ordinary)
(define-math-symbol-classification "o" 'ordinary)
(define-math-symbol-classification "p" 'ordinary)
(define-math-symbol-classification "q" 'ordinary)
(define-math-symbol-classification "r" 'ordinary)
(define-math-symbol-classification "s" 'ordinary)
(define-math-symbol-classification "t" 'ordinary)
(define-math-symbol-classification "u" 'ordinary)
(define-math-symbol-classification "v" 'ordinary)
(define-math-symbol-classification "w" 'ordinary)
(define-math-symbol-classification "x" 'ordinary)
(define-math-symbol-classification "y" 'ordinary)
(define-math-symbol-classification "z" 'ordinary)

;;; Upper case English letters.

(define-math-symbol-classification "A" 'ordinary)
(define-math-symbol-classification "B" 'ordinary)
(define-math-symbol-classification "C" 'ordinary)
(define-math-symbol-classification "D" 'ordinary)
(define-math-symbol-classification "E" 'ordinary)
(define-math-symbol-classification "F" 'ordinary)
(define-math-symbol-classification "G" 'ordinary)
(define-math-symbol-classification "H" 'ordinary)
(define-math-symbol-classification "I" 'ordinary)
(define-math-symbol-classification "J" 'ordinary)
(define-math-symbol-classification "K" 'ordinary)
(define-math-symbol-classification "L" 'ordinary)
(define-math-symbol-classification "M" 'ordinary)
(define-math-symbol-classification "N" 'ordinary)
(define-math-symbol-classification "O" 'ordinary)
(define-math-symbol-classification "P" 'ordinary)
(define-math-symbol-classification "Q" 'ordinary)
(define-math-symbol-classification "R" 'ordinary)
(define-math-symbol-classification "S" 'ordinary)
(define-math-symbol-classification "T" 'ordinary)
(define-math-symbol-classification "U" 'ordinary)
(define-math-symbol-classification "V" 'ordinary)
(define-math-symbol-classification "W" 'ordinary)
(define-math-symbol-classification "X" 'ordinary)
(define-math-symbol-classification "Y" 'ordinary)
(define-math-symbol-classification "Z" 'ordinary)

;;; Misc symbols treated as ordinary by tex.
;;; These may be reclassified later.
;;; Classifying quantifiers:
(define-math-symbol-classification "forall" 'quantifier)
(define-math-symbol-classification "exists"  'quantifier)

(define-math-symbol-classification "aleph"  'ordinary)
(define-math-symbol-classification "prime"  'ordinary)
(define-math-symbol-classification "hbar"  'ordinary)
(define-math-symbol-classification "emptyset"  'ordinary)

(define-math-symbol-classification "nabla"  'ordinary)
(define-math-symbol-classification "neg"  'negation-operator)

(define-math-symbol-classification "surd"  'ordinary)
(define-math-symbol-classification "flat"  'ordinary)
(define-math-symbol-classification "ell"  'ordinary)
(define-math-symbol-classification "top"  'ordinary)
(define-math-symbol-classification "natural"  'ordinary)
(define-math-symbol-classification "wp"  'ordinary)
(define-math-symbol-classification "bot"  'ordinary)
(define-math-symbol-classification "sharp"  'ordinary)
(define-math-symbol-classification "Re" 'ordinary)
(define-math-symbol-classification "clubsuit" 'ordinary)
(define-math-symbol-classification "Im"  'ordinary)
(define-math-symbol-classification "angle"  'ordinary)
(define-math-symbol-classification "diamondsuit"  'ordinary)
(define-math-symbol-classification "partial"  'ordinary)
(define-math-symbol-classification "triangle"  'ordinary)
(define-math-symbol-classification "heartsuit"  'ordinary)
(define-math-symbol-classification "infty"  'ordinary)
(define-math-symbol-classification "backslash"  'ordinary)
(define-math-symbol-classification "spadesuit"  'ordinary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Large operators.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-math-symbol-classification "sum" 'big-operator)
(define-math-symbol-classification "bigcap" 'big-operator)
(define-math-symbol-classification "bigodot" 'big-operator)
(define-math-symbol-classification "prod" 'big-operator)
(define-math-symbol-classification "bigcup" 'big-operator)
(define-math-symbol-classification "bigotimes" 'big-operator)
(define-math-symbol-classification "coprod" 'big-operator)
(define-math-symbol-classification "bigsqcup" 'big-operator)
(define-math-symbol-classification "bigoplus" 'big-operator)
(define-math-symbol-classification "int" 'big-operator)
(define-math-symbol-classification "bigvee" 'big-operator)
(define-math-symbol-classification "biguplus" 'big-operator)
(define-math-symbol-classification "oint" 'big-operator)
(define-math-symbol-classification "bigwedge" 'big-operator)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; binary operators: These are rarely subscripted
;;; as opposed to the  big operators above.
;;; The above comment is misleading. The big operators are considered
;;; as unary while the following operators are binary. This is the
;;; main difference. The binary operators can be subscripted though
;;; the subscripting in this case means something different.  For the
;;; large operators, the subscript is often an argument to the
;;; function that the operator denotes, in the case of binary
;;; operators, the subscript is merely used to distinguish the
;;; operator eg the subscripted '+' sign in modulo arithmetic.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-math-symbol-classification "+" 'binary-operator)
(define-math-symbol-classification "-" 'binary-operator)
(define-math-symbol-classification "*" 'binary-operator)
(define-math-symbol-classification "/" 'binary-operator)
(define-math-symbol-classification "pm" 'binary-operator)
(define-math-symbol-classification "cap" 'binary-operator)
(define-math-symbol-classification "vee" 'binary-operator)
(define-math-symbol-classification "mp" 'binary-operator)
(define-math-symbol-classification "cup" 'binary-operator)
(define-math-symbol-classification "wedge" 'binary-operator)
(define-math-symbol-classification "setminus" 'binary-operator)
(define-math-symbol-classification "uplus" 'binary-operator)
(define-math-symbol-classification "oplus" 'binary-operator)
(define-math-symbol-classification "cdot" 'binary-operator)
(define-math-symbol-classification "sqcap" 'binary-operator)
(define-math-symbol-classification "ominus" 'binary-operator)
(define-math-symbol-classification "times" 'binary-operator)
(define-math-symbol-classification "sqcup" 'binary-operator)
(define-math-symbol-classification "otimes" 'binary-operator)
(define-math-symbol-classification "ast" 'binary-operator)
(define-math-symbol-classification "triangleleft" 'binary-operator)
(define-math-symbol-classification "oslash" 'binary-operator)
(define-math-symbol-classification "star" 'binary-operator)
(define-math-symbol-classification "triangleright" 'binary-operator)
(define-math-symbol-classification "odot" 'binary-operator)
(define-math-symbol-classification "diamond" 'binary-operator)
(define-math-symbol-classification "wr" 'binary-operator)
(define-math-symbol-classification "dagger" 'binary-operator)
(define-math-symbol-classification "circ" 'binary-operator)
(define-math-symbol-classification "bigcirc" 'binary-operator)
(define-math-symbol-classification "ddagger" 'binary-operator)
(define-math-symbol-classification "bullet" 'binary-operator)
(define-math-symbol-classification "bigtriangleup" 'binary-operator)
(define-math-symbol-classification "amalg" 'binary-operator)
(define-math-symbol-classification "div" 'binary-operator)
(define-math-symbol-classification "bigtriangledown" 'binary-operator)
(define-math-symbol-classification "land"  'binary-operator)
(define-math-symbol-classification "lor" 'binary-operator)
(define-math-symbol-classification "lnot" 'binary-operator)
(define-math-symbol-classification "&" 'binary-operator)
;;; According to the tex book \over and \atop are not treated as
;;; binary math operators for typesetting, but it makes sense to parse
;;; them as such.
(define-math-symbol-classification "atop" 'special-binary-operator)
(define-math-symbol-classification "over" 'special-binary-operator)
(define-math-symbol-classification "choose" 'special-binary-operator)

;;; above are special because tex treats them for all practical
;;; purposes like binary operators with lowest  precedence. 
;;; The following four binary operators from the Latex book man3.tex
;;; do not appear in the tex book.
(define-math-symbol-classification "unlhd" 'binary-operator)
(define-math-symbol-classification "lhd" 'binary-operator)
(define-math-symbol-classification "rhd" 'binary-operator)
(define-math-symbol-classification "unrhd" 'binary-operator)
(define-math-symbol-classification "bmod" 'binary-operator)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-math-symbol-classification "pmod" 'binary-operator)
;;; relations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-math-symbol-classification "=" 'relational-operator)
(define-math-symbol-classification "<" 'relational-operator)
(define-math-symbol-classification ">" 'relational-operator)
(define-math-symbol-classification "leq" 'relational-operator)
(define-math-symbol-classification "geq" 'relational-operator)
(define-math-symbol-classification "equiv" 'relational-operator)
(define-math-symbol-classification "prec" 'relational-operator)
(define-math-symbol-classification "succ" 'relational-operator)
(define-math-symbol-classification "approx" 'relational-operator)
(define-math-symbol-classification "preceq" 'relational-operator)
(define-math-symbol-classification "succeq" 'relational-operator)
(define-math-symbol-classification "propto" 'relational-operator)
(define-math-symbol-classification "ll" 'relational-operator)
(define-math-symbol-classification "gg" 'relational-operator)
(define-math-symbol-classification "asymp" 'relational-operator)
(define-math-symbol-classification "supset" 'relational-operator)
(define-math-symbol-classification "sim" 'relational-operator)
(define-math-symbol-classification "subseteq" 'relational-operator)
(define-math-symbol-classification "simeq" 'relational-operator)
(define-math-symbol-classification "supseteq" 'relational-operator)
(define-math-symbol-classification "sqsubseteq" 'relational-operator)
(define-math-symbol-classification "sqsupseteq" 'relational-operator)
(define-math-symbol-classification "cong" 'relational-operator)
(define-math-symbol-classification "in" 'relational-operator)
(define-math-symbol-classification "ni" 'relational-operator)
(define-math-symbol-classification "bowtie" 'relational-operator)
(define-math-symbol-classification "vdash" 'relational-operator)
(define-math-symbol-classification "dashv" 'relational-operator)
(define-math-symbol-classification "models" 'relational-operator)
(define-math-symbol-classification "smile" 'relational-operator)
(define-math-symbol-classification "mid" 'relational-operator)
(define-math-symbol-classification "doteq" 'relational-operator)
(define-math-symbol-classification "frown" 'relational-operator)
(define-math-symbol-classification "parallel" 'relational-operator)
(define-math-symbol-classification "perp" 'relational-operator)
(define-math-symbol-classification "ne" 'relational-operator)
(define-math-symbol-classification "neq" 'relational-operator)
(define-math-symbol-classification "le" 'relational-operator)
(define-math-symbol-classification "ge" 'relational-operator)
(define-math-symbol-classification "owns" 'relational-operator)
(define-math-symbol-classification "vert" 'ordinary)
(define-math-symbol-classification "iff" 'relational-operator)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; handle negation carefully.
(define-math-symbol-classification "not" 'negation-operator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; arrows: relations?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-math-symbol-classification "leftarrow" 'arrow-operator)
(define-math-symbol-classification "longleftarrow" 'arrow-operator)
(define-math-symbol-classification "uparrow" 'arrow-operator)
(define-math-symbol-classification "Leftarrow" 'arrow-operator)
(define-math-symbol-classification "Longleftarrow" 'arrow-operator)
(define-math-symbol-classification "Uparrow" 'arrow-operator)
(define-math-symbol-classification "rightarrow" 'arrow-operator)
(define-math-symbol-classification "longrightarrow" 'arrow-operator)
(define-math-symbol-classification "downarrow" 'arrow-operator)
(define-math-symbol-classification "Rightarrow" 'arrow-operator)
(define-math-symbol-classification "Longrightarrow" 'arrow-operator)
(define-math-symbol-classification "Downarrow" 'arrow-operator)
(define-math-symbol-classification "leftrightarrow" 'arrow-operator)
(define-math-symbol-classification "longleftrightarrow" 'arrow-operator)
(define-math-symbol-classification "updownarrow" 'arrow-operator)
(define-math-symbol-classification "Leftrightarrow" 'arrow-operator)
(define-math-symbol-classification "Longleftrightarrow" 'arrow-operator)
(define-math-symbol-classification "Updownarrow" 'arrow-operator)
(define-math-symbol-classification "mapsto" 'arrow-operator)
(define-math-symbol-classification "longmapsto" 'arrow-operator)
(define-math-symbol-classification "nearrow" 'arrow-operator)
(define-math-symbol-classification "hookleftarrow" 'arrow-operator)
(define-math-symbol-classification "hookrightarrow" 'arrow-operator)
(define-math-symbol-classification "searrow" 'arrow-operator)
(define-math-symbol-classification "leftharpoonup" 'arrow-operator)
(define-math-symbol-classification "rightharpoonup" 'arrow-operator)
(define-math-symbol-classification "swarrow" 'arrow-operator)
(define-math-symbol-classification "leftharpoondown" 'arrow-operator)
(define-math-symbol-classification "rightharpoondown" 'arrow-operator)
(define-math-symbol-classification "nwarrow" 'arrow-operator)
(define-math-symbol-classification "rightleftharpoons" 'arrow-operator)
(define-math-symbol-classification "to" 'arrow-operator)
(define-math-symbol-classification "gets" 'arrow-operator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; opening delimiters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-math-symbol-classification "lbrack" 'open-delimiter)
(define-math-symbol-classification "lbrace" 'open-delimiter)
(define-math-symbol-classification "langle" 'open-delimiter)
(define-math-symbol-classification "lfloor" 'open-delimiter)
(define-math-symbol-classification "lceil" 'open-delimiter)
(define-math-symbol-classification "(" 'open-delimiter)
(define-math-symbol-classification "[" 'open-delimiter)
(define-math-symbol-classification "{" 'open-delimiter)
(define-math-symbol-classification "left" 'ordinary)

;;; fix how \left handled. 
;;; corresponding closing delimiters:
(define-math-symbol-classification "rbrack" 'close-delimiter)
(define-math-symbol-classification "rbrace" 'close-delimiter)
(define-math-symbol-classification "rangle" 'close-delimiter)
(define-math-symbol-classification "rfloor" 'close-delimiter)
(define-math-symbol-classification "rceil" 'close-delimiter)
(define-math-symbol-classification ")" 'close-delimiter)
(define-math-symbol-classification "]" 'close-delimiter)
(define-math-symbol-classification "}" 'close-delimiter)
(define-math-symbol-classification "right" 'ordinary)
;;; Note: If more matching delimiters added, these will have to be
;;; <(inserted into  define-math-delimiter)> as well.

;;; handling subscript and superscript here:
;;; following need to be reclassified.
(define-math-symbol-classification "^" 'superscript)
(define-math-symbol-classification "_" 'subscript)
;;; misc. 
(define-math-symbol-classification "!" 'ordinary)
(define-math-symbol-classification "." 'ordinary)
(define-math-symbol-classification "," 'ordinary) 
(define-math-symbol-classification "'" 'ordinary)
(define-math-symbol-classification ":" 'binary-operator)
(define-math-symbol-classification "''" 'ordinary)
(define-math-symbol-classification "'' " 'ordinary) 
(define-math-symbol-classification ";" 'ordinary)
(define-math-symbol-classification "cdots" 'ordinary)
(define-math-symbol-classification "ldots" 'ordinary)
(define-math-symbol-classification "\"" 'ordinary)
(define-math-symbol-classification "|" 'ordinary)
(define-math-symbol-classification "qquad" 'ordinary)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mathematical function names taken from the latex book:
;;; <(math functions in man3.tex)>


(define-math-symbol-classification "arccos" 'mathematical-function-name)
(define-math-symbol-classification "cos" 'mathematical-function-name)
(define-math-symbol-classification "csc" 'mathematical-function-name)
(define-math-symbol-classification "exp" 'mathematical-function-name)
(define-math-symbol-classification "ker" 'mathematical-function-name)
(define-math-symbol-classification "limsup" 'mathematical-function-name)
(define-math-symbol-classification "min" 'mathematical-function-name)
(define-math-symbol-classification "sinh" 'mathematical-function-name)
(define-math-symbol-classification "arcsin" 'mathematical-function-name)
(define-math-symbol-classification "cosh" 'mathematical-function-name)
(define-math-symbol-classification "deg" 'mathematical-function-name)
(define-math-symbol-classification "gcd" 'mathematical-function-name)
(define-math-symbol-classification "lg" 'mathematical-function-name)
(define-math-symbol-classification "ln" 'mathematical-function-name)
(define-math-symbol-classification "Pr" 'mathematical-function-name)
(define-math-symbol-classification "sup" 'mathematical-function-name)
(define-math-symbol-classification "arctan" 'mathematical-function-name)
(define-math-symbol-classification "cot" 'mathematical-function-name)
(define-math-symbol-classification "det" 'mathematical-function-name)
(define-math-symbol-classification "hom" 'mathematical-function-name)
(define-math-symbol-classification "lim" 'mathematical-function-name)
(define-math-symbol-classification "log" 'mathematical-function-name)
(define-math-symbol-classification "sec" 'mathematical-function-name)
(define-math-symbol-classification "tan" 'mathematical-function-name)
(define-math-symbol-classification "arg" 'mathematical-function-name)
(define-math-symbol-classification "coth" 'mathematical-function-name)
(define-math-symbol-classification "dim" 'mathematical-function-name)
(define-math-symbol-classification "inf" 'mathematical-function-name)

(define-math-symbol-classification "liminf" 'mathematical-function-name)
(define-math-symbol-classification "max" 'mathematical-function-name)
(define-math-symbol-classification "sin" 'mathematical-function-name)
(define-math-symbol-classification "tanh" 'mathematical-function-name)
                                        ;(define-math-symbol-classification "sqrt" 'mathematical-function-name)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tex treats accents as a macro taking one argument.
;;; instead model it as a math char which is classified as an accent,
;;; and make process-accent pick up the object being accented.
;;; For doing this all accents except sqrt are being classified as
;;; accents.
;;; I should treat  sqrt differently since it is really a mathematical
;;; function with one or possibly two arguments.
;;; for the present classified sqrt as a mathematical-function-name so
;;; the latex kluge of supplying an optional argument to sqrt will not
;;; be caught. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-math-symbol-classification "hat" 'accent)
(define-math-symbol-classification "grave" 'accent)
(define-math-symbol-classification "breve" 'accent)
(define-math-symbol-classification "check" 'accent)
(define-math-symbol-classification "dot" 'accent)
(define-math-symbol-classification "bar" 'accent)
(define-math-symbol-classification "tilde" 'accent)
(define-math-symbol-classification "ddot" 'accent)
(define-math-symbol-classification "acute" 'accent)
(define-math-symbol-classification "vec" 'accent)
                                        ;(define-math-symbol-classification "overline" 'accent)
                                        ;(define-math-symbol-classification "overbrace" 'accent)
(define-math-symbol-classification "widetilde" 'accent)
(define-math-symbol-classification "widehat" 'accent)
;;; Things that go under the math object
                                        ;(define-math-symbol-classification "underline" 'underbar)
                                        ;(define-math-symbol-classification "underbrace" 'underbar)

