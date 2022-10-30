;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
;;; Modified: Wed Mar 24 15:15:37 EST 1993
;;; Moving to afl package 


;;; Created: Fri Sep 25 11:58:49 EDT 1992
;;; Pronunciations for dectalk.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;{{{text mode

;;; text mode:
(define-pronunciation "[" "left-bracket" :text)
(define-pronunciation "'" " quote " :text)
(define-pronunciation ":" "[_:]" :text)
(define-pronunciation "--" " " :text)
(define-pronunciation "---" "[_,]  " :text)
(define-pronunciation "." "[_.]" :text)
(define-pronunciation "," "[_,]" :text)
(define-pronunciation "!" "[_!]" :text)
(define-pronunciation "?" "[_?]" :text)
(define-pronunciation ";" "[_;]" :text)
(define-pronunciation "(" "[_,]" :text) ;kluge
(define-pronunciation ")" "[_,] " :text) 
(define-pronunciation "u-turn" " yu-turn " :text)
(define-pronunciation "Wed." "Wednesday, " :text)
(define-pronunciation "i."   " eye " :text)
(define-pronunciation "a-lot" "[ey] lot" :text)
(define-pronunciation "prof." " professor [\] " :text)
(define-pronunciation "gries" "grease" :text)
(define-pronunciation "b-lot" "b lot" :text)
(define-pronunciation "knuth" "konooth" :text)
(define-pronunciation "knuth's" "konooth's" :text)
(define-pronunciation "reuse" "re-use" :text)
(define-pronunciation "Phd." "Phd" :text)
(define-pronunciation "PhD." "PhD" :text)
(define-pronunciation "phd." "phd" :text)
(define-pronunciation "cdrom" "cd rom" :text)
(define-pronunciation "intelligibly" "intellijibly" :text)
(define-pronunciation "subspace" "sub-space" :text)
(define-pronunciation "subspaces" "sub-spaces" :text)
;;; Thesis addons:

(define-pronunciation  "rpi" "ar pee i " :text)
(define-pronunciation "ibm" "i b m " :text)
(define-pronunciation "davis" "dayvis" :text)
(define-pronunciation "palo" "pallo" :text)
(define-pronunciation "att" "[`ey']tt " :text)
(define-pronunciation "sgml" " sgm l " :text )
(define-pronunciation "emacs" "eem[']ax " :text)
(define-pronunciation "intonational" "intonaytional" :text)
(define-pronunciation "visually" "visual[iy]" :text)
(define-pronunciation "globally" "global[iy]" :text)
(define-pronunciation "subprocess" "sub-process" :text)
(define-pronunciation "acknowledgements" "acknowledge-ments" :text)
(define-pronunciation "rubik" "roobik" :text)
(define-pronunciation "rubik's"  "roobiks" :text)
(define-pronunciation "html" " htm l " :text)
(define-pronunciation "pune" "POOna" :text)
(define-pronunciation "csrvl" "csr vl" :text)
(define-pronunciation "cobegin" "co-begin" :text)
(define-pronunciation "mph" "miles per  hour " :text)
(define-pronunciation "screenreader" "screen reader " :text)
(define-pronunciation "shakespeare" "shake spear " :text)
(define-pronunciation "figure" "figur" :text)
(define-pronunciation "ny" " new york " :text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;}}}
;;{{{Math mode

;;; Math mode:
;;; Capital letters.  
(define-pronunciation "A" "cap [`ey]" :math)
(define-pronunciation "B"  "cap b" :math)
(define-pronunciation "C"  "cap C" :math)
(define-pronunciation "D"  "cap D" :math)
(define-pronunciation "E"  "cap E" :math)
(define-pronunciation "F"  "cap F" :math)
(define-pronunciation "G"  "cap G" :math)
(define-pronunciation "H"  "cap H" :math)
(define-pronunciation "I"  "cap I" :math)
(define-pronunciation "J"  "cap J" :math)
(define-pronunciation "K"  "cap K" :math)
(define-pronunciation "L"  "cap L" :math)
(define-pronunciation "M"  "cap M" :math)
(define-pronunciation "N"  "cap N" :math)
(define-pronunciation "O"  "cap O" :math)
(define-pronunciation "P"  "cap P" :math)
(define-pronunciation "Q"  "cap Q" :math)
(define-pronunciation "R"  "cap R" :math)
(define-pronunciation "S"  "cap S" :math)
(define-pronunciation "T"  "cap T" :math)
(define-pronunciation "U"  "cap U" :math)
(define-pronunciation "V"  "cap V" :math)
(define-pronunciation "W"  "cap W" :math)
(define-pronunciation "X"  "cap X" :math)
(define-pronunciation "Y"  "cap Y" :math)
(define-pronunciation "Z"  "cap Z" :math)

;;; Lower case letters:
(define-pronunciation "a" "[`ey]" :math)
;;; TeX characters and symbols:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-pronunciation "imath" "i" :math)
(define-pronunciation "jmath" "j" :math)

;;; Lower case Greek letters:

(define-pronunciation  "varrho"   "var rho" :math)
(define-pronunciation  "varsigma"   "var sigma" :math)
(define-pronunciation  "varepsilon"   "var epsilon" :math) 
(define-pronunciation "varphi"   "var phi" :math)
(define-pronunciation  "chi"    "ki" :math) 
(define-pronunciation  "varpi"   "var pi" :math)
(define-pronunciation  "vartheta"   "var theta" :math) 
(define-pronunciation "omega"  "ommega" :math)
;; the rest of the lower case greek letters spoken correctly. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Upper case Greek letters:

(define-pronunciation "Gamma"  " cap Gamma" :math)
(define-pronunciation "Xi"  " cap Xi" :math)
(define-pronunciation "Phi"  " cap Phi" :math)
(define-pronunciation "Delta"  " cap Delta" :math)
(define-pronunciation "Pi"  " cap Pi" :math)
(define-pronunciation "Psi"  " cap Psi" :math)
(define-pronunciation "Theta"  " cap Theta" :math)
(define-pronunciation "Sigma"  " cap Sigma" :math)
(define-pronunciation "Omega"  " cap Ommega" :math)
(define-pronunciation "Lambda"  " cap Lambda" :math)
(define-pronunciation "Upsilon"  " cap Upsilon" :math)

;;; Misc symbols:
(define-pronunciation "infty" "infinity" :math)
(define-pronunciation "Re" "real" :math)
(define-pronunciation "Im" "imaginary" :math)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Large operators: Usually unary.

(define-pronunciation  "sum"   "summation" :math)
(define-pronunciation  "bigcap"   "intersection" :math)
(define-pronunciation  "bigodot"   "circle dot" :math)
(define-pronunciation  "prod"   "product" :math)
(define-pronunciation  "bigcup"   "union" :math)
(define-pronunciation  "bigotimes"   "circle times" :math)
(define-pronunciation  "coprod"  "co product" :math)
(define-pronunciation  "bigsqcup"   "Square Union symbol" :math)
(define-pronunciation  "bigoplus"   "circle plus" :math)
(define-pronunciation  "int"  "integral " :math)
(define-pronunciation  "bigvee"   " Disjunction" :math)
(define-pronunciation "uplus" "you plus" :math)
(define-pronunciation  "oint"   "contour integral" :math)
(define-pronunciation  "bigwedge"   "wedge" :math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; binary operators:

(define-pronunciation  "-"  "minus" :math)
(define-pronunciation  "*"   "star" :math)
(define-pronunciation  "/"   "divided-by" :math)
(define-pronunciation  "pm"   "plus or minus" :math)
(define-pronunciation  "cap"   "intersection" :math)
(define-pronunciation  "vee"   "union" :math)
(define-pronunciation  "mp"   "minus or plus" :math)
(define-pronunciation  "cup"   "union" :math)
(define-pronunciation  "wedge"   "wedge" :math)
(define-pronunciation  "setminus"   "set minus" :math)
(define-pronunciation  "uplus"   "u plus" :math)
(define-pronunciation  "oplus"   "circle plus" :math)
(define-pronunciation  "cdot"   "dot" :math)
(define-pronunciation  "sqcap"   "square cap" :math)
(define-pronunciation  "ominus"   "circle minus" :math)
(define-pronunciation  "times"   "times" :math)
(define-pronunciation "mult" " times " :math)
(define-pronunciation  "sqcup"   "square cup" :math)
(define-pronunciation  "otimes"   "circle times" :math)
(define-pronunciation  "ast"   "asterix" :math)
(define-pronunciation  "triangleleft"   "triangle left" :math)
(define-pronunciation  "oslash"   "circle slash" :math)
(define-pronunciation  "star"   "star" :math)
(define-pronunciation  "triangleright"   "triangleright" :math)
(define-pronunciation  "odot"   "circle dot" :math)
(define-pronunciation  "diamond"   "diamond" :math)
(define-pronunciation  "wr"   "weirstrass" :math)
(define-pronunciation  "dagger"   "dagger" :math)
(define-pronunciation  "circ"   "circle" :math)
(define-pronunciation  "bigcirc"   "big circle" :math)
(define-pronunciation  "ddagger"   "big dagger" :math)
(define-pronunciation  "bullet"   "bullet" :math)
(define-pronunciation  "bigtriangleup"   "big triangle up" :math)
(define-pronunciation  "amalg"   "amalg" :math)
(define-pronunciation  "div"   "divided by" :math)
(define-pronunciation  "bigtriangledown"   "big triangle down" :math)
(define-pronunciation  "land"    "logical and" :math)
(define-pronunciation  "lor"   "logical or" :math)
(define-pronunciation  "lnot"   "logical not" :math)
(define-pronunciation  "&"   "and" :math)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Relationals:
(define-pronunciation  "="  "equals" :math)
(define-pronunciation  "<"   "lessthan" :math)
(define-pronunciation  ">"   "greater than" :math)
(define-pronunciation  "leq"   "less than or equal to" :math)
(define-pronunciation  "geq"   "greater than or equal to" :math)
(define-pronunciation  "equiv"   "equivalent to" :math)
(define-pronunciation  "prec"   "precedes" :math)
(define-pronunciation  "succ"   "succeeds" :math)
(define-pronunciation  "approx"   "is approximately" :math)
(define-pronunciation  "preceq"   "precedes or equals" :math)
(define-pronunciation  "succeq"   "succeeds or equals" :math)
                                        ; "propto" 
(define-pronunciation  "ll"   "less than less than" :math)
(define-pronunciation  "gg"   "greater than greater than" :math)
(define-pronunciation  "asymp"   "asymptotically" :math)
(define-pronunciation  "subset"   "subset" :math)
(define-pronunciation  "sim"   "similar to" :math)
(define-pronunciation  "subseteq"   "subset or equals" :math)
(define-pronunciation  "simeq"   "similar or equal to" :math)
(define-pronunciation  "sqsubseteq"   "square subset or equals" :math)
(define-pronunciation "supset" "super set" :math)
(define-pronunciation "sqsupset" "square super set" :math)
(define-pronunciation "sqsupseteq" "square super set or equals" :math)
(define-pronunciation "supseteq" "super set or equals" :math)
(define-pronunciation  "cong"   "congruent to" :math)
(define-pronunciation  "in"   "belongs to" :math)
(define-pronunciation  "ni"   "not in" :math) ;check
(define-pronunciation  "bowtie"   "low tie" :math)
(define-pronunciation  "vdash"   "vertical dash" :math)
(define-pronunciation  "dashv"   "dash" :math)
(define-pronunciation  "models"   "models" :math)
(define-pronunciation  "smile"   "smile" :math)
(define-pronunciation  "mid"   " vertical line " :math)
(define-pronunciation  "doteq"   " equals dot" :math)
(define-pronunciation  "frown"   "frown" :math)
(define-pronunciation  "parallel"   "parallel" :math)
(define-pronunciation  "perp"   "orthogonal complement" :math)
(define-pronunciation  "ne"   "not equal to" :math)
(define-pronunciation  "neq"   "[\"]not equal to," :math)
(define-pronunciation  "le"   "less than or equal to" :math)
(define-pronunciation  "ge"   "greater than or equal to" :math)
(define-pronunciation  "owns"   "owns" :math)

;;; negation:
(define-pronunciation "not" "not" :math)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arrows:

(define-pronunciation "leftarrow"   "left arrow" :math)
(define-pronunciation "longleftarrow"  "long left arrow" :math)
(define-pronunciation  "uparrow"   "up arrow" :math)
(define-pronunciation  "Leftarrow"   " bold left arrow" :math)
(define-pronunciation  "Longleftarrow"   "bold long left arrow" :math)
(define-pronunciation  "Uparrow"   "bold up arrow" :math)
(define-pronunciation  "rightarrow"   "right arrow" :math)
(define-pronunciation  "longrightarrow"   "long right arrow" :math)
(define-pronunciation  "downarrow"   "down arrow" :math)
(define-pronunciation  "Rightarrow"   "bold right arrow" :math)
(define-pronunciation  "Longrightarrow"   "bold long right arrow" :math)
(define-pronunciation  "Downarrow"   "bold down arrow" :math)
(define-pronunciation  "leftrightarrow"   "left right arrow" :math)
(define-pronunciation  "longleftrightarrow"   "long left right arrow" :math)
(define-pronunciation  "updownarrow"   "up down arrow" :math)
(define-pronunciation  "Leftrightarrow"   "bold left right arrow" :math)
(define-pronunciation  "Longleftrightarrow"
    " bold long left right arrow" :math)
(define-pronunciation  "Updownarrow"   "bold up down arrow" :math)
(define-pronunciation  "mapsto"   "maps to" :math)
(define-pronunciation  "longmapsto"   "long maps to" :math)
(define-pronunciation  "nearrow"    "north east arrow" :math)
(define-pronunciation  "hookleftarrow"   "hook left arrow" :math)
(define-pronunciation  "hookrightarrow"   "hook right arrow" :math)
(define-pronunciation  "searrow"   "south east arrow" :math)
(define-pronunciation  "leftharpoonup"   "left harpoon up" :math)
(define-pronunciation  "rightharpoonup"   "right harpoon up" :math)
(define-pronunciation  "swarrow"   "south west arrow" :math)
(define-pronunciation  "leftharpoondown"   "left harpoon down" :math)
(define-pronunciation  "rightharpoondown"   "right harpoon down" :math)
(define-pronunciation  "nwarrow"   "north west arrow" :math)
(define-pronunciation  "rightleftharpoons"  " right left harpoons" :math)
(define-pronunciation  "to"   " goes to " :math)
(define-pronunciation  "gets"   "gets" :math)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Delimiters:
;;; opening delimiters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-pronunciation "["  "left bracket" :math)
(define-pronunciation  "lbrack"   " left bracket" :math)
(define-pronunciation  "lbrace"   " left brace" :math)
(define-pronunciation  "langle"   " left angle bracket" :math)
(define-pronunciation  "lfloor"   " left floor bracket" :math)
(define-pronunciation  "lceil"   " left ceiling bracket" :math)
(define-pronunciation  "("   " left paran" :math)
(define-pronunciation  "{"   " left brace" :math)
(define-pronunciation  "left"   "" :math)
;;; corresponding closing delimiters:
(define-pronunciation  "rbrack"   " right bracket" :math)
(define-pronunciation  "rbrace"   " right brace" :math)
(define-pronunciation  "rangle"   " right angle bracket" :math)
(define-pronunciation  "rfloor"   " right floor bracket" :math)
(define-pronunciation  "rceil"   " right ceiling bracket" :math)
(define-pronunciation  "]"    "right bracket" :math)
(define-pronunciation  "}"    "right brace" :math)
(define-pronunciation  "right"   "" :math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Punctuation in math:
(define-pronunciation  "!"   " factorial, " :math)
(define-pronunciation  "."   " dot" :math)
(define-pronunciation  ","   " comma" :math)
(define-pronunciation  "'"   " prime" :math)
(define-pronunciation  ":"   " colon" :math)
(define-pronunciation  "''"   " double prime" :math)
(define-pronunciation  ";"   " semi colon" :math)
(define-pronunciation  "cdots" "[_,] and so on [_,] " :math)
(define-pronunciation  "ldots"   " ellipses" :math)
(define-pronunciation  "\""   " backslash" :math)
(define-pronunciation  "|"   " pipe" :math)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mathematical function names taken from the latex book:
;;; <(math functions in man3.tex)>

(define-pronunciation  "arccos"   " cosine inverse" :math)
(define-pronunciation  "cos"   " cosine" :math)
(define-pronunciation  "csc"   " csc" :math)
(define-pronunciation  "exp"   " exponential" :math)
(define-pronunciation  "ker"   " kernel" :math)
(define-pronunciation  "limsup"   " lim sup" :math)
(define-pronunciation  "min"   " min" :math)
(define-pronunciation  "sinh"   " sine h" :math)
(define-pronunciation  "arcsin"   " sine inverse" :math)
(define-pronunciation  "cosh"   " cos h" :math)
(define-pronunciation  "deg"   " degrees" :math)
(define-pronunciation  "gcd"   " gcd" :math)
(define-pronunciation  "lg"   " log" :math)
(define-pronunciation  "ln"   " natural log" :math)
(define-pronunciation  "Pr"   " pr" :math)
(define-pronunciation  "sup"   " suprimum
" :math)
(define-pronunciation  "arctan"   " tan inverse" :math)
(define-pronunciation  "cot"   " co tangent" :math)
(define-pronunciation  "det"   " determinant" :math)
(define-pronunciation  "hom"   " homomorphism" :math)
(define-pronunciation  "lim"   " limit" :math)
(define-pronunciation  "log"   " log" :math)
(define-pronunciation  "sec"   " sechant" :math)
(define-pronunciation  "tan"   " tangent" :math)
(define-pronunciation  "arg"   " argument" :math)
(define-pronunciation  "coth"   " hyperbolic co tangent" :math)
(define-pronunciation "arctanh" "hyperbolic tan inverse" :math)
(define-pronunciation "arcsinh" "hyperbolic sine inverse" :math)
(define-pronunciation "arccosh" "hyperbolic cosine inverse" :math)
(define-pronunciation  "dim"   " dimension" :math)
(define-pronunciation  "inf"   " infimum" :math)
(define-pronunciation  "liminf"   " limit inferior" :math)
(define-pronunciation  "max"   " max" :math)
(define-pronunciation  "sin"   " sine" :math)
(define-pronunciation  "tanh"   " tan h" :math)
(define-pronunciation  "sqrt"   " square root" :math)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accents: 
(define-pronunciation  "hat"   " hat" :math)
(define-pronunciation  "grave"   " graav" :math)
(define-pronunciation  "breve"   " brev" :math)
(define-pronunciation  "check"   " hacheck" :math)
(define-pronunciation  "dot"   " dot" :math)
(define-pronunciation  "bar"   " bar" :math)
(define-pronunciation  "tilde"   " tilde" :math)
(define-pronunciation  "ddot"   " bold dot" :math)
(define-pronunciation  "acute"   " acute" :math)
(define-pronunciation  "vec"   " vector" :math)
(define-pronunciation  "overline"   " over line" :math)
(define-pronunciation  "overbrace"   " over brace" :math)
(define-pronunciation  "widetilde"   " wide tilde" :math)
(define-pronunciation  "widehat"   " wide hat" :math)

;;; Things that go under the math object
(define-pronunciation  "underline"   " under line" :math)
(define-pronunciation  "underbrace"   " under brace" :math)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-pronunciation "iff" "if and only if" :math)
(define-pronunciation "bmod" "mod" :math)
(define-pronunciation "pmod" "modulo" :math)
(define-pronunciation "qquad" "where" :math)
(define-pronunciation "emptyset" "the empty set" :math)
;;;

(define-pronunciation "dy" "d y" :math)
;(define-pronunciation "juxtaposition" " " :math)
(define-pronunciation "atop" " and [']below that, " :math)
(define-pronunciation "paren" "paran" :math)

;;}}}
;;{{{ lisp mode

(define-pronunciation "(" " open paranth, " :lisp)
(define-pronunciation ")"  " close paranth, " :lisp)

;;}}}
