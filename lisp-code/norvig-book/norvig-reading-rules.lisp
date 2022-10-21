;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))

(def-reading-rule (asis  asis-skip)
    "interactively skip over code in norvig's book"
  (read-aloud "Want to read code? ")
  (when (yes-or-no-p "want to read code? ")
    (afl:with-pronunciation-mode (:mode :lisp)
    (read-aloud (contents asis ))))
  )
