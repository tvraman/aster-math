;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;;  tests assignments:
(afl:new-block
 (afl:global-set-state
  (afl:move-to afl:*current-speech-state* 'afl:head-size 110))
 (read-aloud "The head size has been globally set. ")
 )
