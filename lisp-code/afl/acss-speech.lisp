;;;   -*- Syntax: Common-Lisp; Package: afl; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994 by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; export all dimension names.
;;; This defines the Speech-Space in terms of Aural CSS (ACSS).
;;; Dimensions are from Aural CSS (ACSS).

(defstruct acss
  "Aural CSS"
  family gain
  left-volume right-volume
  average-pitch pitch-range
  stress richness
  punctuations
  ) 
;;; }
