;;;   -*-   Mode: LISP -*-    ;;;
 
 

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;


(in-package :aster)

;;; Modified: Thu Apr  2 15:36:36 EST 1992
 
;;; Commenting out some of the structure definitions for
;;; which classes  are being defined.
 
;;; Structure definitions for various parts of a article.
 

;;; Structure: PARSE-TABLE                                   Author: raman
;;; Created: Sun Jan 26 11:21:16 1992

;;; holds names and their associated parsers.

(defstruct (parse-table (:type list))
  (name)
  (parser)
  )

;;; Structure: tex-macro                                    Author: raman
;;; Created: Wed Jan 29 10:34:35 1992

;;; "Holds all the information about a TeX cs == macro

(defstruct (tex-macro (:type list))
  "Holds the information about a TeX control sequence, ie. a macro"
  (name)
  (number-of-args)
  (expand)
  )

;;; Structure: MATH-DELIMITER                                Author: raman
;;; Created: Tue Feb  4 18:34:12 1992

;;; hold a math delimiter 

(defstruct (math-delimiter (:type  list))
  (open)
  (close)
  (name)
  )

