;;;   -*-   Mode: LISP -*-    ;;;
 
;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :aster)

(in-package :aster)

;;; Mon Oct 25 13:36:56 EDT 1993
 ;;; Helper functions to help the around method on read-aloud handle
 ;;; floats:

  ;;; Parameter: *CAN-THIS-HAVE-FLOATS*                        Author: raman
  ;;; Created: Mon Oct 25 13:37:38 1993

(defvar *can-this-have-floats*
  (list 'article  'part  'chapter 'section  'subsection 'subsubsection  'paragraph )
  "These document objects may have  readings float to their end.
 This variable is introduced to make function
read-aloud-delayed-floats more efficient.
We check to see if there are any delayed floats to read only if a
document object's type is listed in this variable. ")


  ;;; Function: READ-ALOUD-DELAYED-FLOATS                      Author: raman
  ;;; Created: Mon Oct 25 13:34:52 1993
(defun read-aloud-delayed-floats (document) 
  "Read all the floating objects whose reading has been delayed. "
  (when (find (class-name (class-of document ))
              *can-this-have-floats*)
  (force-if (class-name (class-of document ))))
  )

