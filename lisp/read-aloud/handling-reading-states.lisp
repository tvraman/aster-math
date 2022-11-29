;;;   -*-   Mode: LISP -*-    ;;;
 
 

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :aster)

(in-package :aster)

;;; Contains the representation and accessors for mapping reading
;;; states to afl states.


;;; Variable: *READING-STATE-TABLE*                          Author: raman
;;; Created: Thu Sep  3 14:28:04 1992

(defvar *reading-state-table*
  (make-hash-table :test #'equal  )
  "Association between reading states and afl states")


;;; If not defined returned identity 
;;; Function: DEFINE-READING-STATE                           Author: raman
;;; Created: Thu Sep  3 14:29:55 1992

(defun define-reading-state (state-name  afl-setter) 
  "define afl-setter as the setter for this reading state"
  (setf (gethash state-name  *reading-state-table* )
        afl-setter)
  )

;;; default is identity 
;;; Function: GET-READING-STATE                              Author: raman
;;; Created: Thu Sep  3 14:31:07 1992

(defun get-reading-state (state) 
  "Retrieve state setter for this state"
  (or 
   (gethash state *reading-state-table*)
   #'identity)
  )

  ;;; Function: REMOVE-READING-STATE                           Author: raman
  ;;; Created: Tue Dec 15 10:31:20 1992

(defun remove-reading-state (state-name) 
  "Remove this state from the table of known reading states. "
  (remhash state-name *reading-state-table*)
  )

;;; Macro: READING-STATE                                     Author: raman
;;; Created: Sat Oct 10 11:11:25 1992

(defmacro reading-state (state-name) 
  "Return a point in speech space reached by applying the rendering rule
for state state-name"
  `(funcall
    (get-reading-state ,state-name)
    afl:*current-speech-state*)
  )


;;; Macro: WITH-READING-STATE                                Author: raman
;;; Created: Fri Nov  6 09:46:45 1992

(defmacro with-reading-state (state &body body) 
  "Execute body in a block after setting state"
  `(afl:new-block
    (afl:local-set-state 
     ,state)
    ,@body)
  )




  ;;; Macro: FLET-READING-STATE                                Author: raman
  ;;; Created: Tue Dec 15 10:33:50 1992

(defmacro flet-reading-state (defs &body body)
  "Executes body with locally defined reading states. Analogous to flet."
  `(let
    ((local-state-names (mapcar #'first   ',defs )))
    (unwind-protect
         (progn 
           (loop for def in ',defs 
                 do
                 (apply #'define-reading-state def))
           ,@body)
      (loop for state-name in local-state-names
            do             (remove-reading-state  state-name))
      ))
  )

;;; Function: SWAP-READING-STATES                            Author: raman
;;; Created: Sat Dec  5 12:22:46 1992

(defun swap-reading-states (one two) 
  "Swap reading states. "
  (let ((state-1 (gethash one *reading-state-table*))
        (state-2 (gethash two *reading-state-table* )))
    (setf (gethash one *reading-state-table*) state-2)
    (setf (gethash two *reading-state-table*)  state-1)
    )
  )
