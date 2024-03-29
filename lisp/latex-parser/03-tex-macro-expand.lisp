;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;


(in-package :aster)


;;; Created: Thu Jan 30 08:52:45 EST 1992

;;; Handles tex macros.


;;; Function: LATEX-EXPAND                                   Author: raman
;;; Created: Thu May  7 19:32:00 1992

(defun latex-expand (text-buffer)
  "process latex logo macro"
  (declare (ignore text-buffer))
  (make-tex-defined-macro :tex-defined-macro-name "latex"
                          :read-as "[l'aat`ehkh]" )
  )

;;; Function: EG-EXPAND                                      Author: raman
;;; Created: Tue May 12 18:45:32 1992

(defun eg-expand (text-buffer)
  "expand eg macro"
  (declare (ignore text-buffer))
  (make-tex-defined-macro :tex-defined-macro-name "eg"
                          :read-as "for [`ehks'aempuhl], " )
  )
;;; Function: IT-EXPAND                                      Author: raman
;;; Created: Thu Jan 30 09:25:14 1992

(defun it-expand (text-buffer)
  "italics macro: side effect buffer passed as argument"
  (setf (buffer-local-env text-buffer)
        (cons   '(font italic)
                (buffer-local-env text-buffer)))
  nil
  )

;;; Function: BF-EXPAND                                      Author: raman
;;; Created: Thu Jan 30 09:26:28 1992

(defun bf-expand (text-buffer)
  "bold face macro: side effect buffer passed as argument"
  (setf (buffer-local-env text-buffer)
        (cons '(font bold)
              (buffer-local-env
               text-buffer))) nil
  )

;;; Function: EM-EXPAND                                      Author: raman
;;; Created: Thu Jan 30 09:27:06 1992

(defun em-expand (text-buffer)
  "emphasize macro: side effect buffer passed as argument"
  (setf (buffer-local-env text-buffer)
        (cons '(font emphasize)
              (buffer-local-env
               text-buffer)))
  nil
  )

;;; Function: LARGE-EXPAND                                   Author: raman
;;; Created: Thu Jan 30 09:27:32 1992

(defun large-expand (text-buffer)
  "large font macro: side effect buffer passed as argument"
  (setf (buffer-local-env text-buffer)
        (cons '(font large)
              (buffer-local-env
               text-buffer)))
  nil
  )

(defun huge-expand (text-buffer)
  "huge font macro: side effect buffer passed as argument"
  (setf (buffer-local-env text-buffer)
        (cons '(font huge)
              (buffer-local-env
               text-buffer)))
  nil
  )

;;; Function: RM-EXPAND                                      Author: raman
;;; Created: Thu Jan 30 09:28:23 1992

(defun rm-expand (text-buffer)
  "roman font macro: side effect buffer passed as argument"
  (setf (buffer-local-env text-buffer)
        (cons '(font roman)
              (buffer-local-env
               text-buffer)))
  nil
  )

;;; Function: TT-EXPAND                                      Author: raman
;;; Created: Fri Feb 21 13:55:41 1992

(defun tt-expand (text-buffer)
  "tt font macro: side effect buffer passed as argument"
  (setf (buffer-local-env text-buffer)
        (cons '(font typewriter)
              (buffer-local-env
               text-buffer)))
  nil
  )

(defun sf-expand (text-buffer)
  "sf font macro: side effect buffer passed as argument"
  (setf (buffer-local-env text-buffer)
        (cons '(font sans-seriph)
              (buffer-local-env
               text-buffer)))
  nil
  )

;;; Function: SC-EXPAND                                      Author: raman
;;; Created: Fri Feb 21 14:00:33 1992

(defun sc-expand (text-buffer)
  "sc macro: side effect buffer passed as argument"
  (setf (buffer-local-env text-buffer)
        (cons '(font smallcaps )
              (buffer-local-env
               text-buffer)))
  nil
  )

;;; Function: SL-EXPAND                                      Author: raman
;;; Created: Fri Feb 21 14:10:48 1992

(defun sl-expand (text-buffer)
  "sl macro side effect buffer passed as argument"
  (setf (buffer-local-env text-buffer)
        (cons '(font slanted  )
              (buffer-local-env
               text-buffer)))
  nil
  )

  ;;; Parameter: *CITATION-MUST-HAVE-A-BLOCK-ARGUMENT*         Author: raman
  ;;; Created: Sun Oct 17 16:12:16 1993

;;; Function: CITE-EXPAND                                    Author: raman
;;; Created: Sat Feb  1 12:00:28 1992

(defun cite-expand ( citation)
  "process citation"
  (assert
   (is-a 'block citation) nil
   "Assert: cite-expand: argument to citation ~a, is not a block"
   citation)
  (make-citation
   :label (process-argument
           citation))
  )

;;; Function: FOOTNOTE-EXPAND                                Author: raman
;;; Created: Sat Feb  1 12:08:40 1992

(defun footnote-expand ( footnote)
  "process footnote"
  (assert
   (is-a 'block footnote) nil
   "Assert: footnote-expand: argument to footnote ~a, is not a block"
   footnote)
  (make-footnote :text
                 (process-argument
                  footnote))
  )

;;; Function: INDEX-EXPAND                                   Author: raman
;;; Created: Mon Oct  5 11:50:20 1992

(defun index-expand (index)
  "Process index term. "
  (assert
   (or (is-a 'block index)
       (is-a 'subformula index))nil
   "Assert: index-expand: argument to index ~a, is not a block"
   index)
  (make-instance 'index-term
                 :contents
                 (process-argument
                  index))
  )

;;; Function: LABEL-EXPAND                                   Author: raman
;;; Created: Sat Feb  1 12:10:33 1992
(defun label-expand ( label)
  "process label"
  (assert
   (or  (is-a 'block label)
        (is-a 'subformula label))nil
   "Assert: label-expand: argument to label ~a, is not a block"
   label)
  (let
      ((contents  (rest label )))
    (setf contents  (mapcar  #'(lambda (unit)
                                 (cond
                                   ((stringp unit ) unit)
                                   ((is-a 'text-number unit) (second unit))
                                   ((is-a 'math-number unit) (second unit))
                                   )) contents ))
    (install-label (make-instance  'label
                                   :contents
                                   (apply #'concatenate 'string  contents )))
    nil
    )
  )

  ;;; Function: REF-EXPAND                                     Author: raman
  ;;; Created: Mon Dec 28 15:08:00 1992

(defun ref-expand (ref)
  "Expand \ref"
  (assert
   (or  (is-a 'block ref)
        (is-a 'subformula ref))nil
   "Assert: ref-expand: argument to label ~a, is not a block"
   ref)
  (let
      ((contents  (rest ref )))
    (setf contents  (mapcar  #'(lambda (unit)
                                 (cond
                                   ((stringp unit ) unit)
                                   ((is-a 'text-number unit) (second unit))
                                   ((is-a 'math-number unit) (second unit))
                                   )) contents ))
    (make-instance 'cross-ref
                   :children (list
                              (apply #'concatenate 'string  contents ) ))
    )
  )

;;; Function: CENTERLINE-EXPAND                              Author: raman
;;; Created: Mon May  4 15:26:08 1992

(defun centerline-expand (centerline)
  "expand centerline macro"
  (assert
   (is-a 'block centerline) nil
   "Assert: centerline-expand: argument to centerline ~a, is not a block"
   centerline)
  (make-centered-text :contents
                      (process-argument
                       centerline))
  )

;;; Function: TITLE-EXPAND                                   Author: raman
;;; Created: Sun Apr 26 18:28:07 1992

(defun title-expand (title)
  "expand title macro."
  (assert
   (is-a 'block title) nil
   "Title must be a block" title)
  (setf (title *document*)
        (process-argument title)))

;;; Function: AUTHOR-EXPAND                                  Author: raman
;;; Created: Sun Apr 26 18:35:12 1992

(defun author-expand (author)
  "process author macro"
  (assert
   (is-a 'block author) nil
   "Assert: author-expand: argument to label ~a, is not a block"
   author)
  (setf (author *document*)
        (process-argument
         author)
        )
  nil
  )

;;; Function: DATE-EXPAND                                    Author: raman
;;; Created: Sun Apr 26 18:36:44 1992

(defun date-expand (date)
  "process date macro"
  (assert
   (is-a 'block date) nil
   "Assert: date-expand: argument to label ~a, is not a block"
   date)
  (setf (date *document*)
        (process-argument
         date)
        )
  nil
  )

;;; Function: ADDRESS-EXPAND                                 Author: raman
;;; Created: Sun Apr 26 18:38:03 1992

(defun address-expand (address)
  "process address macro."
  (assert
   (is-a 'block address) nil
   "Assert: address-expand: argument to label ~a, is not a block"
   address)
  (setf (address *document*)
        (process-argument
         address)
        )
  nil
  )

;;; try to make macro expansion functions fit a pattern. The functions
;;; are already being passed their arguments, they need to process
;;; them either as text or math and build up the right object.
;;; Introducing two auxilliary functions to make this clear.

;;; Function: PROCESS-ARGUMENT                       Author: raman
;;; Created: Sun Oct  4 13:35:33 1992

(defun process-argument (argument)
  "Process argument as text. "
  (process-text
   (make-buffer :contents (list argument ))))


;;; Function: PROCESS-ARGUMENT-AS-MATH                       Author: raman
;;; Created: Sun Oct  4 13:36:04 1992
(defun process-argument-as-math (argument)
  "Process argument as math. "
  (process-math
   (make-buffer :contents (list argument ))))
;;; Following function expects that each of the arguments to the \root
;;; macro are enclosed in  in braces in the tex documents. Tex allows
;;; \root n+1 \of m+n  At present this will not be recognized.

;;; Function: ROOT-EXPAND                                    Author: raman
;;; Created: Thu Sep 17 11:15:13 1992

(defun root-expand (nth-root of m)
  "Process Plain TeX root macro"
  (assert
   (and (is-a 'math-cs  of)
        (string= "of" (math-cs-name of)))
   nil
   " Incorrect arguments to square root tex macro")
  (let ((root-children  (list
                         (process-argument-as-math  nth-root)
                         (process-argument-as-math m)
                         ))
        (self nil ))
    (setf self (make-instance 'generalized-root
                              :contents "root"
                              :type  'root
                              :children root-children
                              ))
    self)
  )

;;; Function: MATHREL-EXPAND                                 Author: raman
;;; Created: Sun Oct  4 09:29:52 1992

(defun mathrel-expand (relational-operator)
  "Handle tex mathrel macro"
  (let
      ((operator (process-argument relational-operator )))
    (make-instance 'math-object
                   :contents operator
                   :type 'relational-operator)
    )
  )

;;; if \dx \dy etc. are defined as integral delimiters, then these
;;; expansion functions have to be hand generated. This is because the
;;; variable of integration is now hidden in the macro name itself.
;;; The good thing would be to have \d{.} and define this as a new
;;; text object.
;;;

;;; Function: DX-EXPAND                                      Author: raman
;;; Created: Tue Nov  3 12:31:07 1992

(defun dx-expand (math-buffer)
  "Expand dx"
  (declare (ignore math-buffer))
  (make-instance 'integral-d
                 :contents "dx"
                 :children (list  "x"))
  )

;;; Function: DY-EXPAND                                      Author: raman
;;; Created: Tue Nov  3 12:31:07 1992

(defun dy-expand (math-buffer)
  "Expand dx"
  (declare (ignore math-buffer))
  (make-instance 'integral-d
                 :contents "dy"
                 :children (list  "y"))
  )

;;; Function: DZ-EXPAND                                      Author: raman
;;; Created: Tue Nov  3 12:31:07 1992

(defun dz-expand (math-buffer)
  "Expand dx"
  (declare (ignore math-buffer))
  (make-instance 'integral-d
                 :contents "dz"
                 :children (list  "z" ))
  )

(defun dt-expand (math-buffer)
  "Expand dt"
  (declare (ignore math-buffer))
  (make-instance 'integral-d
                 :contents "dt"
                 :children (list  "t")))

(define-text-object :macro-name "frac"
  :number-args 2
  :processing-function frac-expand
  :precedence  nil
  :children-are-called (list 'numerator 'denominator )
  :object-name fraction
  :supers (math-object))

(define-text-object :macro-name "gcd"
  :number-args 2
  :processing-function gcd-expand
  :precedence  nil
  :object-name a-gcd
  :supers (math))
(define-tex-macro "dx" 0 'dx-expand)
(define-tex-macro "dy" 0 'dy-expand)
(define-tex-macro "dz" 0 'dz-expand)
(define-tex-macro "dt" 0 'dt-expand)

(define-tex-macro "matrix" 1 'tex-matrix-expand)
(define-tex-macro "pmatrix" 1 'tex-matrix-expand)
;;; TeX matrices can be processed using process-array.

  ;;; Function: TEX-MATRIX-EXPAND                              Author: raman
  ;;; Created: Wed Sep 22 14:06:38 1993

(defun tex-matrix-expand (matrix-contents)
  "Process a TeX matrix "
                                        ; Construct a doubly nested list and hand it off to process-array.
  (let ((input-matrix (rest matrix-contents ))
        (row-list nil)
        (output-matrix nil)
        (current-row nil)
        (current-element nil))
    (loop for element in input-matrix do
      (cond
        ((is-a 'field-separator element)
         (push (nreverse current-element)  current-row)
         (setf current-element nil))
        ((is-a 'newline element)
         (push (nreverse current-element) current-row)
         (push (nreverse current-row) row-list)
         (setf current-element nil
               current-row nil))
        (t (push element current-element))))
    (when current-element
      (push (nreverse current-element) current-row))
    (when current-row
      (push (nreverse current-row) row-list))
    (setf output-matrix (nreverse row-list))
    (process-array (make-buffer
                    :contents(list
                              (push 'array
                                    output-matrix)))
                   )
    )
  )
