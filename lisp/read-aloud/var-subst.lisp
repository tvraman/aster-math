;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :aster)
;;{{{ Introduction:

;;; Sun May 23 10:27:59 EDT 1993
 ;;;
;;; Variable substitution:
;;; <(based on notes on variable substitution )>
;;; Approach:
;;; Using variable substitution is just another reading style:
;;;  When variable substitution is performed on a math-object m,
;;; we build a new object of type substituted-expression S.
;;; Object substituted-expression has two slots:
;;; expression:  which contains the transformed expression,
;;; Transformed expression is just the original expression, with the
;;; substitute slot of the appropriate math objects set to the
;;; variable to be substituted. 
;;; Substitutions: A list of substitution objects.
;;; Each substitution object represents a pair:
;;; Variable :: denotes.
;;; The reading rule for substituted-expression does:
;;; Read the expression.
;;; Read the substitutions.
;;; M transformed into S based on the notes.
 
;;; Algorithm:
;;;  When substituting variable 'x' for expression 'm' we do:
;;; build the substitution pair x::m
;;; Set the substitute slot of 'm' to 'x'
;;; Two reading styles added:
;;; variable-substitute
;;; This reading rule for math objects generates the transformed
;;; representation and then calls read-aloud on substituted-expression
;;; read-aloud method on substituted-expression deactivates style
;;; variable-substitute , and activates reading style
;;; read-substitution for the duration of its execution.
;;; In reading style read-substitution, the substitute slot of a math
;;; object is read.
 
;;; When to substitute
;;;  proportional complexity of a math object is the ratio of its
;;;  weight to the ratio of its parent. For a math object that is a
;;;  root, this proportion is unity.
;;; *proportional-complexity-threshold*  is a user specified number
;;; between 0 and 1.
;;; If the weight of an object m is w, and the weight of the entire
;;; expression is wr  then we try to substitute for m if and only  if
;;; $ w > wr\times *proportional-complexity-threshold*$
;;; If *proportional-complexity-threshold* is 1, then we never substitute. 
;;; Setting this to 0 will cause big trouble:-
 

;;}}}
;;{{{ Class definitions:

  ;;; Class: SUBSTITUTED-EXPRESSION                            Author: raman
  ;;; Created: Sun May 23 10:34:01 1993

(defclass substituted-expression  (document)
  ((expression :initform nil :initarg :expression :accessor  expression)
   (substitutions :initform nil :initarg :substitutions
                  :accessor substitutions))
  (:documentation "A substituted expression"))

(defun make-substituted-expression  ()
  (let ((self (make-instance 'substituted-expression )))
    self))


(defun substituted-expression-p (self)
  (typep  self 'substituted-expression ))



  ;;; Class: SUBSTITUTION                                      Author: raman
  ;;; Created: Sun May 23 10:35:53 1993

(defclass substitution  (document)
  ((variable :initform nil :initarg :variable :accessor substitution-variable)
   (denotes :initform nil :initarg :denotes :accessor substitution-denotes))
  (:documentation "A pair variable :: denotes. "))

(defun make-substitution  ()
  (let ((self (make-instance 'substitution )))
    self))


(defun substitution-p (self)
  (typep  self 'substitution ))

;;}}}
;;{{{ Reading rules

(def-reading-rule (math-object read-substitution)
    "Read the substitution if any for math object."
  (let  ((flag nil ))
    (setf flag (deactivate-style 'read-substitution))
    (cond
      ((substitution  math-object)
       (with-reading-state (reading-state 'read-substitution )
         (read-aloud (substitution   math-object ))))
      (t (error "No substitution to read. ")))
    (when flag
      (add-reading-style 'read-substitution))
    )
  )


  ;;; Parameter: *WAIT-BEFORE-READING-SUBSTITUTIONS*           Author: raman
  ;;; Created: Sun Sep 26 10:51:33 1993

(defparameter *wait-before-reading-substitutions* t
  "If t, wait before speaking the substitutions when using the variable
substitution reading style.
If set to an integer, this is used as a timeout, i.e. reading
continues if y is not  pressed.")

;;; Modified: Fri Nov  5 21:11:05 EST 1993
;;; Introducing macros with-reading-style, without-reading-style ...
;;; This makes the reading rule much more readable.
;;; <( old version has been backed up)>
;;; New version worked. But after lisp killed and restarted bombed for
;;; some weird reason. Probably the macro def. Bringing back the old
;;; version for now:
;;; Trying out clean version again:
;;; Clean version works after a complete clean recompile

;;{{{Clean reading rule:

(def-reading-rule (math-object variable-substitution )
    "Read math object after applying variable substitution. "
  (erase-substitutions math-object)
  (let
      ((substituted-expression (variable-substitution math-object )))
    (without-reading-style (variable-substitution)
                           (with-added-reading-style (read-substitution)
                             (read-aloud
                              (expression substituted-expression )))
                           (when (substitutions substituted-expression )
                             (afl:new-block
                              (cond
                                ((and (numberp
                                       *wait-before-reading-substitutions* )
                                      (y-or-n-p
                                        *wait-before-reading-substitutions*))
                                 (afl:tts-queue "where, ")
                                 (read-aloud
                                  (substitutions substituted-expression)))
                                ((numberp ;timeout 
                                  *wait-before-reading-substitutions*))
                                (*wait-before-reading-substitutions*
                                 (afl:tts-queue "where, ")
                                 (read-char)
                                 (read-aloud
                                  (substitutions substituted-expression  )))
                                (t(afl:tts-queue "where, ")
                                  (read-aloud
                                   (substitutions substituted-expression
                                                  ))))))))
  )

;;}}}

;;}}}
;;{{{ read-aloud methods

(defmethod read-aloud ((substitution substitution ))
  "Read aloud a substitution pair"
  (with-reading-state (reading-state  'read-substitution)
    (read-aloud (substitution-variable substitution )))
  (afl:tts-queue "denotes, ")
  (read-aloud (substitution-denotes substitution ))
  (afl:comma-intonation)
  (afl:tts-force)
  )

;;}}}
;;{{{variable-substitution

(defmethod variable-substitution ((math-object math-object))
  "Transform by applying variable substitution. "
  (reset-substitution-id)
  (let
      ((substitutions (complexity-threshold math-object )))
    (make-instance
     'substituted-expression :expression math-object
                             :substitutions substitutions )))


  ;;; Method: ERASE-SUBSTITUTIONS                            Author: raman
  ;;; Created: Wed May 26 18:14:49 1993

(defmethod  erase-substitutions  (( math-object math-object ))
  "Erase previous substitutions for all subparts of this object"
  (when (substitution math-object)
    (setf (substitution math-object)nil))
  (loop for attr in (attributes math-object) do
        (erase-substitutions attr))
  (loop for child in (children math-object ) do
        (erase-substitutions child ))
  )

(defmethod erase-substitutions((math-subformula math-subformula ))
  "Erase substitutions. "
  (erase-substitutions (contents math-subformula ))
  (when (attributes math-subformula)
    (mapc #'erase-substitutions (attributes math-subformula )))
  )

(defmethod erase-substitutions ((object t )) nil)

(defmethod erase-substitutions ((attribute attribute))
  "Erase substitutions. "
  (erase-substitutions (contents attribute ))
  )

  ;;; Parameter: *ATTR-WEIGHT-FACTOR*                          Author: raman
  ;;; Created: Wed May 26 18:09:25 1993

(defparameter *attr-weight-factor* 2.5 "Scale weight of attributes")

  ;;; Function: COLLECT-SUBSTITUTIONS                          Author: raman
  ;;; Created: Tue May 25 12:31:24 1993

(defun collect-substitutions (math-object &key (threshold 1)
                                           (original-weight 0)) 
  "Collect substitutions applied to this object. Returns a list of
substitutions. Side effects object: the substitution slot in the
object or its children are set whenever a substitution is made. "
  (let ((substitutions nil))
    (cond
      ((< (weight math-object) threshold)
       substitutions )
      ((and (listp (children math-object ))
            (> (weight math-object) original-weight) ; redundant subst
            (not (relational-operator-p (parent math-object )))
            (every #'(lambda(x)
                       (<= (weight x) threshold))
                   (children math-object ))
            (every #'(lambda(x)
                       (<= (weight x) (* *attr-weight-factor*  threshold )))
                   (attributes  math-object )))
       (let ((variable (get-substitution-variable math-object )))
         (setf (substitution math-object ) variable)
         (push (make-instance 'substitution
                              :variable  variable 
                              :denotes math-object) substitutions )))
      ((not (listp (children math-object )))
       (collect-substitutions
        (children math-object) :threshold threshold))
      (t                                ; substitute  child or  attribute
       (push
        (delete nil
                (loop
                  for attribute   in (attributes math-object) 
                  when (> (weight attribute)
                          (* *attr-weight-factor*  threshold))
                    collect
                    (collect-substitutions
                     (contents attribute)
                    :threshold  (* *attr-weight-factor* threshold ))))
            substitutions)
       (push
        (delete nil 
                (loop
                  for child in (children math-object) 
                  when (> (weight child) threshold)
                    collect
                    (collect-substitutions
                     child :threshold threshold )))
             substitutions )
       (nreverse (delete nil substitutions)))
      ))
  )


(defvar *subst-id* 0 )

  ;;; Function: GET-SUBSTITUTION-VARIABLE                      Author: raman
  ;;; Created: Tue May 25 17:19:09 1993

(defun get-substitution-variable (object) 
  "Return a variable to substitute for this object"
  (let ((name (if (attribute-p object)
                  (this-attribute-is-called object)
                  (this-argument-is-called object ))))
    (when (and (null name)
               (math-subformula-p (parent object )))
      (loop while (math-subformula-p (parent object)) do
            (setf object (parent object )))
      (setf name (or (this-argument-is-called  object )
                     (this-attribute-is-called  (parent object )))))
    (get-substitution-id (or name
                             (special-pattern  object)
                             (when (stringp  (contents  object ))
                               (afl:get-pronunciation (contents object )))
                             (type-of object )))
    )
  )


  ;;; Parameter: *ABSOLUTE-COMPLEXITY-THRESHOLD*               Author: raman
  ;;; Created: Wed May 26 07:07:11 1993

(defparameter *absolute-complexity-threshold* 5
  "Absolute threshold beyond which substitution applied ")

  ;;; Parameter: *PROPORTIONAL-COMPLEXITY-THRESHOLD*           Author: raman
  ;;; Created: Tue May 25 12:38:22 1993

(defparameter *proportional-complexity-threshold* (/ 1 7)
  "Complexity threshold ratio. ")

  ;;; Function: COMPLEXITY-THRESHOLD              Author: raman
  ;;; Created: Tue May 25 12:36:34 1993

(defun complexity-threshold  (object) 
  "Compute the threshold value for children of this object"
  (let ((proportional-complexity (+ 1 (truncate
                                       (* (weight object )
                                          *proportional-complexity-threshold* )))))
    (max proportional-complexity *absolute-complexity-threshold*)
    )
  )

;;}}}
;;{{{ reading state

(define-reading-state   'read-substitution-voice 
    #'(lambda(state)
        (declare (ignore state ))
        (afl:get-point-in-speech-space 'afl:kid))
  )


(define-reading-state   'read-substitution
    #'(lambda(state)
        (afl:generalized-afl-operator state
                                      '(afl:move-to afl:quickness 100)
                                      '(afl:move-to afl:laryngilization 25)
                                        ;                                      '(afl:move-to  afl:average-pitch 150)
                                        ;                                      '(afl:move-to afl:hat-rise 30)
                                        ;                                      '(afl:move-to afl:stress-rise
                                        ;                                      30)
                                      )
        )
  )



  ;;; Variable: *SUBST-ID-TABLE*                               Author: raman
  ;;; Created: Wed May 26 19:40:02 1993

(defvar *subst-id-table* (make-hash-table :test #'equal)
  "Table for holding substitution identifiers")


  ;;; Function: GET-SUBSTITUTION-ID                            Author: raman
  ;;; Created: Wed May 26 19:41:01 1993

(defun get-substitution-id (name) 
  "Get unique identifier of this name"
  (let ((entry (gethash name *subst-id-table* )))
    (cond
      (entry                            ; already present
       (incf (gethash name *subst-id-table*))
       (format nil " ~a ~a" name entry))
      (t (setf (gethash name  *subst-id-table*) 2)
         (format nil " ~a ~a " name 1 ))
      ))
  )


  ;;; Function: RESET-SUBSTITUTION-ID                          Author: raman
  ;;; Created: Wed May 26 19:47:00 1993

(defun reset-substitution-id  () 
  "Reset substitution id table"
  (setf *subst-id-table* (make-hash-table :test #'equal ))
  )

;;}}}


