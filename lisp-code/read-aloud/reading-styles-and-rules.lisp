 ;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
 ;;;                                                                       ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)


 ;;; Tue Dec 8 12:17:21 EST 1992 This module replaces the
 ;;; reading-rules module defined in define-reading-rules.lisp.  The
 ;;; old version is kept in a subdirectory.  <(refer to
 ;;; define-reading-rules.lisp )> This version uses a CLOS oriented
 ;;; solution.  Reading rule is now a method which is called by the
 ;;; around method for read-aloud for the document class.  If no
 ;;; reading rule is active for a given object of type document, then
 ;;; a reading rule from the currently effective style is used.  If no
 ;;; style is currently effective, or no rule is defined in the
 ;;; current style for this object, then the default read-aloud method
 ;;; is used.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; Implementation  details:
 ;;; Reading-rule method will specialize on the class of its first
 ;;; argument and use an eql specializer on the second argument which
 ;;; is the rule name.
 ;;;  User level functions:
 ;;; (defmethod reading-rule ... )
 ;;; Alternately use macro def-reading-rule object-name rule-name ...
 ;;; (activate-rule object rule-name )
 ;;; (activate-style style-name)
 ;;; deactivate-rule  rule-name 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; Variable: *CURRENT-READING-STYLE*                        Author: raman
 ;;; Created: Tue Dec  8 12:21:50 1992

(defvar *current-reading-style* nil "Name of current reading style")

 ;;; Function: CURRENT-READING-STYLE                          Author: raman
 ;;; Created: Tue Dec  8 12:22:14 1992

(defun current-reading-style () 
  "Return name of current reading style"
  *current-reading-style*
  )

 ;;; Function: ACTIVATE-STYLE                                 Author: raman
 ;;; Created: Tue Dec  8 14:58:58 1992


(defun activate-style (style) 
  "Activate style as the current reading style"
  (setf *current-reading-style*
        (pushnew  style *current-reading-style*))
  )

(defun add-reading-style (style)
  "Add style to the end of list of currently active styles. "
  (setf *current-reading-style* (append *current-reading-style*
                                        (list style )))
  )

  ;;; Function: DEACTIVATE-STYLE                               Author: raman
  ;;; Created: Tue Dec 22 13:02:21 1992

(defun deactivate-style  (style) 
  "Deactivate this style"
  (setf *current-reading-style*
        (remove style *current-reading-style* ))
  )
 ;;; Variable: *ACTIVE-RULES*                                 Author: raman
 ;;; Created: Tue Dec  8 12:22:51 1992

(defvar *active-rules*
  (make-hash-table :test #'equal )
  "Holds mapping between class names and currently active rules.")

 ;;; Function: ACTIVATE-RULE                                  Author: raman
 ;;; Created: Tue Dec  8 12:23:55 1992

(defun activate-rule (object-name rule-name) 
  "Activate rule rule-name for object object-name"
  (setf (gethash  object-name *active-rules*) rule-name)
  )
 ;;; Function: DEACTIVATE-RULE                                Author: raman
 ;;; Created: Tue Dec  8 15:29:10 1992

(defun deactivate-rule (object-name &optional rule-name ) 
  "Deactivate currently active rule for this object"
  (declare (ignore rule-name))
  (remhash object-name *active-rules*)
  )
 ;;; Method: ACTIVE-RULE                                      Author: raman
 ;;; Created: Tue Dec  8 12:26:40 1992

(defmethod active-rule ((document document))
  "Retrieve active rule if any"
  (let ((object-name (class-name (class-of document ))))
    (gethash object-name *active-rules*))
  )


  ;;; Generic: READING-RULE                                    Author: raman
  ;;; Created: Sun Dec 20 13:39:54 1992

(defgeneric reading-rule (document rule-name)
  #+CLOS (:documentation "Named reading rule for objects ")
  )
 ;;; When an object that is of type document or a subclass of the
 ;;; document class is read, first the around method is called. The
 ;;; around method checks if any reading rule is currently active for
 ;;; this object. If a rule is active and is defined, then that rule is
 ;;; called. If no rule has been activated for this object, or the
 ;;; activated rule is undefined, then the current reading style is
 ;;; checked. If a reading rule has been defined for this object in
 ;;; the current reading style, then that rule is called.
 ;;; The default is to simply called the read-aloud primary method for
 ;;; this object, which can be thought of as the default reading rule.
 ;;; Since the around method  is always called first under the standard
 ;;; method combination, reading rules should  not just directly call
 ;;; read-aloud with the same argument  that was passed to the rule.
 ;;; This will of course generate an infinite loop.  If all that is
 ;;; desired  is to have the default read-aloud method  to be called,
 ;;; then no reading-rule need or should be defined.


;;; Efficiency:
;;; Compute-applicable-methods is too inefficient at run time.
;;; The following function was contributed by
;;; <(refer to net article)>
(proclaim '(inline lookup-effective-method))
(defun lookup-effective-method (gf args)
  ;; Returns the actual function (effective method) that is run when
  ;; GF is applied to ARGS
  ;; You could use this to avoid dispatch overhead if you like.
  (CLOS::CHECKING/CACHING-DCODE-LOOKUP gf args))
(defvar *efficient-styles* nil
  "If t, then use the above efficient lookup-effective-method")


  ;;; Function: LOOKUP-EFFECTIVE-STYLE                         Author: raman
  ;;; Created: Tue Jan 18 15:38:00 1994
(proclaim '(inline lookup-effective-style))
(defun lookup-effective-style (document) 
  "Lookup effective style for this document object."
  (find-if
   #'(lambda(style)
       (lookup-effective-method #'reading-rule
                                (list document style)))
   (current-reading-style))
  )



  ;;; Function: COMPUTE-EFFECTIVE-STYLE                         Author: raman
  ;;; Created: Tue Jan 18 15:38:00 1994
(proclaim '(inline compute-effective-style))
(defun compute-effective-style (document) 
  "Compute effective style for this document object."
  (find-if
   #'(lambda(style)
       (compute-applicable-methods #'reading-rule
                                (list document style)))
   (current-reading-style))
  )

;;; Modified: Fri Jan 14 12:24:15 EST 1994
;;; Trying out efficient solution.
;;; For the present make this behavior dependent on *efficient-styles*
;;; Modified: Wed Apr  7 16:43:51 EDT 1993
;;; Not linking in reading order makes no sense.
;;; <(Backed up version that did this)>
;;; Modified: Tue Jan 18 15:42:58 EST 1994
;;; Using inline function lookup-effective-style

(defmethod read-aloud  :around ((document document))
           "Around method"
           (when document 
                                        ; record for browsing
             (when *read-pointer*
               (setf *previous-read-pointer* *read-pointer* ))
             (setf *read-pointer* document)
             (unless (afl-state document)
               (setf (afl-state document)
                     afl:*current-total-audio-state* ))
             (let* 
                 ((active-rule (active-rule document))
                  (special-pattern (special-pattern document ))
                  (substitution (substitution document ))
                                        ;(return-state nil)
                  (current-style (current-reading-style ))
                  (active-style
                   (if *efficient-styles*
                       (lookup-effective-style document )
                       (compute-effective-style document))))
               (cond
                                        ; read substitution if any. 
                 ((and substitution
                       (find 'read-substitution  current-style))
                  (reading-rule document 'read-substitution ))
                                        ; special pattern have precedence
                 ((and special-pattern
                       (find 'use-special-pattern current-style)
                       (not (eql active-style 'variable-substitution ))
                       (if *efficient-styles*
                           (lookup-effective-method #'reading-rule
                                                    (list document
                                                          special-pattern ))
                           (compute-applicable-methods   #'reading-rule
                                                       (list document
                                                             special-pattern ))))
                  (reading-rule document special-pattern ))
                                        ;active rule  default  call primary 
                 ((equal 'default active-rule )
                  (call-next-method ))
                                        ; rule active and defined then
                 ((and active-rule
                       (if *efficient-styles*
                           (lookup-effective-method
                            #'reading-rule
                            (list document  active-rule ))
                           (compute-applicable-methods
                            #'reading-rule
                            (list document  active-rule ))))
                  (reading-rule document
                                active-rule ))
                                        ; If current applicable style is 
                                        ; 'default then call primary  method
                 ((equal 'default active-style )
                  (call-next-method ))
                                        ; if applicable style found 
                 (  active-style
                  (reading-rule document
                                active-style ))
                                        ; Otherwise use default
                                        ; not setting reading style
                                        ; is same as setting it to 'default
                                        ; primary method
                 (t (call-next-method )))
               (read-aloud-delayed-floats document)
               (when *step-through-math-readings*
                 (step-through-reading document))
               )
             )
           )



;;; { Stepping through math readings:

;;; Switch to determine if we step through a math reading:

  ;;; Variable: *STEP-THROUGH-MATH-READINGS*                            Author: raman
  ;;; Created: Thu Nov 11 12:15:24 1993

(defvar *step-through-math-readings* nil "Switch to determine if we step
through a math reading. ")

;;; This threshold determines if this object is complex enough to be
;;; stepped through:

  ;;; Variable: *MATH-STEP-THRESHOLD*                          Author: raman
  ;;; Created: Thu Nov 11 12:16:12 1993

(defvar *math-step-threshold* 2 "Threshold value for stepping through math objects. Compare weight of a
math object against this threshold. ")

;;; After method on read-aloud  for math-object 
;;; Arranges for math readings to be stepped through.
;;; Actually, this should be an after method on document to be
;;; completely general. 
;;; then:
;;; Is switch on?
;;; is object complex enough?
;;; Are all the children simple?

;;; after method will not work, either on document or math-object for
;;; stepping through readings.

;;; Introduce  a new function and call this from the around method.


  ;;; Method: STEP-THROUGH-READING                             Author: raman
  ;;; Created: Thu Nov 11 13:16:27 1993

(defmethod step-through-reading ((object t))
  "Step through readings. Default is to do nothing."
  nil)


  ;;; Method: STEP-THROUGH-READING                             Author: raman
  ;;; Created: Thu Nov 11 13:16:57 1993

(defmethod step-through-reading ((math-object math-object))
  "Step through math readings. "
  (when (and 
         *step-through-math-readings* 
         (>= (weight math-object) *math-step-threshold* )
         (if  (listp  (children math-object))
              (every #'(lambda(child)
                         (< (weight child) *math-step-threshold* ))
                     (children math-object))
              (< (weight (children math-object))
              *math-step-threshold*)))
    (afl:force-speech)
    (read-char))
  )

;;; }



           


           
 ;;; Macro: DEF-READING-RULE                               Author: raman
 ;;; Created: Tue Dec  8 19:00:47 1992

(defmacro def-reading-rule ((object-name rule-name) &body body) 
  "Define reading rule "
  `(defmethod reading-rule  ((,object-name ,object-name)
                             (rule-name (eql ',rule-name )))
    ,@body)
  )
 ;;; Macro: REM-READING-RULE                                  Author: raman
 ;;; Created: Wed Dec  9 17:17:49 1992
 ;;; Just to make calling remove-method easier. To deactivate a rule,
 ;;; use deactivate-rule.

(defmacro rem-reading-rule (class rule-name) 
  "Remove this reading rule. "
  `(remove-method #'reading-rule
    (find-method #'reading-rule '()
     (append
      (mapcar #'find-class '(,class))
      (list '(eql ,rule-name )))))
  )


  ;;; Created: Wed Dec 23 12:59:19 1992

  ;;; Macro: TRACE-READING-RULE                                Author: raman
  ;;; Created: Sun Jan  3 08:37:55 1993

(defmacro trace-reading-rule (class rule-name) 
  "Trace this reading rule"
  `(trace-method 
    (find-method #'reading-rule '()
     (append
      (mapcar #'find-class '(,class))
      (list '(eql ,rule-name )))))
  )


  ;;; Macro: DOC-READING-RULE                                  Author: raman
  ;;; Created: Sun Jan  3 09:38:44 1993

(defmacro doc-reading-rule (class rule-name) 
  "Return documentation for reading rule"
  `(documentation  
    (find-method #'reading-rule '()
     (append
      (mapcar #'find-class '(, class))
      (list '(eql ,rule-name )))))
  )

;;; removing before and after methods on read-aloud


  ;;; Function: REMOVE-BEFORE-READ-ALOUD                          Author: raman
  ;;; Created: Thu Jan  7 15:34:22 1993

(defun remove-before-read-aloud (class) 
  "Remove before method for this class on read-aloud"
  (remove-method #'read-aloud
                 (find-method #'read-aloud '(:before)
                              (mapcar #'find-class (list class ))))
  )


(defun remove-after-read-aloud (class) 
  "Remove after method for this class on read-aloud"
  (remove-method #'read-aloud
                 (find-method #'read-aloud '(:after)
                              (mapcar #'find-class (list class ))))
  )

(defun remove-around-read-aloud (class) 
  "Remove around method for this class on read-aloud"
  (remove-method #'read-aloud
                 (find-method #'read-aloud '(:around)
                              (mapcar #'find-class (list class ))))
  )



