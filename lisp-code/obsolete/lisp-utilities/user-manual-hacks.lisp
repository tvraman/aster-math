;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'user)

(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))
;;;Hacks for using user manual.

(defun create-external-user-manual (filename &key (output-format 'text)
				    (output-stream *standard-output*)
                                    (package 'user))
  "Automatically creates a user manual for the functions in a file
that are external to package by 
   collecting the documentation strings and argument lists of the 
   functions and formatting the output nicely. Returns a list of the
   definition types of the forms it couldn't handle. Output-format may
   be either 'TEXT or 'SCRIBE."
  (setq *failed-definition-types* nil)
  (with-open-file (stream filename :direction :input)
    (let ((eof (gensym)))
      (case output-format
	(text (format t "~%;;;")))
      (do ((form (read stream nil eof nil)
		 (read stream nil eof nil)))
	  ((eq form eof)
	   *failed-definition-types*)
	(when (listp form)
	  (external-handle-form-output form output-format
                                       output-stream
                                        package ))))))

(defun external-handle-form-output (form &optional (output-format 'text)
				(stream *standard-output*)
                                (package  *package*))
  "This function takes a form as input and outputs its documentation
if the second element of form is external to package 
   segment to the output stream."
  (when (external-symbol? (second form)  package)
  (let* ((key (first form))
	 (handler-entry (find-doc-handler key)))
    (cond (handler-entry 
	   (let ((handler (first handler-entry))
		 (type (second handler-entry))
		 name args documentation qualifiers name-length
		 (type-pos (- 80 1)))	; 1 for right margin
	     (multiple-value-setq (name args documentation qualifiers) 
		 (funcall handler form))
	     (setq name (format nil "~:@(~A~)~@[~{ ~S~}~]" name qualifiers)
		   name-length (length name)
		   type (format nil "~:@(~A~)" type)
		   args (cond ((stringp args) ; variable
			       (format nil "~S" args))
			      ((null args)
			       "()")
			      ((eq args :blank) "")
			      (t args)))
	     ;; subtract the width of [type]
	     (decf type-pos (+ (length type) 2))
	     (let ((width (- type-pos (+ 1 4 1 ) name-length)))
	       (unless (eq output-format 'text)
		 ;; Add in the width of ";;; " since we use it 
		 ;; only in text mode.
		 (incf width 4))
	       (setq args
		     #+:XP(xp-split-string args width)
		     #-:XP(split-string (format nil "~(~S~)" args) width t)))
	     (case output-format
	       (text
		(output-text-documentation name type args documentation
					name-length type-pos
					stream))
	       (scribe
		(output-scribe-documentation name type args documentation
					     stream)))))
	  ((eq key 'eval-when)
	   (dolist (f (cddr form))
	     (handle-form-output f output-format stream)))
	  (t
	   (pushnew key *failed-definition-types*))))
  ))

;;; Variable: *EXTERNAL-SYMBOLS*                             Author: raman
;;; Created: Thu Sep 10 15:15:01 1992

(defvar *external-symbols* nil "variable holding list of external symbols")

;;; Function: EXTERNAL?                                      Author: raman
;;; Created: Thu Sep 10 15:15:30 1992

(defun external? (symbol )
  "find if symbol external to package"
(find
 (format nil "~a" symbol)
 *external-symbols*
 :key #'symbol-name
 :test #'string=)
  )


;;; Function: SETUP-EXTERNAL-SYMBOLS-LIST                    Author: raman
;;; Created: Thu Sep 10 15:17:33 1992


(defun setup-external-symbols-list (package) 
  "set up list of external symbols in package"
  (let ((package-symbols nil)
        )
    (do-external-symbols (symbol package)
      (setf package-symbols
            (cons symbol package-symbols))
      )
    (setf *external-symbols* package-symbols)
    )
  )


;;; Function: EXTERNAL-SYMBOL?                               Author: raman
;;; Created: Fri Sep 11 14:36:19 1992


(defun external-symbol? (symbol &optional(package *package*)) 
  "Check if symbol is external to package"
  (multiple-value-bind  (existing-symbol  type)
      (find-symbol
       (format nil "~a" symbol)
       (find-package package))
    (and existing-symbol
         (eq :external type))
    ))


;;; Modified: Fri Sep 24 18:01:09 EDT 1993
;;; Introducing a handler for def-reading-rule:
(define-doc-handler def-reading-rule (form)
  "reading rule"
  ;; name arglist documentation-string
  (values(second  (second form))
         (second form)
         (when (stringp (third form)) (third form))))
