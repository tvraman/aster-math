;;;   -*-   Mode: LISP -*-    ;;;
 
;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

(in-package :aster)
 

;;; processing math mode of tex.

;;; Function: PROCESS-MATH                                   Author: raman
;;; Created: Sat Feb  1 12:27:35 1992

(defun process-math(math-buffer &optional (termination-condition? #'end-of-buffer?)) 
  "process math stream"
  (let
      ((processed-math nil))
    (loop for token = (lookat-current-entry math-buffer)
          until (funcall termination-condition?  math-buffer)
          do                            ;body advances pointer
                                        (cond
                                          ((end-of-buffer? math-buffer)
                                           (error
                                            "While processing math
 unexpected end reached before closing delimiter"))
                                        ;((is-a 'field-separator token) (advance-pointer math-buffer))
                                          ((open-delimiter? token)
                                           (push   (process-delimited-expression
                                                    math-buffer)
                                                   processed-math ))
                                          (t (push   (mathify (funcall (get-parser token :math-flag t)
                                                                       math-buffer ))
                                                     processed-math )))
          finally (return
                    (inf-to-pre (collapse (nreverse processed-math ))))
          ))
  )


;;; Function: IS-A-SPACE?                                    Author: raman
;;; Created: Mon Sep 28 08:49:23 1992

(defun is-a-space? (token) 
  "Check if token is a space.  "
  (and (stringp token)
       (equal " " token))
  )

;;; Function: MATHIFY                                        Author: raman
;;; Created: Mon Sep 21 20:06:30 1992

(defun mathify (object) 
  "mathify object"
  (if  (typep  object 'math-object) 
       object
       (make-instance 'math-object
                      :contents  object))
  )


;;; Function: COLLAPSE                                       Author: raman
;;; Created: Wed Sep 16 15:06:01 1992


(defun collapse (processed-math) 
  "collapse  math objects"
  (if (>  (length processed-math) 1)
      (collapse-subscripts-and-superscripts processed-math)
      processed-math)
  )

;;; Function: PROCESS-DELIMITED-EXPRESSION                   Author: raman
;;; Created: Tue Feb  4 18:48:12 1992
(defun process-delimited-expression (math-buffer) 
  "process expression delimited by delimiter at front of buffer"
  (let*
      ((token (lookat-current-entry math-buffer ))
       (delimiter  (open-delimiter? token))
       (close-delimiter  nil)
       (processed-expression nil)
       )
    (setf  close-delimiter
	   (math-delimiter-close delimiter))
    (setf processed-expression
	  (process-math
	   (advance-pointer math-buffer)
	   #'(lambda(x)
	       (or 
		(equal  close-delimiter
			(lookat-current-entry x))
		(close-delimiter? (lookat-current-entry x))
		(end-of-buffer? x)))))
    ;; Now see what matched.
    (let
	((matched (lookat-current-entry math-buffer)))
      (cond
	((end-of-buffer? math-buffer)   ; unmatched delimiter
         (make-instance 'delimited-expression
                        :contents  (list
                                    (math-delimiter-open delimiter ))
                        :open-delimiter  (math-delimiter-open delimiter )
                        :children (list processed-expression)
                        :type 'unmatched-delimiter))
	((equal   matched 
		  close-delimiter)
	 (pop-current-entry math-buffer)
         (create-delimited-expression 
          :type 'delimited-expression
          :contents 
          (math-delimiter-name
           delimiter )
          :open-delimiter
          (math-delimiter-open delimiter)
          :close-delimiter
          (math-delimiter-close delimiter)
          :children (list processed-expression )))
	(( close-delimiter?  matched )
	 (pop-current-entry math-buffer)
         (make-instance 'delimited-expression
                        :type 'mismatched-delimiters
                        :contents (list  
                                   (math-delimiter-open
                                    delimiter)
                                   matched )
                        :open-delimiter (math-delimiter-open
                                         delimiter)
                        :close-delimiter matched
                        :children (list processed-expression)))
	(t (error "processed-delimiter: Do not know what happened!
matched ~a "
		  matched))
	)
      ))
  )



;;; Function: OPEN-DELIMITER?                                     Author: raman
;;; Created: Wed Feb 19 10:40:56 1992

(defun open-delimiter? (token) 
  "Checks if token is a delimiter."
  (find token
	*table-of-math-delimiters*
	:key #'math-delimiter-open
	:test #'equal)
  )


;;; Function: CLOSE-DELIMITER?                               Author: raman
;;; Created: Wed Feb 19 11:05:24 1992

(defun close-delimiter? (token) 
  "checks if token is a close delimiter"
  (find token
	*table-of-math-delimiters*
	:key #'math-delimiter-close
	:test #'equal)  
  )



;;; Function: GET-MATH-PARSER                                Author: raman
;;; Created: Tue Feb 25 12:39:43 1992

(defun get-math-parser (token) 
  "Gets appropriate math mode parsing function. "
  (let
      ((parser 
	(parse-table-parser 
	 (find
	  (what-is? token)
	  *processing-function-table*
	  :key #'parse-table-name )
	 )
	 ))
    (cond
      ((null parser ) (error "parser for ~a not found in parse table " token))
      (t parser)))
  )
;;; Function: MARK-AS-PROCESSED                              Author: raman
;;; Created: Mon Feb 10 09:55:02 1992

(defun mark-as-processed (token) 
  "mark token as having been processed"
  (if 
   (processed? token)
   token 
   (cons 'processed token)
   )
  )


;;; Function: PROCESSED?                                     Author: raman
;;; Created: Mon Feb 10 09:57:51 1992

(defun processed? (token) 
  "see if token is tagged as having been processed"
  (if 
   (and
    (listp token)
    (eq 'processed
	(first token)))
   token)
  )



;;; Function: PROCESS-MATH-CS                                Author: raman
;;; Created: Thu Feb 13 14:25:41 1992


(defun process-math-cs (math-buffer) 
  "process a tex control sequence in math mode"
  (let
      ((macro-name (math-cs-name 
		    (lookat-current-entry math-buffer ))))
    (cond
                                        ;user defined macros have precedence
                                        ; using math classification for handling a large number of tex
                                        ; control sequences is very convenient, but can mess up user
                                        ; defined macros that already have a math classification, so
                                        ; check first.  e.g. single letters
      ((and (not (defined-tex-macro-p  macro-name ))
            (lookup-math-classification macro-name))
       (funcall
	(get-parser macro-name :math-flag t)
	math-buffer))
      (t (let ((result (expand-tex-macro math-buffer)))
           (if   (math-object-subtype-p result)  
                 result
                 (when result 
                   (make-instance 'math-object
                                  :contents result
                                  :type 'tex-macro)))))
      )
    ))

;;; Function: PROCESS-SUBFORMULA                             Author: raman
;;; Created: Thu Feb 13 14:30:43 1992

(defun process-subformula (math-buffer)
  "Process a subformula "
  (let ((contents nil)
        (buffer-contents (rest
                          (pop-current-entry math-buffer ))))
    (when buffer-contents 
      (setf contents (process-math
                      (make-buffer :contents buffer-contents ))))
    (make-instance 'math-subformula 
                   :type 'subformula 
                   :contents (or contents ;empty subformula? 
                                 " "))
    )
  )


;;; Function: GET-MATH-OBJECT!                               Author: raman
;;; Created: Tue Mar  3 22:19:11 1992
;;; Used by processing functions to peel off the math-cs marker
;;; If not a math-cs, then object at the current pointer position
;;; is assumed to be a simple math object, namely a string
;;; and handed back to caller
;;; thus assuming that the caller knows what he is doing.

(defun get-math-object! (math-buffer) 
  "Get the math object at front of buffer modifying buffer pointer"
  (let
      ((token (pop-current-entry math-buffer))
       )
                                        ;    (list
    (if
     (is-a 'math-cs token)
     (math-cs-name token)
     token))
                                        ;  )
  )

 
;;; processing functions for the math symbols classified in
;;; math-classification.lisp
;;; At present all these functions look alike,
;;; but they will evolve differently.
 
;;; Current classification types:
;;; ordinary
;;; large-operator
;;; binary-operator
;;; relational-operator
;;; negation-operator
;;; arrow-operator
;;; open-delimiter
;;; close-delimiter
 

;;; Function: PROCESS-MATH-STRING                               Author: raman
;;; Created: Thu Feb 27 22:01:53 1992

(defun process-math-string (math-buffer) 
  "Process an math-string math symbol"
  (make-instance 'math-object
                 :type 'math-string
                 :contents (get-math-object! math-buffer))
  )

;;; Function: PROCESS-ORDINARY                               Author: raman
;;; Created: Thu Feb 27 22:01:53 1992

(defun process-ordinary (math-buffer) 
  "Process an ordinary math symbol"
  (make-instance 'ordinary
                                        ;                 :type 'ordinary
                 :contents (get-math-object! math-buffer))
  )

;;; Function: PROCESS-QUANTIFIER                               Author: raman
;;; Created: Thu Feb 27 22:01:53 1992

(defun process-quantifier (math-buffer ) 
  "Process an quantifier math symbol"
  (make-instance 'quantifier
                                        ;                 :type 'quantifier
                 :contents (get-math-object! math-buffer))
  )




;;; Function: PROCESS-BIG-OPERATOR                         Author: raman
;;; Created: Thu Feb 27 22:06:23 1992

(defun process-big-operator (math-buffer) 
  "Process a large operator"
  (create-big-operator
   :contents (get-math-object! math-buffer))
  )


;;; Function: PROCESS-BINARY-OPERATOR                         Author: raman
;;; Created: Thu Feb 27 22:06:23 1992

(defun process-binary-operator (math-buffer) 
  "Process a binary operator"
  (make-instance 'binary-operator
                                        ;                 :type 'binary-operator
                 :contents (get-math-object! math-buffer))
  )

;;; Function: PROCESS-RELATIONAL-OPERATOR                         Author: raman
;;; Created: Thu Feb 27 22:06:23 1992

(defun process-relational-operator (math-buffer) 
  "Process a relational operator"
  (make-instance 'relational-operator
                                        ;                 :type 'relational-operator
                 :contents (get-math-object! math-buffer))
  )

;;; Function: PROCESS-NEGATION-OPERATOR                         Author: raman
;;; Created: Thu Feb 27 22:06:23 1992

(defun process-negation-operator (math-buffer) 
  "Process a negation operator"
  (make-instance 'negation-operator
                                        ;                 :type 'negation-operator
                 :contents (get-math-object! math-buffer))
  )

;;; Function: PROCESS-ARROW-OPERATOR                         Author: raman
;;; Created: Thu Feb 27 22:06:23 1992

(defun process-arrow-operator (math-buffer) 
  "Process a arrow operator"
  (make-instance 'arrow-operator
                                        ;                 :type 'arrow-operator
                 :contents (get-math-object! math-buffer))
  )

;;; Function: PROCESS-MATHEMATICAL-FUNCTION-NAME Author: raman
;;; Created: Thu Feb 27 22:06:23 1992


(defun process-mathematical-function-name (math-buffer) 
  "Process a mathematical function name  "
  (make-instance 'mathematical-function-name
                                        ;                 :type 'mathematical-function-name
                 :contents (get-math-object! math-buffer))
  )

;;; Function: PROCESS-ACCENT Author: raman
;;; Created: Wed Sep 16 09:54:20 1992

(defun process-accent (math-buffer) 
  "Process a math accent" 
  (let ((accent (get-math-object! math-buffer))
        (object-to-be-accented  nil))
    (setf object-to-be-accented 
          (process-math
           (make-buffer :contents
                        (list
                         (pop-current-entry
                          math-buffer )))))
    (add-attribute 'accent 
                   (make-instance 'ordinary
                                  :contents accent)
                   object-to-be-accented)
    object-to-be-accented)
  )


;;; Function: PROCESS-UNDERBAR Author: raman
;;; Created: Wed Sep 16 09:54:20 1992

(defun process-underbar (math-buffer) 
  "Process a math underbar" 
  (let ((underbar (get-math-object! math-buffer))
        (object-to-be-underbarred  nil))
    (setf object-to-be-underbarred 
          (process-math
           (make-buffer :contents
                        (list
                         (pop-current-entry
                          math-buffer )))))
    (add-attribute 'underbar 
                   (make-instance 'ordinary
                                  :contents underbar)
                   object-to-be-underbarred)
    object-to-be-underbarred)
  )





;;; Function: PROCESS-NUMBER                         Author: raman
;;; Created: Thu Feb 27 22:06:23 1992

(defun process-number (math-buffer) 
  "Process a number"
  (make-instance 'math-number 
                                        ;                 :type 'number
                 :contents (pop-current-entry math-buffer))
  )



;;; Function: PROCESS-SUPERSCRIPT                            Author: raman
;;; Created: Fri Mar  6 09:25:49 1992

(defun process-superscript (math-buffer) 
  "Process superscript operator"
  (advance-pointer math-buffer) ; move on to the superscript
  (make-instance 'math-object
                 :type 'superscript
                 :contents (process-math
                            (make-buffer :contents
                                         (list (pop-current-entry  math-buffer)))))
  )


;;; Function: PROCESS-SUBSCRIPT                            Author: raman
;;; Created: Fri Mar  6 09:25:49 1992

(defun process-subscript (math-buffer) 
  "Process subscript operator"
  (advance-pointer math-buffer) ;move on to the subscript
  (make-instance 'math-object
                 :type 'subscript
                 :contents
                 (process-math
                  (make-buffer :contents
                               (list (pop-current-entry  math-buffer )))))
  )



;;; Collapse subscripts and subscripts by inserting them into the
;;; subscript and superscript slots of the object that they belong to.
;;; This function assumes valid TeX subscript and superscripts ie:
;;; Considers A_i_j to be invalid and does not expect to encounter it.
;;;



  ;;; Function: VOID-MATH-OBJECT-P                             Author: raman
  ;;; Created: Tue Dec 29 10:28:44 1992

(defun void-math-object-p (math-object) 
  "Is this a void math object "
  (and (null (contents math-object ))
       (null (children math-object ))
       (null (attributes math-object )))
  )

;;; Function: COLLAPSE-SUBSCRIPTS-AND-SUPERSCRIPTS           Author: raman
;;; Created: Tue Sep 15 18:56:09 1992

(defun collapse-subscripts-and-superscripts (list-of-math-objects) 
  "Collapse subscript and superscripts "
  (assert  (every #'math-object-subtype-p list-of-math-objects ) nil
           "~a is not a math object"
           (find-if
            #'(lambda(x) (not (math-object-subtype-p x)))
            list-of-math-objects))
  (if (= 1 (length list-of-math-objects ))
      list-of-math-objects              ; nothing to be done
      (let*  (
                                        ; otherwise
              (list-of-math-objects (remove-if #'void-math-object-p
                                               list-of-math-objects ))
              (collapsed-list-of-math-objects  nil)
              (current-math-object (first list-of-math-objects))
              )
        (dolist
            (next-math-object  (rest list-of-math-objects))
          (cond
            ((subscript? next-math-object)  (add-attribute
                                             'subscript ; with value 
                                             (contents
                                              next-math-object ) 
                                        ;to  math object 
                                             current-math-object))
            ((superscript? next-math-object) (add-attribute
                                              'superscript ; with  value 
                                              (contents
                                               next-math-object )
                                        ; to math object
                                              current-math-object))
            (t (setf collapsed-list-of-math-objects
                     (nconc collapsed-list-of-math-objects
                            (list current-math-object )))
               (setf current-math-object next-math-object) ;update
               )
            ))
        (nconc  collapsed-list-of-math-objects
                (list current-math-object ))))
  )



;;; Function: SUBSCRIPT?                                     Author: raman
;;; Created: Tue Sep 15 19:11:48 1992

(defun subscript? (math-object) 
  "Tests if math-object is of type subscript"
  (and  (math-object-p math-object)
        (eq 'subscript (math-object-type math-object))
        )
  )



;;; Function: SUPERSCRIPT?                                   Author: raman
;;; Created: Tue Sep 15 19:13:09 1992

(defun superscript? (math-object) 
  "Checks if math object is of type superscript"
  (and (math-object-p math-object)
       (eq 'superscript (math-object-type math-object))
       )
  )



;;; Function: MATHEMATICAL-FUNCTION-NAME?                    Author: raman
;;; Created: Wed Sep 16 11:42:11 1992

(defun mathematical-function-name? (math-object) 
  "Check if math-object is a mathematical-function-name"
  (and  (math-object-p math-object)
        (eq 'mathematical-function-name (math-object-type math-object))
        )
  )

