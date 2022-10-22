;;; Fri Feb  8 14:42:34 1991 by Mark Kantrowitz <mkant@GLINDA.OZ.CS.CMU.EDU>
;;; user-manual.lisp

;;; ****************************************************************
;;; Automatic User Manual Creation *********************************
;;; ****************************************************************
;;;
;;; The Automatic User Manual Creation system is a common lisp portable
;;; system for automatically generating user's guides from the 
;;; source definitions and their documentation strings. It uses several
;;; heuristics for formatting the documentation segments nicely.
;;; If Waters' XP pretty printer is available, it uses that instead to 
;;; format the argument lists.
;;;
;;; The user guide for this file was created using this program and
;;; provides a good example of its capabilities. It was created by
;;; evaluating (create-user-manual "user-manual.lisp").
;;;
;;; Written by Mark Kantrowitz, December 1990.
;;;
;;; Address: Carnegie Mellon University
;;;          School of Computer Science
;;;          Pittsburgh, PA 15213
;;; 
;;; Copyright (c) 1990. All rights reserved.
;;; 
;;; See general license below.
;;;

;;; ****************************************************************
;;; General License Agreement and Lack of Warranty *****************
;;; ****************************************************************
;;;
;;; This software is distributed in the hope that it will be useful (both
;;; in and of itself and as an example of lisp programming), but WITHOUT
;;; ANY WARRANTY. The author(s) do not accept responsibility to anyone for
;;; the consequences of using it or for whether it serves any particular
;;; purpose or works at all. No warranty is made about the software or its
;;; performance. 
;;; 
;;; Use and copying of this software and the preparation of derivative
;;; works based on this software are permitted, so long as the following
;;; conditions are met:
;;; 	o  The copyright notice and this entire notice are included intact
;;; 	   and prominently carried on all copies and supporting documentation.
;;; 	o  No fees or compensation are charged for use, copies, or
;;; 	   access to this software. You may charge a nominal
;;; 	   distribution fee for the physical act of transferring a
;;; 	   copy, but you may not charge for the program itself. 
;;; 	o  If you modify this software, you must cause the modified
;;; 	   file(s) to carry prominent notices (a Change Log)
;;; 	   describing the changes, who made the changes, and the date
;;; 	   of those changes.
;;; 	o  Any work distributed or published that in whole or in part
;;; 	   contains or is a derivative of this software or any part 
;;; 	   thereof is subject to the terms of this agreement. The 
;;; 	   aggregation of another unrelated program with this software
;;; 	   or its derivative on a volume of storage or distribution
;;; 	   medium does not bring the other program under the scope
;;; 	   of these terms.
;;; 	o  Permission is granted to manufacturers and distributors of
;;; 	   lisp compilers and interpreters to include this software
;;; 	   with their distribution. 
;;; 
;;; This software is made available AS IS, and is distributed without 
;;; warranty of any kind, either expressed or implied.
;;; 
;;; In no event will the author(s) or their institutions be liable to you
;;; for damages, including lost profits, lost monies, or other special,
;;; incidental or consequential damages arising out of or in connection
;;; with the use or inability to use (including but not limited to loss of
;;; data or data being rendered inaccurate or losses sustained by third
;;; parties or a failure of the program to operate as documented) the 
;;; program, even if you have been advised of the possibility of such
;;; damanges, or for any claim by any other party, whether in an action of
;;; contract, negligence, or other tortious action.
;;; 
;;; The current version of this software and a variety of related
;;; utilities may be obtained by anonymous ftp from ftp.cs.cmu.edu
;;; (128.2.206.173) or any other CS machine in the directory 
;;;       /afs/cs.cmu.edu/user/mkant/Public/Lisp-Utilities/
;;; You must cd to this directory in one fell swoop, as the CMU
;;; security mechanisms prevent access to other directories from an
;;; anonymous ftp. For users accessing the directory via an anonymous
;;; ftp mail server, the file README contains a current listing and
;;; description of the files in the directory. The file UPDATES describes
;;; recent updates to the released versions of the software in the directory.
;;; The file COPYING contains the current copy of this license agreement.
;;; Of course, if your site runs the Andrew File System and you have
;;; afs access, you can just cd to the directory and copy the files directly.
;;; 
;;; Please send bug reports, comments, questions and suggestions to
;;; mkant@cs.cmu.edu. We would also appreciate receiving any changes
;;; or improvements you may make. 
;;; 
;;; If you wish to be added to the Lisp-Utilities@cs.cmu.edu mailing list, 
;;; send email to Lisp-Utilities-Request@cs.cmu.edu with your name, email
;;; address, and affiliation. This mailing list is primarily for
;;; notification about major updates, bug fixes, and additions to the lisp
;;; utilities collection. The mailing list is intended to have low traffic.
;;;

;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;; mk   = Mark Kantrowitz <mkant@cs.cmu.edu>
;;; duff = David A. Duff <duff@starbase.MITRE.ORG>
;;; rit  = Jean-Francois Rit <rit@cs.stanford.edu>
;;; wds  = William D Smith <smithwd@kona.crd.ge.com>
;;; dick = Dick Jackson <jackson@ciitip.ciit.nrc.ca>
;;; ma   = Marco Antoniotti <marcoxa@image.cs.nyu.edu>
;;;
;;; 24-JAN-91 duff  Fixed bug in documentation handler for defstructs.
;;; 24-JAN-91 duff  Documentation-handlers for Lucid.
;;; 28-JAN-91 rit   Changed format clauses with @: to :@ in handle-form-output.
;;;                 Otherwise Franz Allegro CL barfs.
;;;           mk    Note that CLtL2 specifies that either is OK, although
;;;                 :@ is traditional. 
;;; 28-JAN-91 mk    Added Scribe output format in addition to TEXT output
;;;                 format.
;;; 31-JAN-91 duff  Doc-handler for define-condition.
;;; 05-FEB-91 wds   Added doc-handlers for defmethod, defgeneric, defclass,
;;;                 deftype and defsetf.
;;; 05-FEB-91 mk    Fixed handler for defmethod to handle qualifiers such 
;;;                 as :before, :after, and :around. Fixed defsetf handler
;;;                 to include alternate format. Extended deftype handler. 
;;; 07-FEB-91 dick  Doc-handlers for CLOS defclass. [This was better than
;;;                 wds's. I've merged the capabilities of the two.
;;;                 Also, I added :blank for use in leaving the args
;;;                 position blank, and fixed split-string to not
;;;                 trim whitespace in certain circumstances. --mk]
;;; 08-FEB-91 wds   Specialized the code using conditional read macros
;;;                 in case XP package not present.
;;; 05-JAN-93 ma    Changed some of the inner working to add LaTeX
;;;                 support: defvar, defconstant and defparameter have
;;;                 their doc-handlers changed when they have nil as
;;;                 default and support functions have bee added for
;;;                 LaTeX. Also the structure of handle-form-output
;;;                 has been slightly changed.
;;;                 A LaTeX document including entries obtained via
;;;                 create-user-manual must contain (e.g. by '\input')
;;;                 the 'lisp:documentation' environment as defined in
;;;                 the companion file 'lisp-documentation.tex'.

;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;; Generalize it to also use the definition's comments to create
;;; even more documentation. Essentially, change the #\; macro
;;; character so that instead of killing the remainder of the line,
;;; it processes it for the documentation. By making this program
;;; understand some sort of structured comments, we can have it
;;; ignore comment stubs and only include the meaty stuff.
;;;
;;; Clean up use of XP pretty printer.
;;; 
;;; Need 'latex output-format. In 'scribe mode, need to convert @ to @@.
;;;
;;; Note that using :output-format 'scribe and running scribe with
;;; the -device FILE arguments should produce output similar to 
;;; :output-format 'text, except without semicolons in the left margin.
;;;

;;; ********************************
;;; Documentation Types ************
;;; ********************************
;;;
;;; This is a list of the documentation types currently supported:
;;; + = things in user-manual.lisp
;;; - = things still missing from user-manual.lisp
;;;
;;;    + (DEFCLASS     name super-types slots &rest options )
;;;    + (DEFCONSTANT  name initial-value &optional documentation )
;;;    + (DEFGENERIC   name lambda-list &rest options )
;;;    + (DEFMACRO     name lambda-list documentation ... )
;;;    + (DEFMETHOD    name lambda-list documentation ... )
;;;    -  defpackage
;;;    + (DEFPARAMETER name initial-value &optional documentation )
;;;    + (DEFSETF      name function documentation )
;;;    + (DEFSTRUCT    name+options documentation &rest slots )
;;;    + (DEFTYPE      name lambda-list documentation ... )
;;;    + (DEFUN        name lambda-list documentation ... )
;;;    + (DEFVAR       name &optional initial-value documentation )
;;;


;;; ********************************
;;; Notes **************************
;;; ********************************
;;;
;;;    USER-MANUAL has been tested (successfully) in the following lisps:
;;;       CMU Common Lisp (M2.9 15-Aug-90, Compiler M1.8 15-Aug-90)
;;;       Macintosh Allegro Common Lisp (1.3.2)
;;;       ExCL (Franz Allegro CL 3.1.12 [DEC 3100] 3/30/90)
;;;       Lucid CL (Version 2.1 6-DEC-87)
;;;
;;;    USER-MANUAL needs to be tested in the following lisps:
;;;       Symbolics Common Lisp (8.0)
;;;       Lucid Common Lisp (3.0)
;;;       Lucid Common Lisp (4.0)
;;;       KCL (June 3, 1987 or later)
;;;       AKCL (1.86, June 30, 1987 or later)
;;;       TI (Release 4.1 or later)
;;;       Ibuki Common Lisp (01/01, October 15, 1987)
;;;       Golden Common Lisp (3.1 IBM-PC)
;;;       VAXLisp (2.0, 3.1)
;;;       HP Common Lisp (same as Lucid?)
;;;       Procyon Common Lisp


;;; ********************************
;;; User Guide *********************
;;; ********************************
;;;
;;; EXTRACT-DOCUMENTATION (body)                                        [MACRO]
;;;
;;; ATOM-OR-CAR (list-or-atom)                                       [FUNCTION]
;;;
;;; *DOCUMENTATION-HANDLERS* (make-hash-table :test                  [VARIABLE]
;;;                           (function equal))
;;;    Hash table of entries of the form (handler description),
;;;    where definer is the car of the definition form handled (for
;;;    example, DEFUN or DEFMACRO), handler is a function which takes the
;;;    form as input and value-returns the name, argument-list and
;;;    documentation string, and description is a one-word equivalent of
;;;    definer (for example, FUNCTION or MACRO).
;;;
;;; DEFINE-DOC-HANDLER (definer arglist description &body body)         [MACRO]
;;;    Defines a new documentation handler. DEFINER is the car of the
;;;    definition form handled (e.g., defun), DESCRIPTION is a one-word
;;;    string equivalent of definer (e.g., "function"), and ARGLIST
;;;    and BODY together define a function that takes the form as input
;;;    and value-returns the name, argument-list, documentation string,
;;;    and a list of any qualifiers of the form.
;;;
;;; FIND-DOC-HANDLER (definer)                                       [FUNCTION]
;;;    Given the car of a form, finds the appropriate documentation
;;;    handler for the form if one exists.
;;;
;;; DEFINE-DOC-HANDLER (form)                                     [DOC-HANDLER]
;;;    Documentation handler for doc-handlers.
;;;
;;; DEFVAR (form)                                                 [DOC-HANDLER]
;;;    Documentation handler for variables.
;;;
;;; DEFCONSTANT (form)                                            [DOC-HANDLER]
;;;    Documentation handler for constants.
;;;
;;; DEFPARAMETER (form)                                           [DOC-HANDLER]
;;;    Documentation handler for parameters.
;;;
;;; DEFUN (form)                                                  [DOC-HANDLER]
;;;    Documentation handler for functions.
;;;
;;; DEFMACRO (form)                                               [DOC-HANDLER]
;;;    Documentation handler for macros.
;;;
;;; DEFSTRUCT (form)                                              [DOC-HANDLER]
;;;    Documentation handler for structures.
;;;
;;; DEFINE-CONDITION (form)                                       [DOC-HANDLER]
;;;    Documentation handler for conditions.
;;;
;;; DEFTYPE (form)                                                [DOC-HANDLER]
;;;    Documentation handler for types.
;;;
;;; DEFSETF (form)                                                [DOC-HANDLER]
;;;    Documentation handler for setf mappings.
;;;
;;; DEFMETHOD (form)                                              [DOC-HANDLER]
;;;    Documentation handler for methods.
;;;
;;; DEFGENERIC (form)                                             [DOC-HANDLER]
;;;    Documentation handler for generic functions.
;;;
;;; DEFCLASS (form)                                               [DOC-HANDLER]
;;;    Documentation handler for classs.
;;;
;;; *FAILED-DEFINITION-TYPES* "()"                                   [VARIABLE]
;;;    List of definition types that create-user-manual couldn't handle.
;;;
;;; CREATE-USER-MANUAL (filename &key (output-format (quote text))   [FUNCTION]
;;;                     (output-stream *standard-output*))
;;;    Automatically creates a user manual for the functions in a file by 
;;;    collecting the documentation strings and argument lists of the
;;;    functions and formatting the output nicely. Returns a list of the
;;;    definition types of the forms it couldn't handle. Output-format
;;;    may be either 'TEXT or 'SCRIBE.
;;;
;;; HANDLE-FORM-OUTPUT (form &optional (output-format (quote text))  [FUNCTION]
;;;                     (stream *standard-output*))
;;;    This function takes a form as input and outputs its documentation
;;;    segment to the output stream.
;;;
;;; OUTPUT-TEXT-DOCUMENTATION (name type args documentation          [FUNCTION]
;;;                            args-tab-pos type-pos
;;;                            &optional (stream *standard-output*))
;;;    Prints out the user guide entry for a form in TEXT mode.
;;;
;;; OUTPUT-SCRIBE-DOCUMENTATION (name type args documentation        [FUNCTION]
;;;                              &optional
;;;                              (stream *standard-output*))
;;;    Prints out the user guide entry for a form in SCRIBE mode.
;;;
;;; XP-SPLIT-STRING (arglist width)                                  [FUNCTION]
;;;    PPrints the arglist into a string of specified width. Assumes
;;;    that we're running the XP pretty printer.
;;;
;;; SPLIT-STRING (string width &optional arglistp filled             [FUNCTION]
;;;               (trim-whitespace t))
;;;    Splits a string into a list of strings, each of which is shorter
;;;    than the specified width. Tries to be intelligent about where to
;;;    split the string if it is an argument list. If filled is T,
;;;    tries to fill out the strings as much as possible. This function
;;;    is used to break up long argument lists nicely, and to break up
;;;    wide lines of documentation nicely.
;;;
;;; SPLIT-POINT (string max-length &optional arglistp filled)        [FUNCTION]
;;;    Finds an appropriate point to break the string at given a target
;;;    length. If arglistp is T, tries to find an intelligent position to
;;;    break the string. If filled is T, tries to fill out the string as
;;;    much as possible. 
;;;
;;; LAMBDA-LIST-KEYWORD-POSITION (string                             [FUNCTION]
;;;                               &optional end trailer-only)
;;;    If the previous symbol is a lambda-list keyword, returns
;;;    its position. Otherwise returns end.
;;;
;;; BALANCED-PARENTHESIS-POSITION (string &optional end)             [FUNCTION]
;;;    Finds the position of the left parenthesis which is closest to END
;;;    but leaves the prefix of the string with balanced parentheses or
;;;    at most 1 unbalanced left parenthesis.
;;;
;;; PARSE-WITH-DELIMITER (line &optional (delim #\newline))          [FUNCTION]
;;;    Breaks LINE into a list of strings, using DELIM as a 
;;;    breaking point.
;;;

;;; ********************************
;;; Check for XP *******************
;;; ********************************

;(in-package "USER")
;; If the XP package is available, add :XP to *features* so that
;; the conditional read macros will work.
(when (find-package "XP") (push :XP *FEATURES*))

;;; ********************************
;;; Utilities **********************
;;; ********************************

(defmacro extract-documentation (body)
  `(when (and (stringp (car ,body))
	      (cdr ,body))
     (pop ,body)))

(defun atom-or-car (list-or-atom)
  (if (listp list-or-atom)
      (first list-or-atom)
      list-or-atom))

;;; ********************************
;;; Documentation Handlers *********
;;; ********************************
(defvar *documentation-handlers* (make-hash-table :test #'equal)
  "Hash table of entries of the form (handler description),
   where definer is the car of the definition form handled (for example,
   DEFUN or DEFMACRO), handler is a function which takes the form as input
   and value-returns the name, argument-list and documentation string,
   and description is a one-word equivalent of definer (for example,
   FUNCTION or MACRO).")

(defmacro define-doc-handler (definer arglist description &body body)
  "Defines a new documentation handler. DEFINER is the car of the
   definition form handled (e.g., defun), DESCRIPTION is a one-word
   string equivalent of definer (e.g., \"function\"), and ARGLIST
   and BODY together define a function that takes the form as input
   and value-returns the name, argument-list, documentation string, 
   and a list of any qualifiers of the form."
  `(setf (gethash ',definer *documentation-handlers*)
	 (list #'(lambda ,arglist
		   ,@body)
	       ,description)))

(defun find-doc-handler (definer)
  "Given the car of a form, finds the appropriate documentation
   handler for the form if one exists."
  (gethash definer *documentation-handlers*))

(define-doc-handler define-doc-handler (form)
  "doc-handler"
  ;; name arglist documentation-string
  (values (second form)
	  (third form)
	  (format nil "Documentation handler for ~A~P."
		  (fourth form) 2)))

;;; Changed the return form for the arguments in defvar, defconstant
;;; and defparameter (basically 'listified' them).

(define-doc-handler defvar (form)
  "variable"
  ;; name arglist documentation-string
  (values (second form)
	  (list (third form))
	  (fourth form)))

(define-doc-handler defconstant (form)
  "constant"
  ;; name arglist documentation-string
  (values (second form)
	  (list (third form))
	  (fourth form)))

(define-doc-handler defparameter (form)
  "parameter"
  ;; name arglist documentation-string
  (values (second form)
	  (list (third form))
	  (fourth form)))

(define-doc-handler defun (form)
  "function"
  ;; name arglist documentation-string
  (values (second form)
	  (third form)
	  (when (stringp (fourth form)) (fourth form))))

(define-doc-handler defmacro (form)
  "macro"
  ;; name arglist documentation-string
  (values (second form)
	  (third form)
	  (when (stringp (fourth form)) (fourth form))))

(define-doc-handler defstruct (form)
  "structure"
  ;; name arglist documentation-string
  (values (atom-or-car (second form))
	  (mapcar #'atom-or-car
		  (if (stringp (third form))
		      (cdddr form)
		      (cddr form)))
	   (third form)))

(define-doc-handler define-condition (form)
  "condition"
  (values (cadr form)
          ;; handled here like defstruct. 
	  ;; might want to skip this and return nil.
          (mapcar #'atom-or-car (fourth form))
          (cadr (find :documentation (nthcdr 4 form) :key #'car))))

(define-doc-handler DEFTYPE (form)
  "type"
  (let ((name (second form))
	(args (third form))
	doc def)
    (setq form (cdddr form))
    (setq doc (extract-documentation form))
    (when (null (cdr form))
      (setq def (car form)))
    ;; We give a try at the type definition if the body of the deftype
    ;; form consists of a single form.
    (if def
	(values name def doc (list args))
	(values name args doc))))

#|
(define-doc-handler DEFTYPE (form)
  "type"
  (values (second form)
          (third  form)
          (fourth form)))
|#

(define-doc-handler DEFSETF (form)
  "setf mapping"
  ;; name args doc. 
  ;; defsetf has two formats:
  ;;    (defsetf name update-fn doc)
  ;;    (defsetf name lambda-list (store-variable) doc-string body)
  (cond ((listp (third form))
	 ;; long format
	 (values `(setf ,(second form))
		 (fourth form) ; store variable
		 (fifth form)))
	(t
	 ;; short format
	 (values `(setf ,(second form))
		 (third  form)
		 (fourth form)))))

;;; ********************************
;;; CLOS Related *******************
;;; ********************************

(define-doc-handler DEFMETHOD (form)
  "method"
  ;; name arglist documentation-string
  ;; (defmethod name {qualifiers}* lambda-list [ {decl}* || doc ] body)
  (let (name quals args doc)
    (setf name (second form))
    (do ((form (cddr form) (cdr form)))
	((or (null form)
	     ;; qualifiers are non-nil atoms while lambda-list is a list.
	     (listp (car form)))
	 (setq quals (nreverse quals))
	 (setq args (car form))
	 (setq form (cdr form))
	 (setq doc (extract-documentation form))
	 (values name args doc quals))
      (when (atom (car form))
	(push (car form) quals)))))

#|
(define-doc-handler DEFMETHOD (form)
  "method"
  ;; name arglist documentation-string
  (let* ((name (second form))
         (qual (when (symbolp (third form)) (third form)))
         (args (if qual
                   (cons qual (fourth form))
                   (third form)))
         (doc  (if qual (fifth form) (fourth form))))
    (values name args doc)))

;; Dick's version:
(define-doc-handler defmethod (form)
  "method"
  ;; name arglist documentation-string
  (if (listp (third form))
    (values (second form)
            (third form)
            (format nil "Primary method~%~A"
                    (if (stringp (fourth form))
                      (fourth form)
                      "")))
    (values (second form)
            (fourth form)
            (format nil ":~A method~%~A"
                    (third form)
                    (if (stringp (fifth form))
                      (fifth form)
                      "")))))

|#

;;; DEFGENERIC: misses many options (eg, compile optimizations, etc.)
(define-doc-handler DEFGENERIC (form)
  "generic function"
  ;; name arglist documentation-string
  (values (second form)
          (third  form)
          ;; doc-string is embedded in body of DEFGENERIC:
          (second (assoc :documentation (cdddr form)))))

(define-doc-handler DEFCLASS (form)
  "class"
  ;; name super-types slots &rest options
  (let ((class-options (nthcdr 4 form))
	(class-name (second form))
	(superclass-names (third form))
	(slots (fourth form)))
    (values class-name
	    :blank				; no args per se
	    (format nil "~@[~A~%~]Superclasses:~:[ ~A~;~{ ~A~}~]~
                       ~%Slots: ~:[<none>~;~:{~%    ~A~@[: ~A~]~}~]"
		    ;; Class documentation
		    (second (assoc :documentation class-options))
		    ;; Superclasses
		    superclass-names (or superclass-names "<none>")
		    slots
		    (mapcar #'(lambda (slot)
				(if (atom slot)
				    (list slot nil)
				    (list (car slot)
					  (cadr (member :documentation
							slot)))))
			    slots)))))



;;; ********************************
;;; Doc Handlers for Lucid's FFI ***
;;; ********************************
;;; Doc-handlers for Lucid's foreign function interface

#+:lucid
(define-doc-handler def-foreign-synonym-type (form)
  "foreign synonym type"
  (values (second form) (third form) nil))

#+:lucid
(define-doc-handler def-foreign-struct (form)
  "foreign synonym type"
  (values (second form)
	  (mapcar #'car
		  (cddr form))
	  nil))

#+:lucid
(define-doc-handler def-foreign-function (form)
  "foreign function"
  (values (atom-or-car (second form))
	  (if (stringp (third form))
	      (cddr form)
	      (cdddr form))
	  (third form)))



;;; ********************************
;;; Create User Manual *************
;;; ********************************
(defvar *failed-definition-types* nil
  "List of definition types that create-user-manual couldn't handle.")

(defun create-user-manual (filename &key
				    (output-format 'text)
				    (output-stream *standard-output*)
				    (purge-latex t))
  "Automatically creates a user manual for the functions in a file by 
   collecting the documentation strings and argument lists of the 
   functions and formatting the output nicely. Returns a list of the
   definition types of the forms it couldn't handle. Output-format may
   be either 'TEXT, 'SCRIBE or 'LATEX. In this last case the extra
   keyword 'purge-latex' may be specified: if non nil the latex filter
   will try to substitute possible dangerous characters like '&', '\\' and
   '#'."
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
	  (handle-form-output form
			      output-format
			      output-stream
			      purge-latex))))))

(defun handle-form-output (form &optional (output-format 'text)
				(stream *standard-output*)
				(purge-latex t))
  "This function takes a form as input and outputs its documentation
   segment to the output stream."
  (let* ((key (first form))
	 (handler-entry (find-doc-handler key)))
    (cond (handler-entry 
	   (let ((handler (first handler-entry))
		 (type (second handler-entry))
		 )
	     (multiple-value-bind (name args documentation qualifiers) 
		 (funcall handler form)
	       (let ((name-length 0)
		     (args (cond ((stringp args) ; variable
				  (format nil "~S" args))
				 ((null args)
				  "()")
				 ((eq args :blank) "")
				 (t args)))
		     (type-pos (- 80 1)) ; 1 for right margin
		     (args-list-form args)
		     )
		 (setq name (format nil "~:@(~A~)~@[~{ ~S~}~]" name qualifiers)
		       type (format nil "~:@(~A~)" type)
		       name-length (length name)
		       )
		 ;; subtract the width of [type]
		 (decf type-pos (+ (length type) 2))
		 (let ((width (- type-pos (+ 1 4 1 ) name-length)))
		   (unless (eq output-format 'text)
		     ;; Add in the width of ";;; " since we use it 
		     ;; only in text mode.
		     (incf width 4))
		   (setq args
			 #+:XP(xp-split-string args width)
			 #-:XP(split-string (format nil "~(~S~)" args)
					    width t)))
		 (ccase output-format
		   (text
		    (output-text-documentation name type args documentation
					       name-length type-pos
					       stream))
		   (scribe
		    (output-scribe-documentation name type args documentation
						 stream))
		   (latex
		    (output-latex-documentation name type
						args-list-form
						documentation
						stream
						purge-latex)))
		 ))))
	  ((eq key 'eval-when)
	   (dolist (f (cddr form))
	     (handle-form-output f output-format stream)))
	  (t
	   (pushnew key *failed-definition-types*)))))


(defun output-text-documentation (name type args documentation args-tab-pos
				       type-pos
				       &optional (stream *standard-output*))
  "Prints out the user guide entry for a form in TEXT mode." 
  (format stream "~%;;; ~A ~A ~VT[~A]" name (first args) type-pos type)
  (dolist (arg (rest args))
    (format stream "~%;;; ~0,1,V,' @A" 
	    (+ #+:XP 1 #-:XP 2 args-tab-pos)
	    arg))
  (when (stringp documentation)
    ;; We give a line width of 70 characters for documentation
    ;; strings. This leaves us room for a left margin of
    ;; ";;;    " and a right margin of 3 spaces (2 chars left of []).
    (dolist (string (split-string documentation 70 nil nil nil))
      (format stream "~&;;;    ~A" string)))
  (format stream "~%;;;"))

(defun output-scribe-documentation (name type args documentation 
					 &optional (stream *standard-output*))
  "Prints out the user guide entry for a form in SCRIBE mode."
  (format stream "~%@begin(format,group)~%@tabclear()")
  (format stream "~%@t[~(~A~)] @^~A@>[~:(~A~)]"
	  name (first args) type)
  (dolist (arg (rest args))
    (format stream #+:XP "~%@\\@ ~A" #-:XP "~%@\\~A"
	    arg))
  (format stream "~%@tabclear()")
  ;; (format stream "~%@hinge()")
  (format stream "~%@begin(quotation,indent 0,size +0)")
  (when (stringp documentation)
    (dolist (string (split-string documentation 70 nil nil nil))
      (format stream "~%~A" string)))
  (format stream "~%@end(quotation)~%@end(format)~%"))

(defun output-latex-documentation (name type args documentation 
					 &optional
					 (stream *standard-output*)
					 (purge-documentation t))
  "Prints out the user guide entry for a form in LaTeX mode."
  (format stream "\\begin{lisp:documentation}")
  (format stream "{~(~A~)}{~A}" name type)
  (format stream "~:[{\\ }~;{~{~(~A~) ~}}~]~%"
	  args (preprocess-lisp-latex-clashes args purge-documentation))
  (if (stringp documentation)
    (format stream "~{~A~%~}"
	    (split-string (purge-string-for-latex documentation
						  purge-documentation)
			  70 nil nil nil))
    (format stream "{\\ } % NO DOCUMENTATION FOR ~A~%" name))
  (format stream "\\end{lisp:documentation}~2%"))

(defun purge-string-for-latex (a-string purge-doc)
  "Tries to purge a string from characters that are potentially
   dangerous for LaTeX."
  (if purge-doc
      (with-input-from-string (a-str a-string)
	(with-output-to-string (result)
	  (let ((eos (gensym)))
	    (do ((c (read-char a-str nil eos nil)
		    (read-char a-str nil eos nil)))
		((eq c eos) result)
	      (case c
		(#\& (format result "\\&"))
		(#\\ (format result "$\\backslash$")) ; I have to
						      ; resort to math
						      ; mode to do this.
		(#\# (format result "\\#"))
		(#\$ (format result "\\$"))
		(#\% (format result "\\%"))
		(#\{ (format result "\\{"))
		(#\} (format result "\\}"))
		(#\_ (format result "\\_}"))
		;; missing ~ and ^ from the list of the "ten nasty
		;; characters" in the LaTeX manual
		(t   (format result "~C" c)))))
	  ))
      a-string
      ))

(defun preprocess-lambda-keywords (args)
  "Unused"
  (mapcar #'(lambda (arg)
	      (if (member arg lambda-list-keywords :test #'eq)
		  (format nil "{\\sf \\~(~A~)}" arg)
		  arg))
	  args))

(defun preprocess-lisp-latex-clashes (args purge-doc)
  "This function is used to make the strings for the arguments of the
   form digestible for LaTeX, e.g. by removing '#' and '&'."
  (if (stringp args)
      (list args)
      (mapcar #'(lambda (arg)
		  (cond ((member arg lambda-list-keywords :test #'eq)
			 (format nil "{\\sf \\~(~A~)}" arg))
			((listp arg) ; OK OK I am missing cons cells!
			 (preprocess-specials arg purge-doc))
			((characterp arg)
			 (preprocess-character arg))
			(t (purge-string-for-latex
			    (format nil "~S" arg)
			    purge-doc))))
	      args)))

(defun preprocess-character (c)
  "Low level processing of single characters, when passed as defaults
   to optional, key and aux parameters."
  ;; The stupid LaTeX manual does not tell be how to produce a single
  ;; '\' without being in math mode, so I have to trick it.
  (case c
    (#\newline "\\#$\\backslash${newline}")
    (#\space "\\#$\\backslash${space}")
    (#\rubout "\\#$\\backslash${rubout}")
    (#\page "\\#$\\backslash${page}")
    (#\backspace "\\#$\\backslash${backspace}")
    (#\return "\\#$\\backslash${return}")
    (#\linefeed "\\#$\\backslash${linefeed}")
    (t c)))

(defun preprocess-specials (list-form purge-doc)
  "Processing of some 'special' forms. Only 'quote' and 'function' are
   treated for the time being."
  (case (first list-form)
    (function
     (format nil "\\#'~A"
	     (purge-string-for-latex
	      (format nil "~A" (second list-form))
	      purge-doc)))
    (quote
     (format nil "'~A"
	     (purge-string-for-latex
	      (format nil "~A" (second list-form))
	      purge-doc)))
    (t (preprocess-lisp-latex-clashes list-form purge-doc))))

#+:XP
(defun xp-split-string (arglist width)
  "PPrints the arglist into a string of specified width. Assumes
   that we're running the XP pretty printer."
  (let ((xp::*default-right-margin* width)
	(xp::*print-miser-width* 30)
	string)
    (cond ((stringp arglist) (list arglist))
	  (t
	   (setq string
		 (with-output-to-string (stream) (pprint arglist stream)))
	   (setq string (format nil "~(~A~)" string))
	   (setq string (remove-if #'(lambda (x) (string-equal x ""))
				   (parse-with-delimiter string #\newline)))
	   string))))

(defun split-string (string width 
			    &optional arglistp filled (trim-whitespace t))
  "Splits a string into a list of strings, each of which is shorter
   than the specified width. Tries to be intelligent about where to
   split the string if it is an argument list. If filled is T,
   tries to fill out the strings as much as possible. This function
   is used to break up long argument lists nicely, and to break up
   wide lines of documentation nicely."
  (let ((string-list (parse-with-delimiter string #\newline))
	(result nil))
    (do* ((rest string-list (rest rest))
	  (s (car rest) (car rest)))
	((null rest)
	 (nreverse result))
      (multiple-value-bind (first second) (split-point s width arglistp filled)
	(if trim-whitespace
	    (setf first (string-trim '(#\space #\tab) first))
	    (when (and (> (length first) 0)
		       (char= (char first 0) #\space))
	      (if (and (> (length first) 3)
		       (char= (char first 1) #\space)
		       (char= (char first 2) #\space)
		       (not (char= (char first 3) #\space)))
		  (setf first (string-trim '(#\space #\tab) first))
		  (setf first (subseq first 1)))))
	(when (not (string-equal first ""))
	  (push first result))
	(when (and second (not (string-equal second "")))
	  (setf rest
		(list* 
		 nil
		 (concatenate 'string
			      (string-trim '(#\space #\tab) second)
			      " "
			      (and (cadr rest)
				   (string-trim '(#\space #\tab)
						(cadr rest)))) 
		 (cddr rest))))))))

;;; need some way for the last line from an arglist to possibly
;;; be split, even if it has a perfect fit.
(defun split-point (string max-length &optional arglistp filled)
  "Finds an appropriate point to break the string at given a target length.
   If arglistp is T, tries to find an intelligent position to break the
   string. If filled is T, tries to fill out the string as much as possible."
  ;; we probably should split some strings that are short enough anyway
  ;; but need a base condition to prevent infinite loops.
  (cond ((< (length string) max-length)
	 (let ((lambda (lambda-list-keyword-position string (length string)
						     t)))
	   (if (and arglistp lambda (not (zerop lambda)))
	       (values (subseq string 0 lambda)
		       (unless (= lambda (length string))
			 (subseq string lambda)))
	     string)))
	(t
	 ;; Find the first space that breaks the arglist.
	 ;; If parentheses are not balanced at this point,
	 ;; go to the first balanced paren that isn't at position
	 ;; zero (actually, the conditions are much more complex).
	 ;; Then check if the previous "word" is a lambda-list keyword.
	 ;; This gives it a preference for lambda-list keywords.
	 (let* ((space-pos (position #\space string :from-end t 
				    :end max-length))
		(pos space-pos))
	   (when arglistp
	     (let* ((paren (balanced-parenthesis-position 
			    string (or space-pos max-length)))
		    (lambda (lambda-list-keyword-position 
			     string (or paren max-length))))
	       (cond ((and lambda paren space-pos arglistp
			   (not (zerop lambda))
			   (or (not filled)
			       (< (- max-length 10) lambda max-length)))
		      (setq pos lambda))
		     ((and paren space-pos
			   arglistp
			   (not (zerop paren))
			   (or (not filled)
			       (< (- max-length 10) paren max-length)))
		      (setq pos paren)))))
	   (if pos
	       (values (subseq string 0 pos)
		       (unless (= pos (length string))
			 (subseq string pos)))
	     string)))))

(defun lambda-list-keyword-position (string &optional end trailer-only)
  "If the previous symbol is a lambda-list keyword, returns
   its position. Otherwise returns end."
  ;; possibly extend this to also search for colons for keywords? 
  (when (null end) (setf end (length string)))
  (let ((ampersand (position #\& string :from-end t :end end))
	(rightmost-space (position #\space string :from-end t :end end)))
    (if ampersand 
	(cond ((find (string-trim '(#\space) (subseq string ampersand end))
		   lambda-list-keywords 
		   :key #'symbol-name
		   :test #'string-equal)
	       ampersand)
	      ((and rightmost-space (not trailer-only)
		    (< ampersand rightmost-space)
		    (find (string-trim '(#\space)
				       (subseq string
					       ampersand rightmost-space))
			  lambda-list-keywords 
			  :key #'symbol-name
			  :test #'string-equal))
	       ampersand)
	      (t
	       end))
      end)))

(defun balanced-parenthesis-position (string &optional end)
  "Finds the position of the left parenthesis which is closest to END
   but leaves the prefix of the string with balanced parentheses or 
   at most 1 unbalanced left parenthesis."
  (when (null end) (setf end (length string)))
  (let* ((num-left (count #\( string :end end))
	 (num-right (count #\) string :end end))
	 (imbalance (max 0 (- num-left num-right)))
	 (leftmost-left-paren (position #\( string :end end))
	 (leftmost-right-paren (position #\) string :end end))
	 (rightmost-left-paren (position #\( string :end end :from-end t)))
    (cond ((and leftmost-left-paren leftmost-right-paren
		(< leftmost-right-paren leftmost-left-paren))
	   ;; if we have ")(", break after the right paren.
	   leftmost-left-paren)
	  ((and leftmost-right-paren (not leftmost-left-paren))
	   ;; we have a right paren but no left
	   (1+ (position #\) string :from-end t :end end)))
	  ((or (= imbalance 0)
	       (and (or (char= (char string 0) #\()
			(char= (char string 0) #\&))
		    (cond (leftmost-right-paren
			   ;; there's a right paren and the left
			   ;; parens before it account for the imbalance
			   ;; actually, we need to do a fancier balancing
			   ;; operation here to absorb balanced left
			   ;; parentheses. 
			   (not (< (1- (count #\( string 
					  :end leftmost-right-paren))
			      imbalance)))
			  ((find #\space string :end rightmost-left-paren)
			   nil)
			  (t))))
	   ;; either we're balanced, or the imbalance is due to
	   ;; left-parens at the left edge.
	   end)
	  (t
	   ;; let's try to reduce the imbalance
	   (if (and rightmost-left-paren (not (zerop rightmost-left-paren)))
	       (balanced-parenthesis-position string rightmost-left-paren)
	     end)))))

(defun parse-with-delimiter (line &optional (delim #\newline))
  "Breaks LINE into a list of strings, using DELIM as a 
   breaking point."
  ;; what about #\return instead of #\newline?
  (let ((pos (position delim line)))
    (cond (pos
           (cons (subseq line 0 pos)
                 (parse-with-delimiter (subseq line (1+ pos)) delim)))
          (t
           (list line)))))

;;; ********************************
;;; Dead Code **********************
;;; ********************************

#|
;(split-string "the quick brown fox jumped over the lazy dogs" 20)

(split-string "(((&OPTIONAL (NODE (QUOTE NODE)) (QUEUE (QUOTE QUEUE))
   (CHILDREN (QUOTE CHILDREN)) VISITED)
  INITIAL-QUEUE CHILDREN-FORM &OPTIONAL
  (DEQUEUE-FORM (QUOTE (POP QUEUE)))
  (MERGE-FORM (QUOTE (SETQ QUEUE (APPEND QUEUE PROGENY))))
  RESULT-FORM)
 &BODY BODY)" 30 t)

;(BALANCED-PARENTHESIS-POSITION "foo the (bar baz) (biz" )

|#

