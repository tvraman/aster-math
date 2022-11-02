(provide 'clisp-template)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This part of the file provides an interactive template-based
;;; definition facility. Prompts the user for the different parts of
;;; each definition. Adapted by Riad Mohammed
;;; (mohammed@cs.cornell.edu) from code originally written by Rick
;;; Palmer (rick@cs.cornell.edu).

;;; *TEMPLATE-ALIST* describes the types of templates this facility
;;; knows about. The car is the type of the thing being defined, while
;;; the cdr is a string that is concatenated with "def" to get the
;;; defining form.

(defvar *template-alist* '(("function" . "un") 
			   ("macro" . "macro") 
			   ("structure" . "struct") 
			   ("variable" . "var") 
			   ("constant" . "constant") 
			   ("parameter" . "parameter")
			   ("class" . "class")
			   ("generic" . "generic")
			   ("method" . "method")))

;;; If set to t, then *clisp-separator-string* will be placed before
;;; every template. If set to nil, then *clisp-separator-string* will
;;; be set only if a prefix argument is given to clisp-make-template.

(defvar *clisp-default-separate* nil)

;;; Sets width (in characters) of the separator and header. Defaults
;;; to 74 (+ the leading ";;;").

(defvar *clisp-separator-width* 74)
(defvar *clisp-separator-character* ?;)
(defvar *clisp-separator-string* 
  (format ";;;%s\n" (make-string *clisp-separator-width*
				   *clisp-separator-character*)))

(defvar *clisp-default-doc-string* "Undocumented.")

;;; The following strings are used to define the template.

(defvar *clisp-header-string* (concat "  ;;; %s: %s%sAuthor: "
				    (user-login-name)
				    "\n  ;;; Created: %s\n\n"))

(defvar *clisp-defn-string* "(def%s %s")

;;; CLISP-TEMPLATE-GET-TYPE returns a consed pair from
;;; *template-alist* with the type and the letters required following
;;; "def" to define type.

(defun clisp-template-get-type ()
  (let* ((type (completing-read "Type? " *template-alist* nil t)))
    (cond ((= (length type) 0) '("function" . "un"))
	  (t (assoc type *template-alist*)))))

;;; CLISP-PROMPT-USER prompts the user with prompt-string in the
;;; minibuffer and accepts a reply. If reply is not equal to
;;; null-reply, it is returned; else default is returned.

(defun clisp-prompt-user (prompt-string &optional null-reply default)
  (let ((reply (read-string prompt-string)))
    (cond ((string= reply null-reply) default)
	  (t reply))))

;;; CLISP-MAKE-TEMPLATE does the brunt of the work setting up the
;;; template. If called with a prefix arg (or if
;;; *clisp-default-separate* is t), the *clisp-separator* (initially a row
;;; of semicolons) is inserted before the template.

(defun clisp-make-template (&optional separate)
  "Creates a template interactively for the appropriate defun, defvar, 
defconstant, defparamater, defstruct, defclass, or defmethod."
  (interactive "P")
  (let ((type (clisp-template-get-type)) postfix header)
    (setq postfix (cdr type))
    (setq type (car type))
    (let ((name (clisp-prompt-user (concat (capitalize type) " name? ")
			     "" 
			     (upcase (concat "UNNAMED-" (upcase type))))))
      (or (bolp)
	  (progn (end-of-line 1)
		 (insert "\n")))
      (insert "\n")
      (setq header
	    (concat 
	     (cond ((or separate *clisp-default-separate*)
		    *clisp-separator-string*)
		   (t ""))
	     (format *clisp-header-string* 
		     (capitalize type) 
		     (upcase name) 
		     (make-string (max (- *clisp-separator-width*
					  (+ 19 (length type)(length name))) 1)
				       ?\ )
		     (current-time-string))
	     (format *clisp-defn-string* postfix name)))
      (cond ((string= type "function")
	     (clisp-make-function-template name header))
	    ((or (string= type "variable")
		 (string= type "constant")
		 (string= type "parameter"))
	     (clisp-make-variable-template name header ))
	    ((string= type "macro")
	     (clisp-make-macro-template name header))
	    ((string= type "structure")
	     (clisp-make-structure-template name header))
	    ((string= type "class")
	     (clisp-make-class-template name header))
	    ((string= type "generic")
	     (clisp-make-generic-template name header))
	    ((string= type "method")
	     (clisp-make-method-template name header))))))

;;; Each of the following functions writes out the proper template for
;;; a particular type of defining form.

(defun clisp-make-function-template (name header)
  (let ((args (clisp-prompt-user "Arguments? "))
	(doc (clisp-prompt-user "Documentation? " "" 
				*clisp-default-doc-string*)))
    (insert header)
    (insert (format " (%s) \n\"%s\"\n)" args doc))
    (clisp-reindent-form)
    (backward-char 1)))

(defun clisp-make-macro-template (name header)
  (let ((args (clisp-prompt-user "Arguments? "))
	(doc (clisp-prompt-user "Documentation? " "" 
				*clisp-default-doc-string*)))
    (insert header)
    (insert (format " (%s) \n\"%s\"\n)" args doc))
    (clisp-reindent-form)
    (backward-char 1)))

(defun clisp-make-variable-template (name header)
  (let ((value (clisp-prompt-user "Value? " "" "nil"))
	(doc (clisp-prompt-user "Documentation? " "" 
				*clisp-default-doc-string*)))
    (insert header)
    (insert (format " %s \"%s\")" value doc))))

(defun clisp-make-structure-template (name header)
  (let ((slots (clisp-prompt-user "Slots? "))
	(doc (clisp-prompt-user "Documentation? " "" 
				*clisp-default-doc-string*)))
    (insert header)
    (or (string= doc *clisp-default-doc-string*)
	(progn (beginning-of-line)
	       (insert (format ";;; %s" doc))
	       (clisp-reindent-form)
	       (insert "\n\n")
	       (end-of-line 1)))
    (let ((index 0))
      (while (< index (length slots))
	     (setq index (read-from-string slots index))
	     (newline-and-indent)
	     (insert (format " %s" (car index)))
	     (setq index (cdr index))))
    (insert ")")
    (backward-char 1)
    (clisp-reindent-form)
    (end-of-line)))

(defun clisp-make-class-template (name header)
  (let ((supers (clisp-prompt-user "Supers? "))
	(slots (clisp-prompt-user "Slots? "))
	(doc (clisp-prompt-user "Documentation? " "" 
				*clisp-default-doc-string*)))
    (insert header)
    (insert (format " (%s)\n ()\n" supers))
    (backward-char 2)
    (clisp-add-slots slots)
    (forward-char 1)
    (insert (format "\n (:documentation \"%s\"))\n" doc))
    (clisp-reindent-form)
    (end-of-line)
    (insert "\n")
    (insert 
     (format 
      "(defun make-%s ()\n (let ((self (make-instance '%s)))\n self))\n\n"
      name name))
    (clisp-reindent-form)
    (insert (format
             "(proclaim '(inline %s-p))\n\n" name))
    (clisp-reindent-form)
    (insert 
     (format 
      "(defun %s-p (self)\n(eq (class-name (class-of self)) '%s))\n"
      name name))
    (insert 
     (format 
      "\n(defun %s-subtype-p (self)\n(typep  self '%s))\n"
      name name))
    (clisp-reindent-form)))

(defun clisp-add-slot (var-name var-val)
  (insert (format "(%s :initform %s :initarg :%s :accessor %s-%s)\n " 
		  var-name var-val var-name name var-name)))

(defun clisp-add-slots (slot-specifications)
  (unless (zerop (length slots))
    (let ((index 0) var-name var-val)
      (while (< index (length slots))
	     (setq index (read-from-string slots index))
	     (setq var-name (car index))
	     (setq var-val "nil")
	     (and (consp var-name)
		  (progn (setq var-val (car (cdr var-name)))
			 (setq var-name (car var-name))))
	     (setq index (cdr index))
	     (clisp-add-slot var-name var-val))
      (delete-char -2))))

(defun clisp-make-generic-template (name header)
  (let ((args (clisp-prompt-user "Arguments? "))
	(doc (clisp-prompt-user "Documentation? " ""
				*clisp-default-doc-string*)))
    (insert header)
    (insert (format " (%s)\n#+CLOS (:documentation \"%s\")\n )" args doc))
    (backward-char 1)
    (clisp-reindent-form)
    (forward-char 2)))

(defun clisp-make-method-template (name header)
  (let ((classes (clisp-prompt-user "Class(es)? "))
	(doc (clisp-prompt-user "Documentation? " "" 
				*clisp-default-doc-string*)))
    (insert header)
    (insert " (")
    (clisp-add-class-specifiers classes)
    (insert (format ")\n \"%s\"\n)" doc))
    (clisp-reindent-form)
    (backward-char 1)))

(defun clisp-add-class-specifiers (classes)
  (unless (zerop (length classes))
    (let ((index 0) name)
      (while (< index (length classes))
	     (setq index (read-from-string classes index))
	     (setq name (car index))
	     (setq index (cdr index))
	     (insert (format "(%s %s)\n " name name)))
      (delete-char -2))))

;;; CLISP-INSERT-SEPARATOR inserts the *clisp-separator-string* right
;;; before the current line.

(defun clisp-insert-separator ()
  (interactive)
  (beginning-of-line)
  (insert *clisp-separator-string*))

;;; CLISP-INDENT-FOR-COMMENT calls indent-for-comment.

(defun clisp-indent-for-comment ()
  (interactive)
  (indent-for-comment))

;;; CLISP-CONTINUE-COMMENT calls indent-new-comment-line.

(defun clisp-continue-comment ()
  (interactive)
  (indent-new-comment-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation commands.

;;; CLISP-REINDENT-FORM should rejustify a comment if it is called from
;;; within a comment line. Otherwise, if called from within a lisp
;;; form, it should reindent the entire lisp form.

(defun clisp-reindent-form ()
  "Reindents the current form, whether it be comment or code."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (cond ((looking-at ";")
	   (clisp-set-prefix-string)
	   (beginning-of-comment)
	   (let ((begin (point)))
	     (end-of-comment)
	     (fill-region-as-paragraph begin (point))))
	  (t (beginning-of-defun)
	     (next-line 1)
	     (message "Reindenting...")
	     (while (not (or (eobp)
			     (let ((indent (calculate-lisp-indent)))
			       (cond ((consp indent)
				      (zerop (car indent)))
				     (t (zerop indent))))))
	       (lisp-indent-line)
	       (next-line 1))
	     (lisp-indent-line)
	     (message "Reindenting...done.")))))

(local-set-key "\C-c=" 'clisp-make-template)
