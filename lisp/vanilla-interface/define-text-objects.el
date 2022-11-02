;;; Fri Oct 16 11:12:59 EDT 1992
;;; Author: T. V. Raman
;;; Contains elisp functions that  are used to generate the
;;; definitions of text objects.
(provide 'define-text-objects)
;;; 1) Generate call to define-text-object
;;; 2) Generate read-aloud method
;;; 3) Leave point inside the body of the empty method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; generate define-text-object call

(defun generate-define-text-object()
  (interactive)
  "Generate call to define-text-object"
  (let
      ((macro-name (read-from-minibuffer "Enter macro name: "
                                         nil nil nil)) ; get a string
       (number-of-args (read-minibuffer "Number of arguments: " )) 
       (processing-function
        (read-minibuffer
         "processing function: "))
       (precedence
        (read-minibuffer  "Precedence same as  existing operator: " ))
       (object-type (read-minibuffer "Enter object type: "))
       (children-are-called  (read-minibuffer
                              "Children are called:  : a list"))
       (supers (read-minibuffer
                "Supers: a list"))
       )
;;; Now generate the template:
    (save-excursion 
      (insert
       (format
        "(define-text-object \
:macro-name \"%s\" \n\
:number-args %s\n\
:processing-function %s \n\
:precedence  %s \n\
:object-name %s\n\
:supers %s\n\
:children-are-called %s\n\
)\n\n"
        macro-name number-of-args processing-function
        precedence  object-type supers children-are-called )))
    (indent-sexp)
    (forward-sexp 1)
    (insert (format "\n\n"))
    
    (if (= 0 number-of-args )
        (insert
         (format ";;; Object has 0 slots \n"))
        (insert (format
                 ";;; Use  (argument object)  1 ...( argument
                        ;;; object %d)  in\
                         read-aloud \n" number-of-args)))
    (generate-read-aloud-method object-type)
    )
  )

(defun generate-read-aloud-method(class) 
  (interactive "SEnter object name")
  "Generate an empty read aloud method for class class" 
  (save-excursion
    (insert
     (format
      "(defmethod read-aloud  (( %s %s )) \n\
\"Read aloud method for object %s \"\n\
\n )\n"
      class class class))
    )
  (indent-sexp)
  (forward-line  2)
  )
;;; Generate read-aloud call template with slotafter prompting for
;;; slot number

(defun generate-read-aloud-call-on-slot()
  (interactive)
  "Generate (read-aloud  (argument class n)) "
  (let
      ((slot-number (read-minibuffer"Enter slot number: " )))
    (save-excursion
      (beginning-of-defun)
      (forward-char  1)
      (forward-sexp 2 )
      (forward-char 2)
      (mark-sexp 1)
      (copy-region-as-kill (point) (mark)))
;;; Now generate template: 
    (insert  (format
              "(read-aloud (argument %d  " slot-number))
    (yank)
    (insert(format " ))  "))
    (beginning-of-line)
    (lisp-indent-line)
    (end-of-line))
  )



(defun generate-read-aloud-call-on-class-slot()
  (interactive)
  "Generate (read-aloud  (argument class n)) "
  (let
      ((slot-number (read-minibuffer"Enter slot number: " ))
       (class-name (read-minibuffer "enter class name: "))
       )
;;; Now generate template: 
    (insert  (format
              "(read-aloud (argument %s %d ))" 
              slot-number class-name))
    (beginning-of-line)
    (lisp-indent-line)
    (end-of-line))
  
  )

