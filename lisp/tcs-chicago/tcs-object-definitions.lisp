;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :aster)

;;; Object definitions for tcs chicago
;;{{{ end of sentence marker:

(define-text-object :macro-name "@"
  :number-args 1
  :processing-function at-expand
  :precedence  nil
  :object-name end-of-sentence
  :supers (document)
  :children-are-called nil
  )

(define-text-object :macro-name "sentence"
  :number-args  0
  :processing-function sentence-expand
  :precedence  nil
  :object-name end-of-sentence-mark
  :supers (end-of-sentence)
  :children-are-called nil
  )

;;}}}
;;{{{ absolute sectional units:

(defclass asectional-unit(sectional-unit)
  ((absolute-number  :initform nil
                     :initarg :absolute-number
                     :accessor absolute-number )))

(defclass achapter (asectional-unit )
  ()
  )

(defclass asection (asectional-unit )
  ()
  )

(defclass asubsection (asectional-unit)
  ()
  )

(defclass asubsubsection (asectional-unit)
  ()
  )

(defclass apar(asectional-unit)
  ()
  )

  ;;; Function: CREATE-ASECTIONAL-UNIT                         Author: raman
  ;;; Created: Wed Jan 11 15:23:06 1995

(defun create-asectional-unit (asectional-unit-text
                               &key (sectional-unit-name 'asection))
  "Create a asectional unit, default is a asection."
  (let ((asectional-unit-buffer (make-buffer :contents
                                             asectional-unit-text )))
    (when
        (eq sectional-unit-name (lookat-current-entry asectional-unit-buffer))
      (advance-pointer asectional-unit-buffer)
      (let ((new-asectional-unit  (make-instance sectional-unit-name
                                                 :name (cond
                                                         ((eq sectional-unit-name 'achapter )
                                                          "chapter")
                                                         ((eq sectional-unit-name 'asection )
                                                          "section")
                                                         ((eq sectional-unit-name 'asubsubsection )
                                                          "subsubsection")
                                                         ((eq sectional-unit-name 'apar )
                                                          "paragraph")))))
        (when (can-this-be-cross-referenced? sectional-unit-name)
          (add-enclosing-referend new-asectional-unit))
        (setf (absolute-number new-asectional-unit)
              (get-absolute-section-number!  asectional-unit-buffer ))
        (unless (eq sectional-unit-name 'apar )
          (setf (title new-asectional-unit) (get-unit-title!  asectional-unit-buffer)))
        (setf (body new-asectional-unit)
              (make-paragraph-if-necessary
               (process-text  asectional-unit-buffer
                              #'(lambda(x) (or (is-a
                                                (child-of-asectional-unit
                                                 sectional-unit-name)
                                                (lookat-current-entry
                                                 x))
                                               (end-of-buffer? x))))))
        (when (exists-child-of-asectional-unit? sectional-unit-name )
          (setf (children  new-asectional-unit)
                (get-sectional-units! asectional-unit-buffer
                                      :sectional-unit-name
                                      (child-of-asectional-unit
                                       sectional-unit-name)
                                      )))
        (pop-enclosing-referend)
        new-asectional-unit))))

  ;;; Variable: *VALID-ASECTIONAL-UNIT-NAMES*                  Author: raman
  ;;; Created: Wed Jan 11 15:17:29 1995

(defvar *valid-asectional-unit-names*
  (list 'achapter 'asection 'asubsection 'asubsubsection 'apar)
  "Valid names for absolute sectional units")

(setf *objects-that-can-be-referred*
      (append *objects-that-can-be-referred*
              *valid-asectional-unit-names* ))

  ;;; Function: VALIDATE-aSECTIONAL-UNIT-NAME                   Author: raman
  ;;; Created: Wed Jan 11 15:56:18 1995

(defun validate-asectional-unit-name (unit-name)
  "check if unit-name is a valid asectional unit"
  (or
   (find unit-name *valid-asectional-unit-names* )
   (error "validate-asectional-unit-name: ~a is not a valid  asectional unit name"
          unit-name))
  )

  ;;; Function: EXISTS-CHILD-OF-ASECTIONAL-UNIT                Author: raman
  ;;; Created: Wed Jan 11 15:19:58 1995

(defun exists-child-of-asectional-unit? (asectional-unit-name)
  "Sees if child posible "
  (< (1+ (position
          asectional-unit-name
          *valid-asectional-unit-names*))
     (length *valid-asectional-unit-names*)
     )
  )

  ;;; Function: CHILD-OF-ASECTIONAL-UNIT                       Author: raman
  ;;; Created: Wed Jan 11 15:21:28 1995

(defun child-of-asectional-unit (asectional-unit-name)
  "return name of the child of this unit"
  (when (exists-child-of-asectional-unit? asectional-unit-name)
    (elt *valid-asectional-unit-names*
         (1+
          (position asectional-unit-name
                    *valid-asectional-unit-names*))))
  )

  ;;; Function: GET-ABSOLUTE-SECTION-NUMBER                    Author: raman
  ;;; Created: Wed Jan 11 15:25:53 1995

(defun get-absolute-section-number! (text-buffer)
  "Return first item in buffer and advance pointer if it is a block."
  (process-text-block
   (make-buffer :contents
                (list (pop-when-true text-buffer
                                     #'(lambda(x) (is-a 'block x))))))
  )

;;; install the processing function:
(define-parsing-function 'achapter
    '(lambda(x &key (do-not-test nil ))
      (create-asectional-unit  (pop-current-entry x )
       :sectional-unit-name 'achapter)))

(define-parsing-function 'asection
    '(lambda(x &key (do-not-test nil ))
      (create-asectional-unit  (pop-current-entry x )
       :sectional-unit-name 'asection)))

(define-parsing-function 'asubsection
    '(lambda(x &key (do-not-test nil ))
      (create-asectional-unit  (pop-current-entry x )
       :sectional-unit-name 'asubsection)))

(define-parsing-function 'asubsubsection
    '(lambda(x &key (do-not-test nil ))
      (create-asectional-unit  (pop-current-entry x )
       :sectional-unit-name 'asubsubsection )))

(define-parsing-function 'apar
    '(lambda(x &key (do-not-test nil ))
      (create-asectional-unit  (pop-current-entry x )
       :sectional-unit-name 'apar )))

;;}}}

;;{{{ New environments:

(define-new-environment  :env-name "articleinfo" :class-name article-info)
(define-new-environment :env-name "articletext" :class-name article-text)

;;}}}
