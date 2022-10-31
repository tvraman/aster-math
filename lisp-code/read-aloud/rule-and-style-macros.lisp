;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;; Macros to conveniently activate and deactivate styles and rules
;;; within other rules.
;;;


  ;;; Macro: WITHOUT-STYLE                                     Author: raman
  ;;; Created: Fri Nov  5 17:57:01 1993

(defmacro without-reading-style ((style-name)  &body body) 
  "Execute body with style style-name deactivated. "
  `(let
    ((style-flag (find ',style-name (current-reading-style ))))
      (unwind-protect
    (progn
      (when style-flag  (deactivate-style ',style-name))
      ,@body)
        (when style-flag (activate-style ',style-name ))))
  )




  ;;; Macro: WITHOUT-READING-RULE                              Author: raman
  ;;; Created: Fri Nov  5 18:04:04 1993

(defmacro without-reading-rule ((object-name rule-name) &body body) 
  "Execute body with rule rule-name for object object-name
  deactivated. "
  `(let
    ((rule-flag
      (eq ',rule-name
          (active-rule  (make-instance ',object-name)))))
    (unwind-protect
         (progn
           (when rule-flag (deactivate-rule ',object-name))
           ,@body)
      (when rule-flag (activate-rule ',object-name ',rule-name))))
  )


  ;;; Macro: WITH-READING-STYLE                             Author: raman
  ;;; Created: Fri Nov  5 21:00:07 1993

(defmacro with-reading-style ((style-name) &body body) 
  "Execute body with style style-name active"
`(let
    ((style-flag (find ',style-name (current-reading-style ))))
      (unwind-protect
    (progn
      (unless  style-flag  (activate-style ',style-name))
      ,@body)
        (unless  style-flag (deactivate-style ',style-name ))))    
  )



  ;;; Macro: WITH-READING-RULE                                 Author: raman
  ;;; Created: Fri Nov  5 21:02:34 1993

(defmacro with-reading-rule ((object-name rule-name) &body body) 
  "Execute body with rule rule-name activated for object object-name.
  "
  `(let
    ((rule-flag
      (eq ',rule-name
          (active-rule  (make-instance ',object-name)))))
    (unwind-protect
         (progn
           (unless  rule-flag (activate-rule ',object-name))
           ,@body)
      (unless  rule-flag (deaactivate-rule ',object-name ',rule-name))))
  )



  ;;; Macro: WITH-ADDED-READING-STYLE                          Author: raman
  ;;; Created: Fri Nov  5 21:20:26 1993

(defmacro with-added-reading-style ((style-name) &body body) 
  "Execute body with style style-name added at the end of currently
active styles."
  `(let
    ((style-flag (find ',style-name (current-reading-style ))))
      (unwind-protect
    (progn
      (unless  style-flag  (add-reading-style ',style-name))
      ,@body)
        (unless  style-flag (deactivate-style ',style-name ))))
)
