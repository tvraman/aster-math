;;;   -*-   Mode: LISP -*-    ;;;
 
 

(in-package :afl)


(export
 '( get-pronunciation define-pronunciation
   with-pronunciation-mode current-pronunciation-mode
   *pronunciation-mode* dehyphenate-word))
;;; Created: Fri Sep 25 11:36:27 EDT 1992
;;; Pronunciation tables for dectalk.
;;; Separate tables for text and math mode.
 

;;; Variable: *PRONUNCIATION-MODE*                           Author: raman
;;; Created: Fri Sep 25 11:37:16 1992

(defvar *pronunciation-mode*  :text " Current pronunciation mode")

  ;;; Function: VALID-PRONUNCIATION-MODE?                      Author: raman
  ;;; Created: Tue Feb 23 19:59:11 1993
(defun valid-pronunciation-mode? (mode)
  "Is this a valid pronunciation mode?"
  (find mode '(:text :math)))

;;; Function: SET-PRONUNCIATION-MODE                         Author: raman
;;; Created: Fri Sep 25 11:38:07 1992

(defun set-pronunciation-mode (mode)
  "Set pronunciation mode "
  (assert (valid-pronunciation-mode? mode) nil
          "Unknown pronunciation mode ~a "
          mode)
  (setf *pronunciation-mode* mode))


(defmethod get-pronounce-internal ((string string) (mode t))
  "Default method, unknown mode. "
  (error "Pronunciation mode ~a has no get-pronunciation-internal
  method "
         mode)
  )

;;{{{ text mode

;;; Variable: *TEXT-MODE-PRONUNCIATIONS*                     Author: raman
;;; Created: Fri Sep 25 11:40:46 1992

(defvar *text-mode-pronunciations*
  (make-hash-table :test #'equal  )
  "Pronunciations in text mode.")

;;; Method: DEFINE-PRONUNCIATION                           Author: raman
;;; Created: Fri Sep 25 11:42:42 1992

(defmethod  define-pronunciation  (( string string ) (pronounced-as string)
                                   (mode (eql :text )))
  " Define pronunciation for string in text mode  "
  (setf (gethash string *text-mode-pronunciations*) pronounced-as)
  )

;;; Method: REMOVE-PRONUNCIATION                           Author: raman
;;; Created: Tue Oct 27 15:55:18 1992

(defmethod  remove-pronunciation ( (string string) (mode (eql :text )))
  "Remove pronunciation for string in text mode "
  (remhash string *text-mode-pronunciations*)
  )
(defun dehyphenate-word (str)
  "Remove hyphens and replace by spaces."
  (let ((result " "))
    (loop for w in
                (uiop:split-string str :separator "-")
          do
             (setq result (concatenate 'string result w " ")))
    result))

;;; Method: GET-PRONOUNCE-INTERNAL                           Author: raman
;;; Created: Wed Apr  7 12:03:12 1993

(defmethod get-pronounce-internal ((string string) (mode (eql :text )))
  "Internal method for getting pronunciation in text mode"
  (let ((lcs (string-downcase string )))
    (or
     (gethash lcs *text-mode-pronunciations*)
       (dehyphenate-word string))))

;;}}}
;;{{{ math mode

;;; Variable: *MATH-MODE-PRONUNCIATIONS*                     Author: raman
;;; Created: Fri Sep 25 11:40:46 1992

(defvar *math-mode-pronunciations*
  (make-hash-table :test #'equal )
  "Pronunciations in math mode.")

;;; Variable: *PRONOUNCE-IGNORE-CASE-IN-MATH*                Author: raman
;;; Created: Tue Nov 10 15:27:54 1992

(defvar *pronounce-ignore-case-in-math* nil
  "If t ignore case when deciding pronunciation")

;;; Method: DEFINE-PRONUNCIATION                           Author: raman
;;; Created: Fri Sep 25 11:42:42 1992

(defmethod  define-pronunciation  (( string string ) (pronounced-as string)
                                   (mode (eql :math )))
  " Define pronunciation for string in math mode  "
  (setf (gethash string *math-mode-pronunciations*) pronounced-as)
  )

;;; Method: REMOVE-PRONUNCIATION                           Author: raman
;;; Created: Tue Oct 27 15:55:18 1992

(defmethod  remove-pronunciation ( (string string) (mode (eql :math )))
  "Remove pronunciation for string in math mode "
  (remhash string *math-mode-pronunciations*)
  )

  ;;; Method: GET-PRONOUNCE-INTERNAL                           Author: raman
  ;;; Created: Wed Apr  7 12:03:12 1993
(defmethod get-pronounce-internal ((string string) (mode (eql :math )))
  "Internal method for getting pronunciation in math mode"
  (declare (optimize (compilation-speed 0) (safety 0) (speed 3 )))
  (let ((lower-case-string (string-downcase string )))
    (or                                 ; first disjunct:
     (gethash (if *pronounce-ignore-case-in-math* lower-case-string
                  string)
              *math-mode-pronunciations*)
     string  )                          ; second disjunct: default  is string
    )
  )

;;}}}
;;{{{ lisp mode

  ;;; Method: GET-PRONOUNCE-INTERNAL                           Author: raman
  ;;; Created: Wed Apr  7 12:03:12 1993



;;}}}

;;; Function: get-PRONUNCIATION                              Author: raman
;;; Created: Fri Sep 25 11:48:28 1992
(defun   get-pronunciation (string &optional (mode *pronunciation-mode*))
  "Get pronunciation for string in current pronunciation mode "
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (get-pronounce-internal    string mode))
  ;;; Macro: WITH-PRONUNCIATION-MODE                           Author: raman
  ;;; Created: Sun Dec 13 09:40:20 1992

(defmacro with-pronunciation-mode  ((&key mode ) &body body)
  "Execute body in this pronunciation mode. "
  `(new-block
     (local-set-state ,mode)
     ,@body)
  )

(defmacro old-with-pronunciation-mode  ((&key mode ) &body body)
  "Execute body in this pronunciation mode. "
  `(let  ((saved-mode *pronunciation-mode*))
     (unwind-protect
          (progn
            (set-pronunciation-mode  ,mode)
            ,@body)
       (set-pronunciation-mode saved-mode))
     )
  )
