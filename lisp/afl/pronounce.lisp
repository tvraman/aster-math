;;;   -*-   Mode: LISP -*-    ;;;
 
 

(in-package :afl)


(export '( get-pronunciation define-pronunciation
          with-pronunciation-mode current-pronunciation-mode
          *pronunciation-mode* *global-pronunciation-mode*
          *pronounce-ignore-case-in-text*
          *always-dehyphenate*
          dehyphenate-word
          ))
;;; Created: Fri Sep 25 11:36:27 EDT 1992
;;; Pronunciation tables for dectalk.
;;; Separate tables for text and math mode.
 

;;; Variable: *PRONUNCIATION-MODE*                           Author: raman
;;; Created: Fri Sep 25 11:37:16 1992

(defvar *pronunciation-mode*  :text " Current pronunciation mode")

  ;;; Variable: *GLOBAL-PRONUNCIATION-MODE*                    Author: raman
  ;;; Created: Thu Mar 25 09:14:35 1993

(defvar *global-pronunciation-mode*  *pronunciation-mode*
  "Global state  for pronunciation")

  ;;; Variable: *VALID-PRONUNCIATION-MODES*                    Author: raman
  ;;; Created: Tue Feb 23 19:58:30 1993

(defvar *valid-pronunciation-modes*
  (list :text :math)
  "Valid pronunciation modes")

  ;;; Function: VALID-PRONUNCIATION-MODE?                      Author: raman
  ;;; Created: Tue Feb 23 19:59:11 1993
(defun valid-pronunciation-mode? (mode)
  "Is this a valid pronunciation mode?"
  (find mode *valid-pronunciation-modes*)
  )

;;; Function: CURRENT-PRONUNCIATION-MODE                     Author: raman
;;; Created: Mon Oct  5 16:37:15 1992

(defun current-pronunciation-mode ()
  "Return current pronunciation mode"
  *pronunciation-mode*
  )

;;; Function: SET-PRONUNCIATION-MODE                         Author: raman
;;; Created: Fri Sep 25 11:38:07 1992

(defun set-pronunciation-mode (mode)
  "Set pronunciation mode "
  (assert (valid-pronunciation-mode? mode) nil
          "Unknown pronunciation mode ~a "
          mode)
  (setf *pronunciation-mode* mode)
  )

;;; Modified: Wed Apr  7 11:46:10 EDT 1993
;;;; define-pronunciation, remove-pronunciation and get-pronunciation
;;;; being made into methods that despatch on the second argument.
;;; <(function version backed up)>
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

;;; Variable: *PRONOUNCE-IGNORE-CASE-IN-TEXT*                Author: raman
;;; Created: Tue Nov 10 15:28:51 1992
;;; external variable:
(defvar *pronounce-ignore-case-in-text* t
  "If t case ignore in text mode when choosing pronunciation")

  ;;; Variable: *ALWAYS-DEHYPHENATE*                           Author: raman
  ;;; Created: Wed May  5 09:34:26 1993
;;; external variable:
(defvar *always-dehyphenate* t
  "Always dehyphenate words. Avoids the dectalk spelling out
things. ")

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
  (declare (optimize (compilation-speed 0) (safety 0) (speed 3 )))
  (let ((lcs (string-downcase string )))
    (or
     (gethash
      (if *pronounce-ignore-case-in-text* lcs string)
      *text-mode-pronunciations*)
     (if *always-dehyphenate* (dehyphenate-word string) string))))

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
;;; Modified: Thu Dec 10 11:01:23 EST 1992
;;; Need to fix it to handle ignore-case, feeling too lazy to do it.
;;; To get the right thing, separate out clauses in the conditional

(defun   get-pronunciation (string &optional (mode *pronunciation-mode*))
  "Get pronunciation for string in current pronunciation mode "
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (get-pronounce-internal    string mode))
  ;;; Macro: WITH-PRONUNCIATION-MODE                           Author: raman
  ;;; Created: Sun Dec 13 09:40:20 1992

;;; Modified: Thu Mar 25 09:39:59 EST 1993
;;; Use afl blocks.

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
