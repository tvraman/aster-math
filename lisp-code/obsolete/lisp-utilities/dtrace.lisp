;;; -*- Mode: Lisp; Package: DTRACE -*-
;;; Modified: T. V. Raman
;;; Thu Nov  5 20:11:14 EST 1992
;;; Modifying the display functions to optionally audio format what is
;;; displayed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; DTRACE is a portable alternative to the Common Lisp TRACE and UNTRACE
;;; macros.  It offers a more detailed display format than the tracing
;;; tool most Common Lisp implementations provide.
;;;
;;; From the book "Common Lisp:  A Gentle Introduction to
;;;      Symbolic Computation" by David S. Touretzky.  
;;; The Benjamin/Cummings Publishing Co., 1990.
;;;
;;; This version is for Lucid Common Lisp.
;;;
;;; User-level routines:
;;;   DTRACE  - same syntax as TRACE
;;;   DUNTRACE - same syntax as UNTRACE
;;;
;;; Copyright (c) 1988,1989 Symbolic Technology, Ltd.
;;; This software may not be sold or used for commercial purposes without 
;;; prior written permission from the copyright holder.

;(in-package "DTRACE" :use '("LISP"afl user))
(in-package 'user)

;(export '(dtrace::dtrace dtrace::duntrace
;          *dtrace-print-length* *dtrace-print-level*
;          *dtrace-print-circle* *dtrace-print-pretty*
;          *dtrace-print-array*))
;(import  '(user::read-aloud))
;(shadowing-import '(dtrace::dtrace dtrace::duntrace) (find-package "USER"))

;(use-package "DTRACE" "USER")


;;; DTRACE and related routines.

(defparameter *dtrace-print-length* 7)
(defparameter *dtrace-print-level*  4)
(defparameter *dtrace-print-circle* t)
(defparameter *dtrace-print-pretty* nil)
(defparameter *dtrace-print-array* *print-array*)

(defvar *traced-functions* nil)
(defvar *trace-level* 0)

(defmacro dtrace (&rest function-names)
  "Turns on detailed tracing for specified functions.  Undo with DUNTRACE."
  (if (null function-names)
      (list 'quote *traced-functions*)
      (list 'quote (mapcan #'dtrace1 function-names))))

(defun dtrace1 (name)
  (unless (symbolp name)
    (format *error-output* "~&~S is an invalid function name." name)
    (return-from dtrace1 nil))
  (unless (fboundp name)
    (format *error-output* "~&~S undefined function." name)
    (return-from dtrace1 nil))
  (eval `(untrace ,name))	;if they're tracing it, undo their trace
  (duntrace1 name)		;if we're already tracing it, undo our trace
  (when (special-form-p name)
    (format *error-output*
	    "~&Can't trace ~S because it's a special form." name)
    (return-from dtrace1 nil))
  (if (macro-function name)
      (trace-macro name)
      (trace-function name))
  (setf *traced-functions* (nconc *traced-functions* (list name)))
  (list name))

; The functions below reference DISPLAY-... routines that are implementation-specific.
; These routines are defined at the end of the file.

(defun trace-function (name)
  (let* ((formal-arglist (fetch-arglist name))
	 (old-defn (symbol-function name))
	 (new-defn
	  #'(lambda (&rest argument-list)
	      (let ((result nil))
		(display-function-entry name)
		(let ((*trace-level* (1+ *trace-level*)))
		  (with-dtrace-printer-settings
		   (show-function-args argument-list formal-arglist))
		  (setf result (multiple-value-list (apply old-defn argument-list))))
		(display-function-return name result)
		(values-list result)))))
    (setf (get name 'original-definition) old-defn)
    (setf (get name 'traced-definition) new-defn) ;in case function was re-DEFUN'ed while traced
    (setf (get name 'traced-type) 'defun)
    (setf (symbol-function name) new-defn)))

(defun trace-macro (name)
  (let* ((formal-arglist (fetch-arglist name))
	 (old-defn (macro-function name))
	 (new-defn
	  #'(lambda (macro-args env)
	      (let ((result nil))
		(display-function-entry name 'macro)
		(let ((*trace-level* (1+ *trace-level*)))
		  (with-dtrace-printer-settings
		   (show-function-args macro-args formal-arglist))
		  (setf result (funcall old-defn macro-args env)))
	(display-function-return name (list result) 'macro)
		(values result)))))
    (setf (get name 'original-definition) old-defn)
    (setf (get name 'traced-definition) new-defn) ;in case function was re-DEFUN'ed while traced
    (setf (get name 'traced-type) 'defmacro)
    (setf (macro-function name) new-defn)))


(defun show-function-args (actuals formals &optional (argcount 0))
  (cond ((null actuals) nil)
	((null formals) (handle-args-numerically actuals argcount))
	(t (case (first formals)
	     (&optional (show-function-args actuals (rest formals) argcount))
	     (&rest (show-function-args (list actuals) (rest formals) argcount))
	     (&key (handle-keyword-args actuals))
	     (&aux (show-function-args actuals nil argcount))
	     (t (handle-one-arg (first actuals) (first formals))
		(show-function-args (rest actuals) (rest formals) (1+ argcount)))))))

(defun handle-args-numerically (actuals argcount)
  (dolist (x actuals)
    (incf argcount)
    (display-arg-numeric x argcount)))

(defun handle-one-arg (val varspec)
  (cond ((atom varspec) (display-one-arg val varspec))
	(t (display-one-arg val (first varspec))
	   (if (third varspec)
	       (display-one-arg t (third varspec))))))

(defun handle-keyword-args (actuals)
  (cond ((null actuals))
	((keywordp (first actuals))
	 (display-one-arg (second actuals) (first actuals))
	 (handle-keyword-args (rest (rest actuals))))
	(t (display-one-arg actuals "Extra args:"))))



;;; DUNTRACE and related routines.

(defmacro duntrace (&rest function-names)
  "Turns off tracing for specified functions.  With no args, turns off all tracing."
  (setf *trace-level* 0)  ;safety precaution in case things got broken
  (list 'quote (mapcan #'duntrace1 (or function-names *traced-functions*))))

(defun duntrace1 (name)
  (unless (symbolp name)
    (format *error-output* "~&~S is an invalid function name." name)
    (return-from duntrace1 nil))
  (setf *traced-functions* (delete name *traced-functions*))
  (let ((orig-defn (get name 'original-definition 'none))
	(traced-defn (get name 'traced-definition))
	(traced-type (get name 'traced-type 'none)))
    (unless (or (eq orig-defn 'none)
		(not (fboundp name))
		(not (equal traced-defn
			 (ecase traced-type
			   (defun (symbol-function name))
			   (defmacro (macro-function name))))))  ;in case redefined
      (ecase traced-type
	(defun (setf (symbol-function name) orig-defn))
	(defmacro (setf (macro-function name) orig-defn)))))
  (remprop name 'traced-definition)
  (remprop name 'traced-type)
  (remprop name 'original-definition)
  (list name))


;;; T. V. Raman
;;; Variables affecting audio display.


;;; Variable: *AUDIO-DISPLAY-ON-ENTRY*                       Author: raman
;;; Created: Thu Nov  5 20:13:13 1992

(defvar *audio-display-on-entry* t
  "If t, dtrace will  use an audio display upon entry to a dtraced
function")


;;; Variable: *AUDIO-DISPLAY-ON-EXIT*                        Author: raman
;;; Created: Thu Nov  5 20:14:25 1992

(defvar *audio-display-on-exit* t
  "If t, dtrace will audio display on exiting a dtraced function")

;;; Display routines.


; The code below generates vanialla character output for ordinary terminals.
; It can be replaced with special graphics code if the implementation permits,
; e.g., on a PC you can use the IBM graphic character set to draw nicer looking
; arrows.  On a color PC you can use different colors for arrows, for function,
; names, for argument values, and so on.
;;; And on an audio display, you can audio format!
;;; --Raman

(defmacro with-dtrace-printer-settings (&body body)
  `(let ((*print-length* *dtrace-print-length*)
	 (*print-level* *dtrace-print-level*)
	 (*print-circle* *dtrace-print-circle*)
	 (*print-pretty* *dtrace-print-pretty*)
	 (*print-array* *dtrace-print-array*))
     ,@body))

(defparameter *entry-arrow-string* "----")
(defparameter *vertical-string*    "|   ")
(defparameter *exit-arrow-string*  " \\--")


;;; Parameter: *ENTRY-CUE*                                   Author: raman
;;; Created: Thu Nov  5 20:16:46 1992

(defparameter *entry-cue*
"/home/raman/sounds/cues/item.au"
  "Sound cue played on entry")


;;; Parameter: *EXIT-CUE*                                    Author: raman
;;; Created: Thu Nov  5 20:17:08 1992

(defparameter *exit-cue*
"/home/raman/sounds/cues/paragraph.au" 
  "Sound cue played on exit")

(defparameter *trace-wraparound* 15)


(defun display-function-entry (name &optional ftype)
  (space-over)
  (draw-entry-arrow)
  
  (when *audio-display-on-entry* 
    (with-reading-state  (reading-state  'center)
     (user::read-aloud
      (format nil  " ~S " name ))))
  (format *trace-output* "Enter ~S" name)
  (if (eq ftype 'macro)
      (format *trace-output* " macro")))

(defun display-one-arg (val name)
  (space-over)
  (when *audio-display-on-entry*
(with-reading-state (reading-state 'variable-name) 
     (read-aloud name))
(with-reading-state (reading-state 'variable-value)
     (read-aloud val )))
  (format *trace-output*
	  (typecase name
	    (keyword "  ~S ~S")
	    (string  "  ~A ~S")
	    (t "  ~S = ~S"))
	  name val))

(defun display-arg-numeric (val num)
  (space-over)
  (when *audio-display-on-entry*
    (user::read-aloud
     (read-aloud  (format nil "argument ~d is " num))
     (read-aloud val )))
  (format *trace-output* "  Arg-~D = ~S" num val))

(defun display-function-return (name results &optional ftype)
  (with-dtrace-printer-settings
      (space-over)
    (draw-exit-arrow)
    (when *audio-display-on-exit*
      (afl:new-block
       (user::read-aloud
        (format nil  "~S ~A"
                name
                (if (eq ftype 'macro) "expanded to " "returned " )))))
    (format *trace-output* "~S ~A"
	    name
	    (if (eq ftype 'macro) "expanded to" "returned"))
    (cond ((null results))
	  ((null (rest results))
           (when *audio-display-on-exit*
             (with-reading-state (reading-state 'return-value )
               (read-aloud
                (first results))))
           (format *trace-output* " ~S" (first results)))
	  (t
           (when *audio-display-on-exit*
             (with-reading-state (reading-state 'return-value) 
               (mapcar #'read-aloud
                       (butlast results)
                       (car (last results ))))))
          (format *trace-output* " values ~{~S, ~}~s"
                  (butlast results)
                  (car (last results))))))

(defun space-over ()
  (format *trace-output* "~&")
  (dotimes (i (mod *trace-level* *trace-wraparound*))
    (format *trace-output* "~A" *vertical-string*)))

(defun draw-entry-arrow ()
  (when *entry-cue*
    (dectalk:synchronize-and-play *entry-cue*))
  (when *audio-display-on-entry*
    (read-aloud  (format nil " ~s. "(+ 1 *trace-level* ))))
  (format *trace-output* "~A" *entry-arrow-string*))

(defun draw-exit-arrow ()
  (when *exit-cue*
    (dectalk:synchronize-and-play *exit-cue*))
  (when  *audio-display-on-exit*
    (read-aloud (format nil " ~s. "(+ 1 *trace-level* ))))
  (format *trace-output* "~A" *exit-arrow-string*))


;;; Other implementation-specific routines go here.

; The function FETCH-ARGLIST is implementation-specific.  It returns the
; formal argument list of a function, exactly as the list would appear in
; a DEFUN or lambda expression.
;
;  The version below is for CMU Common Lisp.

#+cmu (defun fetch-arglist (x &optional original-x)
  (format t "~&arglist is ~S" x)
  (cond ((symbolp x) (fetch-arglist (symbol-function x) x))
	((compiled-function-p x)
	 (read-from-string
	  (lisp::%primitive header-ref x lisp::%function-arg-names-slot)))
	((listp x) (case (first x)
		     (lambda (second x))
		     (lisp::%lexical-closure% (fetch-arglist (second x)))
		     (system:macro '(&rest "Form ="))
		     (t '(&rest "Arglist:"))))
	(t (cerror (format nil
			   "Use a reasonable default argument list for ~S" original-x)
		   "Unkown object in function cell of ~S:  ~S" original-x x)
	   '())))

#+lucid (defun fetch-arglist (x) (system::arglist x))
