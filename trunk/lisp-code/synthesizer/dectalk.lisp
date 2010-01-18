;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; functions and variables this package needs to provide
;;; await-silence
;;; space (smart-talker.lisp)
;;; speak-file (simple function in local.lisp can be moved here)
;;; *stream* (serial.lisp)
;;; should-i-continue (smart-talker.lisp)
;;; should-i-stop (smart-talker.lisp)
;;; force-speech (smart-talker.lisp)
;;; send-text (smart-talker.lisp)
;;; speak-number-string (smart-talker.lisp)
;;; speak-string (serial.lisp)  
;;; *dribbling-to-file*
;;;
;;; read from a *stream*:
(in-package :dectalk)

(export '(await-silence speak-file))

;;; Mon Nov 21 18:25:18 PST 1994 wab
;;; generating unique index
;;; numbers each time await silence is called. This should not be
;;; absolutely necessary but is provided just to be safe.
;;; this purposely avoids index 0 because the dectalk returns
;;; nothing if the index is 0.
(let ((previous-index 0)
      (max-index-number 32767))
  (defun new-index ()
     (setf previous-index (+ 1 (mod  previous-index max-index-number)))))

(defconstant *max-wait-period* 500
  "number of times to look for the index from the dectalk before giving up approximately in units of *sleep-period* ") 
(defconstant  *millisecond* (/ 1.0 1000.0))
(defconstant *sleep-period* *millisecond*
  "seconds to sleep while waiting for dectalk to send out another character")

(defvar *escape* #\Escape )
#+:ACLPC (defvar *vt* #\CONTROL-K )
#-:ACLPC (defvar *vt* #\VT )
(defvar *return-index-string* nil "index string expected back from dectalk")
(defun send-index-to-dectalk ()
  (let* ((current-index (new-index))
    (index-string
#+:multivoice (format nil "~aP0;21;~az~a\\~a" *escape* current-index *escape* *vt*)
#+:express (format nil "[:index reply ~a]~a" current-index *vt*)
	))
    (setf *return-index-string*
#+:multivoice (format nil "~aP;31;~az~a\\" *escape* current-index *escape*)
#+:express  (format nil "[:index ~a]" current-index)
	  )
    (send-text index-string)
    (force-output *stream* )))
    
(defun await-silence ()
  "Simple version of await silence "
  (cond
   (*dribbling-to-file* (format *stream* "~%" ))
   (t
    ;; first slurp up anything pending on the line 
    (loop with c =(read-char-no-hang *stream*) 
          while c do (setq c (read-char-no-hang *stream* )))
    ;; send the index 
    (send-index-to-dectalk)
    ;; wait for it to return, then slurp up everything 
    (loop with c =(read-char *stream*) 
          while c do (setq c (read-char-no-hang *stream* ))))))
  


;(defun await-silence ()
;  "send-index-to-dectalk-and-wait-for-its-return"
;  (cond
;   (*dribbling-to-file* (format *stream* "~%" ))
;   (t
;    (send-index-to-dectalk)
;    (loop
;     with c = (read-char *stream* nil nil)
;     with tc = nil
;                                        ;with time-out = 0
;     with index-returned = nil
;     with index-string-mark = 0
;     with index-string-end = (length *return-index-string*)
;     do
;     (progn
;                                        ;(peek-char nil *stream* nil nil t)
;       (setf c (read-char-no-hang *stream* nil nil))
;                                        ;(incf time-out)
;       ;(sleep *sleep-period*)
;       ;;; bill, so what happens if seen is nil? 
;       (if c
;           (cond
;            ((equalp c (char *return-index-string* index-string-mark ))
;               (incf index-string-mark)
;               (when (equalp index-string-mark index-string-end)
;                   (setf index-returned t)))
;            (t (setf index-string-mark 0)))
;         )
;       )
;                                        ;until (or (> time-out *max-wait-period*) index-returned)
;     until  index-returned
;     )))
;  )

