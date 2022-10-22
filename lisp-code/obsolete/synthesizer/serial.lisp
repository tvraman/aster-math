;;; -*- Syntax: Common-lisp; Mode: LISP; Base: 10; Package: DECTALK -*-


(in-package :dectalk)
#+lucid (use-package :lucid-common-lisp)

;;; Serial Line Code

(export '(*stream* setup finish speak-string))

(defvar *serial-device* nil "Dectalk Serial Port.")
#+(and PC386 UNIX) (setf *serial-device*  "/dev/ttyUSB0")
#+SUN4 (setf *serial-device*  "/dev/ttya")
(unless *serial-device*
  (setf *serial-device* "/dev/tty00" ))

(defvar *stream* nil "This is the io stream to the DecTalk synthesizer.")

#-lucid          
(defun make-stream (device)
  "Create and return a stream suitable for talking to DecTalk"
  (open device :direction :io ) )
#+lucid
(defun make-stream (device)
  "Opens and returns a bidirectional split stream. "
  (let
      ((handle (lcl:extract-stream-handle
                (open device :direction :io ))))
    (cond
     ((minusp handle) (error "Error opening device ~a"))
     (t (lcl:make-lisp-stream :input-handle handle
                          :output-handle handle
                          :auto-force t))))
  )

(defun setup ()
  "Create and initialize connection to the Dectalk"
  (setq *stream* (make-stream *serial-device*))
  ;(shell (format nil  "stty  sane 9600 raw  < ~a" *serial-device* ))
  ;(shell (format nil  "stty -echo < ~a" *serial-device* ))
  ;(shell (format nil  "stty ixon ixoff  < ~a" *serial-device* ))
  )
 

(defun finish (&optional abort?)
  (when *stream*
    (close *stream* :abort abort?))
  (setf *stream* nil))


(defun out (char &rest format-args)
  "send a string or char to dectalk"
  (etypecase char
    (character (write-char char *stream*))
    (string (apply #'format *stream* char format-args))
    (symbol (write-string (symbol-name char)))  ; two bonus features
    #-cl2(fixnum (out (int-char char)))
    ))

(defun speak-string (string)
  (setf string (string string))
  (when (> (length string) 0)
    (out string)
    (unless (find (aref string (- (length string) 1)) '(#\. #\! #\?))
      (out #\.))
    (out #\return)))
















