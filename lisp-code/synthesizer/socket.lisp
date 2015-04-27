;;; -*- Syntax: Common-lisp; Mode: LISP; Base: 10; Package: DECTALK -*-


(in-package :dectalk)
#+lucid (use-package :lucid-common-lisp)

;;; Code that talks over a socket

(export '(*stream* setup finish speak-string))

(defvar *serial-port* 12534
  "TTS socket.")
(defvar *serial-device* nil
  "TTS listener.")


(defun make-stream (device)
  "Create and return a stream suitable for talking to the speech server."
  (open device :direction :io ) )


(defun setup ()
  "Create and initialize connection to the TTS server"
  (setq *stream* (make-stream *serial-device*)))
 

(defun finish (&optional abort?)
  (when *stream*
    (close *stream* :abort abort?))
  (setf *stream* nil))


(defun out (char &rest format-args)
  "send a string or char to TTS"
  (etypecase char
    (character (write-char char *stream*))
    (string (apply #'format *stream* char format-args))
    (symbol (write-string (symbol-name char)))))

(defun speak-string (string)
  (setf string (string string))
  (when (> (length string) 0)
    (out string)
    (unless (find (aref string (- (length string) 1)) '(#\. #\! #\?))
      (out #\.))
    (out #\return)))
