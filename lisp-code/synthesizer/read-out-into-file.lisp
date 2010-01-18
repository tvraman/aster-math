;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :dectalk)

;;; Contains functions to make read-aloud dribble to a file instead of
;;; speaking on the dectalk.
;;; with help from the net: <(refer to duplicating streams)>

(export '(
          with-file-as-well-as-dectalk
          with-std-out-as-well-as-dectalk
          with-file-instead-of-dectalk
          *dribbling-to-file*
          ))
;;; Macro: WITH-FILE-INSTEAD-OF-DECTALK                      Author: raman
;;; Created: Sun Oct  4 13:59:38 1992

        

;;; Variable: *DRIBBLING-TO-FILE*                            Author: raman
;;; Created: Wed Oct  7 11:10:30 1992

(defvar *dribbling-to-file* nil "If t, no synchronization commands are used on the dectalk")
(defmacro with-file-as-well-as-dectalk ((&key (filename "temp")) &body body) 
  "Read into file instead of speaking. "
  `(let (
         (saved-stream *stream*)
         (*dribbling-to-file* t)
         )
    (unwind-protect
         (with-open-stream
             (file-stream
              (open
               (merge-pathnames
                (user-homedir-pathname)
                ,filename )
               :direction :output
               :if-does-not-exist :create
               :if-exists :rename 
               ))
           (let
               ((output-stream (make-broadcast-stream
                                *stream*
                                file-stream))
                (input-stream *stream*)
                (*dribbling-to-file* t))
             (setf *stream*
                   (make-two-way-stream
                    input-stream
                    output-stream))
             ,@body
             ))
      (setf *stream* saved-stream)
      )
    )
  )



(defmacro with-std-out-as-well-as-dectalk (&body body)
  "Read into file instead of speaking. "
  `(let (
         (saved-stream *stream*)
         (*dribbling-to-file* t)
         )
    (unwind-protect
           (let
               ((output-stream (make-broadcast-stream
                                *stream*
                                *standard-output*))
                (input-stream *stream*))
             (setf *stream*
                   (make-two-way-stream
                    input-stream
                    output-stream))
             ,@body
             )
      (setf *stream* saved-stream)
      )
    )
  )
(defmacro with-file-instead-of-dectalk ((&key (filename "temp")) &body body) 
  "Read into file instead of speaking. "
  `(let (
         (saved-stream *stream*)
         (*dribbling-to-file* t)
         )
    (unwind-protect
         (with-open-stream
             (file-stream
              (open
               (merge-pathnames
                (user-homedir-pathname)
                ,filename )
               :direction :output
               :if-does-not-exist :create
               :if-exists :rename 
               ))
           (let
               ((output-stream  file-stream)
                (*dribbling-to-file* t))
             (setf *stream*
                   output-stream)
             ,@body
             ))
      (setf *stream* saved-stream)
      )
    )
  )
