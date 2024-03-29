;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :aster)

;;; Sun May 16 13:53:22 EDT 1993
;;; User defined bookmarks.
;;; Basic functionality:
;;; Stop reading, move pointer around and give current pointer
;;; location a name.
;;; Later on, user can move to this mark.
;;; Smarts:

  ;;; Variable: *BOOKMARKS*                                    Author: raman
  ;;; Created: Sun May 16 13:59:54 1993

(defvar *bookmarks* (make-hash-table :test #'equal)
  "Table of bookmarks. ")

  ;;; Function: SET-BOOKMARK                                   Author: raman
  ;;; Created: Sun May 16 14:01:13 1993

(defun set-bookmark (tag object)
  "Define tag as a bookmark for object. "
  (setf (gethash tag *bookmarks* ) object )
  )

(defun remove-bookmark (tag)
  "Remove bookmark"
  (remhash tag *bookmarks* )
  )

  ;;; Function: GET-BOOKMARK                                   Author: raman
  ;;; Created: Sun May 16 14:01:58 1993

(defun get-bookmark (tag)
  "Get object marked by this bookmark tag . "
  (gethash tag *bookmarks* )
  )

  ;;; Function: MARK-READ-POINTER                              Author: raman
  ;;; Created: Sun May 16 14:04:44 1993

(defun mark-read-pointer ()
  "Mark current location of read pointer. "
  (let ((tag nil ))
    (afl:tts-queue "Enter bookmark name. ")
    (set-bookmark tag *read-pointer* )
    (values))
  )

  ;;; Function: FOLLOW-BOOKMARK                                Author: raman
  ;;; Created: Sun May 16 14:08:54 1993

(defun follow-bookmark ()
  "Follow bookmark. "
  (let ((tag nil)
        (referend nil))
    (afl:tts-queue "Follow which bookmark? ")
    (setf referend (get-bookmark tag ))
    (cond
      ((null referend) (afl:tts-queue "No such bookmark defined. "))
      (t (save-pointer-excursion
           (read-aloud referend )))
      )
    )
  )

(defun goto-bookmark ()
  "Follow bookmark. "
  (let ((tag nil)
        (referend nil))
    (afl:tts-queue "goto  which bookmark? ")
    (setf referend (get-bookmark tag ))
    (cond
      ((null referend) (afl:tts-queue "No such bookmark defined. "))
      (t (setf *read-pointer* referend)
         (save-pointer-excursion
           (read-aloud referend )))
      )
    )
  )
