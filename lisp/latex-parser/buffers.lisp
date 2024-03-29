;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;

(in-package :aster)

;;; Implements a buffer structure, which maintains contents as a list,
;;; and a pointer to the current buffer location.
;;; buffer is implemented as a structure.
;;; Operations provided:
;;; pop-next-entry
;;; return next entry and mofify buffer pointer.
;;; lookat-current-entry Return current entry without modifying pointer.

;;; Structure: BUFFER                                        Author: raman
;;; Created: Thu Oct 31 09:37:17 1991
(defstruct (buffer
            (:type list)
            (:constructor make-buffer-internal ))
  (contents nil)
  (pointer  nil)
  (local-env nil))

(defun make-buffer (&key (contents nil) (local-env nil ))
  (make-buffer-internal
   :contents contents
   :pointer contents
   :local-env local-env ))

;;; Function: pop-next-n-entries                       Author: raman
;;; Created: Thu Oct 31 12:39:58 1991

(defun pop-next-n-entries (buff  &optional (n 1))
  "Pops off n entries from buffer, reseting the pointer"
  (declare (fixnum n))
  (cond
    ((<= n 0) (error " negative argument to pop-current-entries")
     nil)
    ((= n 1)(list  (pop (buffer-pointer buff))))
    (t
     (do
      ((result nil
               (setf result (nconc result (list (pop-current-entry buff)))))
       (counter 0 (+ 1 counter)))
      ((= counter n) result)))))

;;; Function: POP-CURRENT-ENTRY                              Author: raman
;;; Created: Thu Oct 31 13:44:16 1991


(defun pop-current-entry (buff)
  "pops current entry off buffer modifyin gpointer"
  (pop (buffer-pointer buff)))

;;; Function: LOOKAT-CURRENT-ENTRY                           Author: raman
;;; Created: Thu Oct 31 13:15:56 1991

(defun lookat-current-entry (buff )
  "Looks at current entry  in buff. Does not modify pointer"
  (first (buffer-pointer buff)))

;;; Function: LOOKAT-NEXT-N-ENTRIES                          Author: raman
;;; Created: Thu Oct 31 13:31:25 1991

(defun lookat-next-n-entries (buff &optional (n 1))
  "Looks at the next n entries in the buffer buff "
  (let ((old-position (buffer-pointer buff))
        (result nil))
    (cond
      ((<= n 0)
       (error ":Negative arg to lookat-next-n-entries") nil)
      (t
       (dotimes (counter n result)
         (setf result (nconc result (list (pop (buffer-pointer buff))))))))
    (setf (buffer-pointer buff) old-position)
    result))

;;; Function: ADVANCE-POINTER                                   Author: raman
;;; Created: Fri Nov  1 15:28:02 1991

(defun  advance-pointer (buff)
  "return buffer after advancing pointer. "
  (pop-current-entry buff)
  buff)

;;; Function: POP-WHILE-TRUE                                 Author: raman
;;; Created: Fri Nov  1 11:07:52 1991

(defun pop-while-true (text-buffer predicate)
  "Pops off entries reseting pointer while predicate is satisfied."
  (loop
    while
    (and (funcall predicate (lookat-current-entry text-buffer ))
         (not (end-of-buffer? text-buffer )))
    collect (pop-current-entry text-buffer )))

;;; Function: POP-WHEN-TRUE                                  Author: raman
;;; Created: Fri Nov  1 11:04:12 1991

(defun pop-when-true  (text-buffer predicate)
  "Pops off first entry in buffer reseting pointer if this satisfies predicate."
  (cond
    ((funcall predicate (lookat-current-entry text-buffer))
     (pop-current-entry text-buffer))
    (t nil)))

;;; Function: END-OF-BUFFER?                                 Author: raman
;;; Created: Tue Nov  5 15:51:47 1991

(defun end-of-buffer? (buff)
  "Checks if pointer is at the end of buff"
  (endp (buffer-pointer buff)))

;;; Function: RESET-POINTER                                  Author: raman
;;; Created: Thu Nov  7 19:14:30 1991

(defun reset-pointer (buff &optional (pos nil))
  "Resets buffer pointer to point to the beginning of buff or pos if supplied."
  (cond
    ((null pos) (setf (buffer-pointer buff) (buffer-contents buff )))
    (t (setf (buffer-pointer buff) pos))))
