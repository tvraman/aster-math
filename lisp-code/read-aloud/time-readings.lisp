;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;;

;;; Sat Jan  8 12:01:06 EST 1994
;;; Code to time readings.
;;; Introduce a slot internal-time-to-read in object sectional-unit.
;;; If this slot is not set, then guess the time to read based on the
;;; weight.
;;; To set this slot with the right value, use a before and after
;;; method on read-aloud for class sectional-unit that records the
;;; universal time in this slot.
;;; The before method records when it started reading, the after
;;; method computes the difference from the present and stores this
;;; difference
;;; Bugs: Will only work if the default reading style for sectional
;;; units is active.
;;; Will not work if you interrupt the reading and continue, it will
;;; report the total time taken to read the unit.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Function: DURATION                                       Author: raman
  ;;; Created: Sat Jan  8 12:15:13 1994

(defun duration (u-duration)
  "Convert to days hours minutes seconds. "
  (let ((days nil)
        (hours nil)
        (minutes nil)
        (seconds nil))
    (multiple-value-setq (minutes seconds)
      (truncate  (/ u-duration 60)))
    (multiple-value-setq (hours minutes)
      (truncate  (/ minutes 60 )))
    (multiple-value-setq (days hours )
      (truncate  (/ hours  24 )))
    (list days 
     (* 24  hours ) 
     (* 60 minutes)
     (* 60 seconds) ))
  )


(defmethod read-aloud :before ((sectional-unit sectional-unit))
           "Record when reading started."
           (setf  (internal-time-to-read sectional-unit)
                  (get-universal-time)))

(defmethod read-aloud :after ((sectional-unit sectional-unit))
           "After method: Compute how long it took to read. "
           (setf (internal-time-to-read sectional-unit)
                 (- (get-universal-time)
                    (internal-time-to-read sectional-unit ))))

 

  ;;; Method: TIME-TO-READ                                     Author: raman
  ;;; Created: Sat Jan  8 12:08:28 1994

(defmethod time-to-read ((sectional-unit sectional-unit))
  "Time taken to render this sectional unit."
  (let ((guess  (null (internal-time-to-read sectional-unit ))))
    (cond
      (guess (guess-how-long-to-read sectional-unit))
      (t (speak-decoded-time
          (duration  (internal-time-to-read sectional-unit ))))
      )))

(defmethod time-to-read ((article article))
  "Time taken to render this sectional unit."
  (let ((guess  (null (internal-time-to-read article ))))
    (cond
      (guess (guess-how-long-to-read article))
      (t (speak-decoded-time
          (duration  (internal-time-to-read article )))))))

  ;;; Constant: *TIME-FIELDS*                                  Author: raman
  ;;; Created: Sat Jan  8 12:19:20 1994

(defconstant *time-fields*
  '( DAYS HOURS MINUTES SECONDS)
  "Order in which time fields returned by duration. ")


  ;;; Function: SPEAK-DECODED-TIME                             Author: raman
  ;;; Created: Sat Jan  8 12:20:58 1994
;;; Using format directive instead of loop.
;;; <(old version has been backed up)>
(defun speak-decoded-time (decoded-time) 
  "Speaks decoded time. Input should be a list returned by
duration. "
  (assert (listp decoded-time) nil 
          "~a should be a list. " decoded-time)
  (loop for i from 1 to 4
        and field in decoded-time
        and name in *time-fields*
        do
        (unless (zerop field )
          (afl:tts-queue
           (format nil   "~a ~a, "
                   field name ))))
  (afl:tts-force)
  )


  ;;; Method: TABLE-OF-CONTENTS                                Author: raman
  ;;; Created: Sat Jan  8 12:49:47 1994

(defmethod table-of-contents ((sectional-unit sectional-unit))
  "Generate contents for this sectional unit. "
  (let   ((guess (null (internal-time-to-read sectional-unit))))
    (cond
      (guess (summarize sectional-unit)
       (guess-how-long-to-read sectional-unit))
      (t 
       (afl:new-block
        (afl:local-set-state
         (afl:multi-move-to
          afl:*current-speech-state*
          '(afl:left-volume 100 )
          '(afl:right-volume 0)))
        (summarize sectional-unit))
       (afl:new-block
        (afl:local-set-state
         (afl:multi-move-to
          afl:*current-speech-state*
          '(afl:left-volume 0 )
          '(afl:right-volume 100)))
        (time-to-read sectional-unit))
       (loop for unit in (sectional-units sectional-unit ) do  
             (table-of-contents unit )))))
  )


  ;;; Method: TABLE-OF-CONTENTS                                Author: raman
  ;;; Created: Sat Jan  8 12:57:00 1994

(defmethod table-of-contents ((article article))
  "Table of contents for an article."
  (let ((guess (null (internal-time-to-read article ))))
    (cond
      (guess (summarize article)
       (guess-how-long-to-read article))
  (t (loop for unit in  (sectional-units article) do
        (table-of-contents unit )))))
  )


(defun guess-how-long-to-read(document)
  "Guess how long it will take to read this document. "
  (let ((duration
         (duration ( round  (/ 
                             (* (weight document ) 60)
                             (* (afl:get-final-scale-factor 'afl:speech-rate)
                                (afl:current-value 'afl:speech-rate))))
                   )))
    (afl:tts-queue "approximately ")
    (speak-decoded-time duration)
    )
  )
