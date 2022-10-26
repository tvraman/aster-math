;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;; Sat Apr 17 13:31:30 EDT 1993
;;; Summary reading style.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;; Parameter: *PARAGRAPH-SUMMARY-LENGTH*                    Author: raman
  ;;; Created: Sat Apr 17 13:36:29 1993

(defparameter *paragraph-summary-length*  'sentence
  "Read so many words to
summarize a paragraph.
 If 'sentence, read first sentence. If 'clause, read first clause. ")

(def-reading-rule (paragraph summarize)
    "Summarize reading rule for a paragraph. "
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (afl:new-block (afl:local-set-state :text)
                 (with-slots
                     ((contents contents ))
                   paragraph
                   ;(afl:tts-icon "paragraph")
                   (cond
                     ((and *paragraph-summary-length*
                           (not (word-p (first contents ))))
                      (afl:tts-queue "contains: ")
                      (summarize (first contents )))
                     (*paragraph-summary-length* 
                      (loop for word in contents
                            and count = 1 then (+ 1 count)
                            until
                            (or
                             (and (numberp *paragraph-summary-length*)
                                  (= count *paragraph-summary-length*))
                             (and (eql  'sentence
                                        *paragraph-summary-length*)
                                  (end-of-sentence? word))
                             (and (eql 'clause *paragraph-summary-length*)
                                  (punctuation? word))
                             (endp contents))  do 
                            (read-aloud  word )))
                     (t (afl:tts-queue " paragraph ")
                        (afl:tts-force)
                        (afl:synchronize-and-play 
                    *paragraph-cue* :background-flag t)
                        ))))
  (afl:tts-force)
  )

(def-reading-rule (slide read-only-display-math)
    "Render only the display math from a slide. "
  (with-slots ((contents contents )) slide
              (loop for item in contents do
                    (when (paragraph-p item)
                      (reading-rule item 'read-only-display-math))))
  )
