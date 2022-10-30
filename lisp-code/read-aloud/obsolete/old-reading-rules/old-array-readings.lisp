;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)


;;; Fri Dec 18 09:23:55 EST 1992
;;; Contains the old reading rules for arrays using directional audio.
;;;
;;; These rules work by changing the step size, and then stepping
;;; along the dimension.
;;; Since afl provides a move-by operator as well, I think the above
;;; is an unnecessarily complicated approach.
;;; Rewriting these with move-by and then compare the rules.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-reading-rule (math-array simple)
  "Simple reading rule for arrays, uses directional audio"
  (with-pronunciation-mode
      (:mode :math) 
    (let ((contents
            (if *transpose-table*
                (transpose-table (contents math-array))
                (contents math-array ))))
      (loop
        for row in   contents 
        do
           (afl:new-block
             (afl:with-surrounding-pause *math-surround*
               (loop
                 for column in row
                 and i = 1 then (+ i 1) 
                 do
                    (afl:force-speech)
                    (afl:with-surrounding-pause 1 
                      (read-aloud  column))
                    (afl:tts-force))))))))

(def-reading-rule (tabular simple)
    "Simple reading rule for arrays, uses directional audio"
  (with-pronunciation-mode (:mode :text) 
    (let
        ((contents
           (if *transpose-table*
               (transpose-table (contents tabular))
               (contents tabular ))))
      (loop
        for row in   contents 
        do
           (afl:new-block
               (loop
                 for column in row
                 and i = 1 then (+ i 1) 
                 do
                    (afl:pause 1) 
                    (afl:with-surrounding-pause 1 
                      (read-aloud  column))
                    (afl:force-speech)))))))



(def-reading-rule (tabular descriptive)
    (with-pronunciation-mode (:mode :text) 
      (let* 
          ((contents   (contents tabular ))
           (headers (first contents )))
        (loop
          for row in    (rest contents) 
          do
             (afl:new-block
               (loop
                   for column in row
                   and
                     head in headers
                     and i = 1 then (+ i 1) 
                   do
                      (afl:force-speech)
                      (read-aloud head)
                      (read-aloud "at" ) 
                      (afl:with-surrounding-pause 1 
                        (read-aloud  column))
                      (when (< i (length row )) 
                        (afl:local-set-state
                         (reading-state 'next-column )))
                      (afl:force-speech))
                 )))
      )
  )

