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
  (with-pronunciation-mode (:mode :math) 
    (let
        ((contents  (if *transpose-table*
                        (transpose-table (contents
                                          math-array))
                        (contents math-array ))))
      (loop for row in   contents 
            do
            (afl:new-block
                             (let* 
                                 ((range (afl:dimension-range
                                          'afl:right-volume ))
                                  (column-step (if  (>  (length row) 1)
                                                    (/ range (-
                                                              (length row)
                                                              1 ))
                                                    1))
                                  )
                               
                               (define-reading-state 'row
                                   #'(lambda (state)
                                       (afl:multi-move-to state
                                                          '(afl:left-volume 100)
                                                          '(afl:right-volume 0)
                                                          `(afl:left-volume
                                                            ,column-step
                                                            :slot afl:step-size)
                                                          `(afl:right-volume
                                                            ,column-step   :slot
                                                            afl:step-size ))))
                               (afl:local-set-state
                                (reading-state  'row))
                               (afl:with-surrounding-pause *math-surround*
                                 (loop for column in row
                                       and i = 1 then (+ i 1) 
                                       do
                                       (afl:force-speech)
                                       (afl:with-surrounding-pause 1 
                                         (read-aloud  column))
;                                       (dectalk:await-silence)
                                       (when (< i (length row ))
                                         (afl:local-set-state
                                          (reading-state 'next-column )))
                                       )
                                 )
                               ))))
    )
  )

(define-reading-state 'next-column
    #'(lambda(state)
        (afl:multi-step-by state
                           '(afl:left-volume -1)
                           '(afl:right-volume 1)))
  )



(def-reading-rule (tabular simple)
    "Simple reading rule for arrays, uses directional audio"
  (with-pronunciation-mode (:mode :text) 
    (let
        ((contents  (if *transpose-table*
                        (transpose-table (contents
                                          tabular))
                        (contents tabular ))))
      (loop for row in   contents 
            do
            (afl:new-block
                             (let* 
                                 ((range (afl:dimension-range
                                          'afl:right-volume ))
                                  (column-step (if  (>  (length row) 1)
                                                    (/ range (-
                                                              (length row)
                                                              1 ))
                                                    1))
                                  )
                               
                               (define-reading-state 'row
                                   #'(lambda (state)
                                       (afl:multi-move-to state
                                                          '(afl:left-volume 100)
                                                          '(afl:right-volume 0)
                                                          `(afl:left-volume
                                                            ,column-step
                                                            :slot afl:step-size)
                                                          `(afl:right-volume
                                                            ,column-step   :slot
                                                            afl:step-size ))))
                               (afl:local-set-state
                                (reading-state  'row))
                               (loop for column in row
                                     and i = 1 then (+ i 1) 
                                     do
                                     (afl:pause 1) 
                                     (afl:with-surrounding-pause 1 
                                       (read-aloud  column))
                                     (when (< i (length row ))
                                       (afl:local-set-state
                                        (reading-state 'next-column) ))
                                     (afl:force-speech))
                               ))))
    )
  )



(def-reading-rule (tabular descriptive)
    (with-pronunciation-mode (:mode :text) 
      (let* 
          ((contents   (contents tabular ))
           (headers (first contents )))
        (loop for row in    (rest contents) 
              do
              (afl:new-block
                               (let* 
                                   ((range (afl:dimension-range
                                            'afl:right-volume ))
                                    (column-step (if  (>  (length row) 1)
                                                      (/ range (-
                                                                (length row)
                                                                1 ))
                                                      1))
                                    )
                                 (define-reading-state 'row
                                     #'(lambda (state)
                                         (afl:multi-move-to state
                                                            '(afl:left-volume 100)
                                                            '(afl:right-volume 0)
                                                            `(afl:left-volume
                                                              ,column-step
                                                              :slot afl:step-size)
                                                            `(afl:right-volume
                                                              ,column-step   :slot
                                                              afl:step-size ))))
                                 (afl:local-set-state
                                  (reading-state  'row))
                                 (loop for column in row
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
                                 ))))
      )
  )

