;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))



;;; Examples to test afl:

(initialize-speech-space)

(changed-block
 (dectalk:speak-string "This is the first block, and at present I am
using default settings")
 (local-set! *current-speech-state*
             (move-by *current-speech-state*
                      'speech-rate 75))
 (dectalk:speak-string "Now a local set has speeded up  my speech")
 (changed-block
  (local-set! *current-speech-state*
              (get-point-in-speech-space 'betty))
  (dectalk:speak-string "Now I have entered a second block")
  (local-set! *current-speech-state*
              (step-by *current-speech-state*
                       'head-size
                       3))
  (dectalk:speak-string "Now another local set has moved the current
state along the head-size dimension by two steps")
  (dectalk:speak-string "this change is local to
the second block")
  (changed-block
   (dectalk:speak-string "entering a third block
where a local set will now change my average pitch")
   (local-set! *current-speech-state*
               (move-to *current-speech-state*
                        'average-pitch
                        90))
   (dectalk:speak-string "Now I have a lower
pitched voice")
   (dectalk:speak-string "I can also have
parameters set globally")
   (dectalk:speak-string "Now my pitch range
will be globally set to 250")
   (global-set! *current-speech-state*  'pitch-range 250)
   (dectalk:speak-string"I will speak like 
 I am excited  for the rest of this demo")
   (dectalk:speak-string "About to exit third block")
   )
  (dectalk:speak-string "back in second block, the average pitch has
been automatically reset")
  )
 (dectalk:speak-string "Back in outer block, the head size and voice
have  been automatically reset")
 )

;;; Move along a line in speech space:
#|
(changed-block
 (local-set! *current-speech-state*
             (multi-move-to *current-speech-state*
                            '( head-size 90)
                            '( average-pitch 80)
                            ))
 (dotimes  (i 9)
   (dectalk:speak-string "This is the next iteration, after moving
one step along a line in the speech space")
   (local-set! *current-speech-state*
               (multi-step-by *current-speech-state*
                              '(head-size 1)
                              '(average-pitch 1)
                              '(pitch-range 1)))
   ))

(changed-block
 (dectalk:speak-string "speaking on both channels")
 (dotimes (i 5)
   (local-set! *current-speech-state*
               (multi-step-by *current-speech-state*
                              '(left-volume -1)
                              '(right-volume 1)))
   (dectalk:speak-string "Moving from left to right")
   )
 (dotimes (i 10)
   (local-set! *current-speech-state*
               (multi-step-by *current-speech-state*
                              '(left-volume 1)
                              '(right-volume -1)))
   (dectalk:speak-string "Moving from right to left")
   )
 )
;;; *lazy-set-state* can be set locally:

(changed-block
 (dectalk:speak-string "This is the first block, and at present I am
using default settings")
 (local-set! *current-speech-state*
             (move-by *current-speech-state*
                      'speech-rate 75))
 (dectalk:speak-string "Now a local set has speeded up  my speech")
 (with-lazy-set-state
     (changed-block
      (local-set! *current-speech-state*
                  (move-to
                   (get-point-in-speech-space 'betty)
                   'speech-rate
                   (current-value 'speech-rate *current-speech-state*))
                  )
      (dectalk:speak-string "Now I have entered a second block")
      (local-set! *current-speech-state*
                  (step-by *current-speech-state*
                           'head-size
                           3))
      (dectalk:speak-string "Now another local set has moved the current
state along the head-size dimension by two steps")
      (dectalk:speak-string "this change is local to
the second block")
      (changed-block
       (dectalk:speak-string "entering a third block
where a local set will now change my average pitch")
       (local-set! *current-speech-state*
                   (move-to *current-speech-state*
                            'average-pitch
                            90))
       (dectalk:speak-string "Now I have a lower
pitched voice")
       (dectalk:speak-string "I can also have
parameters set globally")
       (dectalk:speak-string "Now my pitch range
will be globally set to 250")
       (global-set! *current-speech-state*  'pitch-range 250)
       (dectalk:speak-string"I will speak like 
 I am excited  for the rest of this demo")
       (dectalk:speak-string "About to exit third block")
       )
      (dectalk:speak-string "back in second block, the average pitch has
been automatically reset")
      ))
 (dectalk:speak-string "Back in outer block, the head size and voice
have  been automatically reset")
 )

(changed-block
 (dectalk:speak-string "Changing pitch inside a loop")
 (dotimes (i 5)
   (local-set! *current-speech-state*
               (step-by *current-speech-state*
                        'average-pitch
                        1))
   (dectalk:speak-string "How do I sound")
   ))
|#
;;; testing new-local-set-state

(changed-block
 (new-local-set-state 
             (multi-move-to *current-speech-state*
                            '( head-size 90)
                            '( average-pitch 80)
                            ))
 (dotimes  (i 9)
   (dectalk:speak-string "This is the next iteration, after moving
one step along a line in the speech space")
   (new-local-set-state 
               (multi-step-by *current-speech-state*
                              '(head-size 1)
                              '(average-pitch 1)
                              '(pitch-range 1)))
   ))

(changed-block
 (dectalk:speak-string "This is the first block, and at present I am
using default settings")
 (new-local-set-state 
             (move-by *current-speech-state*
                      'speech-rate 75))
 (dectalk:speak-string "Now a local set has speeded up  my speech")
 (changed-block
  (new-local-set-state 
              (move-to *current-speech-state*
                       'smoothness
                       50))
  (dectalk:speak-string "Now I have entered a second block")
  (new-local-set-state 
              (step-by *current-speech-state*
                       'head-size
                       3))
  (dectalk:speak-string "Now another local set has moved the current
state along the head-size dimension by two steps")
  (dectalk:speak-string "this change is local to
the second block")
  (changed-block
   (dectalk:speak-string "entering a third block
where a local set will now change my average pitch")
   (new-local-set-state 
               (move-to *current-speech-state*
                        'average-pitch
                        90))
   (dectalk:speak-string "Now I have a lower
pitched voice")
   (dectalk:speak-string "I can also have
parameters set globally")
   (dectalk:speak-string "Now my pitch range
will be globally set to 250")
   (global-set-state (move-to *global-speech-state* 'pitch-range 250))
   (dectalk:speak-string"I will speak like 
 I am excited  for the rest of this demo")
   (dectalk:speak-string "About to exit third block")
   )
  (dectalk:speak-string "back in second block, the average pitch has
been automatically reset")
  )
 (dectalk:speak-string "Back in outer block, the head size and voice
have  been automatically reset")
 )


(changed-block
 (dectalk:speak-string "This is the first block, and at present I am
using default settings")
 (new-local-set-state 
             (move-by *current-speech-state*
                      'speech-rate 75))
 (dectalk:speak-string "Now a local set has speeded up  my speech")
 (changed-block
  (new-local-set-state 
              (move-to *current-speech-state*
                       'smoothness
                       50))
  (dectalk:speak-string "Now I have entered a second block")
  (new-local-set-state 
              (step-by *current-speech-state*
                       'head-size
                       3))
  (dectalk:speak-string "Now another local set has moved the current
state along the head-size dimension by two steps")
  (dectalk:speak-string "this change is local to
the second block")
  (changed-block
   (dectalk:speak-string "entering a third block
where a local set will now change my average pitch")
   (new-local-set-state 
               (move-to *current-speech-state*
                        'average-pitch
                        90))
   (dectalk:speak-string "Now I have a lower
pitched voice")
   (dectalk:speak-string "I can also have
parameters set globally")
   (dectalk:speak-string "Now my pitch range
will be globally set to 250")
   (global-set-value 'pitch-range 250)
   (dectalk:speak-string"I will speak like 
 I am excited  for the rest of this demo")
   (dectalk:speak-string "About to exit third block")
   )
  (dectalk:speak-string "back in second block, the average pitch has
been automatically reset")
  )
 (dectalk:speak-string "Back in outer block, the head size and voice
have  been automatically reset")
 )
(changed-block
 (dectalk:speak-string "This is the first block, and at present I am
using default settings")
 (new-local-set-state 
             (move-by *current-speech-state*
                      'speech-rate 75))
 (dectalk:speak-string "Now a local set has speeded up  my speech")
 (changed-block
  (new-local-set-state 
              (move-to *current-speech-state*
                       'smoothness
                       50))
  (dectalk:speak-string "Now I have entered a second block")
  (new-local-set-state 
              (step-by *current-speech-state*
                       'head-size
                       3))
  (dectalk:speak-string "Now another local set has moved the current
state along the head-size dimension by two steps")
  (dectalk:speak-string "this change is local to
the second block")
  (changed-block
   (dectalk:speak-string "entering a third block
where a local set will now change my average pitch")
   (new-local-set-state 
               (move-to *current-speech-state*
                        'average-pitch
                        90))
   (dectalk:speak-string "Now I have a lower
pitched voice")
   (dectalk:speak-string "I can also have
parameters set globally")
   (dectalk:speak-string "Now my pitch range
will be globally set to 250")
   (global-set-state (move-to *global-speech-state* 'pitch-range 250))
   (dectalk:speak-string"I will speak like 
 I am excited  for the rest of this demo")
   (dectalk:speak-string "About to exit third block")
   )
  (dectalk:speak-string "back in second block, the average pitch has
been automatically reset")
  )
 (dectalk:speak-string "Back in outer block, the head size and voice
have  been automatically reset")
 )

