;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Fri Feb 12 10:10:14 EST 1993
;;; testing blocks of cross products

(new-block 
 (send-text "No sound is currently playing. ")
 (local-set-state (toggle-switch *current-audio-state* ))
 (send-text "Now a local set state has toggled the audio switch. ")
 (new-block
  (local-set-state (toggle-switch *current-audio-state* ))
  (send-text "second block, toggled  the switch. ")
  (send-text "still in  second block. ")
  (local-set-state  (move-to *current-speech-state* 'head-size 110))
  (send-text "Now a local set state has changed the head size. ")
  (new-block
   (send-text "Entered a third block. ")
   (local-set-state (switch-on *current-audio-state* ))
   (send-text "turned on the audio again. ")
   (local-set-state (select-sound
                     *current-audio-state* "item" ))
   (send-text "locally selected a
new sound. ")
   (new-block
    (local-set-state (move-to *current-audio-state* 'interval
                              2))
    (send-text "this is another nested block. ")
    (send-text "Locally slowed down the sound. ")
    (send-text "about to exit the inner most block, so sound will
speed up again. "))
   (send-text "Exited innermost block, the sound has speeded up again.
")
   )
  (send-text "back in second block. "))
 (send-text "back in outer block. ")
 (global-set-state (toggle-switch *global-audio-state* ))
 (send-text "globally toggled the switch. ")
 )



(send-text "Outside all blocks, audio playing, since switch was turned
on globally. ")

(send-text "About to start another blocks example. ")
(new-block
 (local-set-state (switch-on *current-audio-state* ))
 (local-set-state (get-point-in-speech-space 'betty))
 (send-text "this is the first block. ")
 (new-block
  (local-set-state (select-sound "paragraph" *current-audio-state* ))
  (send-text "this is the second block. ")
  (new-block
   (local-set-state (switch-off *current-audio-state* ))
   (send-text "Sound turned off in third block. "))
  (send-text "back in second block. ")
  (send-text "about to leave second block. "))
 (send-text "Back in first block. ")
 )

(send-text "Outside all blocks, about to turn off the switch in the
global state. ")
(global-set-state (switch-off *global-audio-state* ))

(new-block
 (local-set-state (switch-on *current-audio-state* ))
 (local-set-state (get-point-in-speech-space 'betty))
 (send-text "this is the first block. ")
 (new-block
  (local-set-state (select-sound "paragraph" *current-audio-state* ))
  (send-text "this is the second block. ")
  (new-block
   (local-set-state (switch-off *current-audio-state* ))
   (send-text "Sound turned off in third block. ")
   (new-block
    (local-set-state (switch-on *current-audio-state* ))
    (send-text "Entered a fourth block. ")
    (local-set-state (move-to
                      *current-audio-state* 'volume 15))
    (send-text "Increased the volume of the sound. ")
    (send-text "About to exit the fourth block. "))
   (send-text "About to exit the third block. ")
   )
  (send-text "back in second block. ")
  (send-text "about to leave second block. "))
 (send-text "Back in first block. ")
 )




