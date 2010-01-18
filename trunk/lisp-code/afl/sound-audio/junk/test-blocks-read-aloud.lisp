;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
;;;
;;; Thu May  6 15:17:26 EDT 1993

;;; For some reason using user::read-aloud slows down things.

;;; testing blocks of cross products

(afl:new-block 
 (user::read-aloud "No sound is currently playing. ")
 (local-set-state (toggle-switch *current-audio-state* ))
 (user::read-aloud "Now a local set state has toggled the audio switch. ")
 (afl:new-block
  (local-set-state (toggle-switch *current-audio-state* ))
  (user::read-aloud "second block, toggled  the switch. ")
  (user::read-aloud "still in  second block. ")
  (local-set-state  (move-to *current-speech-state* 'head-size 110))
  (user::read-aloud "Now a local set state has changed the head size. ")
  (afl:new-block
   (user::read-aloud "Entered a third block. ")
   (local-set-state (switch-on *current-audio-state* ))
   (user::read-aloud "turned on the audio again. ")
   (local-set-state (select-sound
                     *current-audio-state* "item" ))
   (user::read-aloud "locally selected a
new sound. ")
   (afl:new-block
    (local-set-state (move-to *current-audio-state* 'interval
                              2))
    (user::read-aloud "this is another nested block. ")
    (user::read-aloud "Locally slowed down the sound. ")
    (user::read-aloud "about to exit the inner most block, so sound will
speed up again. "))
   (user::read-aloud "Exited innermost block, the sound has speeded up again.
")
   )
  (user::read-aloud "back in second block. "))
 (user::read-aloud "back in outer block. ")
 (global-set-state (toggle-switch *global-audio-state* ))
 (user::read-aloud "globally toggled the switch. ")
 )



(user::read-aloud "Outside all blocks, audio playing, since switch was turned
on globally. ")

(user::read-aloud "About to start another blocks example. ")
(afl:new-block
 (local-set-state (switch-on *current-audio-state* ))
 (local-set-state (get-point-in-speech-space 'betty))
 (user::read-aloud "this is the first block. ")
 (afl:new-block
  (local-set-state (select-sound "paragraph" *current-audio-state* ))
  (user::read-aloud "this is the second block. ")
  (afl:new-block
   (local-set-state (switch-off *current-audio-state* ))
   (user::read-aloud "Sound turned off in third block. "))
  (user::read-aloud "back in second block. ")
  (user::read-aloud "about to leave second block. "))
 (user::read-aloud "Back in first block. ")
 )

(user::read-aloud "Outside all blocks, about to turn off the switch in the
global state. ")
(global-set-state (switch-off *global-audio-state* ))

(afl:new-block
 (local-set-state (switch-on *current-audio-state* ))
 (local-set-state (get-point-in-speech-space 'betty))
 (user::read-aloud "this is the first block. ")
 (afl:new-block
  (local-set-state (select-sound "paragraph" *current-audio-state* ))
  (user::read-aloud "this is the second block. ")
  (afl:new-block
   (local-set-state (switch-off *current-audio-state* ))
   (user::read-aloud "Sound turned off in third block. ")
   (afl:new-block
    (local-set-state (switch-on *current-audio-state* ))
    (user::read-aloud "Entered a fourth block. ")
    (local-set-state (move-to
                      *current-audio-state* 'volume 15))
    (user::read-aloud "Increased the volume of the sound. ")
    (user::read-aloud "About to exit the fourth block. "))
   (user::read-aloud "About to exit the third block. ")
   )
  (user::read-aloud "back in second block. ")
  (user::read-aloud "about to leave second block. "))
 (user::read-aloud "Back in first block. ")
 )




