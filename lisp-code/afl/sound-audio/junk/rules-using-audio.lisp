;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Thu Jan  7 15:11:12 EST 1993
;;; Contains some simple reading rules using audio player.
;;; Create an audio player in the before method for article, and kill
;;; it in the after method.


  ;;; Variable: *AUDIO-PLAYER*                                 Author: raman
  ;;; Created: Thu Jan  7 15:12:16 1993

(defvar *audio-player* nil "Audio player used to read")

(proclaim '(inline audio-player))
(defun audio-player () *audio-player* )

(defmethod read-aloud :before ((article article ))
           "Set up audio player for this article"
           (setf *audio-player*
                 (make-instance 'audio-player))
           )

(defmethod read-aloud :after ((article article))
           "Kill audio player"
           (remove-player  (audio-player) )
           )

;;; After seeing how fast they run introduce some error checking in
;;; above.

(defmethod read-aloud :before ((abstract abstract ))
           (afl:await-silence)
           (switch-on (audio-player) )
           )

(defmethod read-aloud :after ((abstract abstract ))
           (afl:await-silence)
           (switch-off (audio-player) )
           )
(defmethod read-aloud :around ((fraction fraction))
           (switch-on (audio-player))
           (call-next-method)
           (switch-off (audio-player))
           )
