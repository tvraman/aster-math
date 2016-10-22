;;; Implement AFL Using Aural CSS (ACSS) 

(in-package :acss)
;;; {Externals from original AFL:

(defvar afl-external-functions
      '(
        send-space send-text speak-number-string
        minimum-value scale-by define-standard-voice interrogative multi-move-to
 generalized-afl-operator switch make-audio-filename define-minimum-value maximum-value
 with-surrounding-pause select-sound length-of-subinterval set-period-pause play-repeatedly
 multi-scale-by get-point-in-speech-space reset-dectalk paragraph-begin new-block
 define-final-scale-factor with-lazy-set-state  initialize-audio-space multi-move-by
  switch-off interval get-final-scale-factor 
 current-pronunciation-mode secondary-stress switch-on primary-stress local-set-state
 get-pronunciation  pause activate-sound-audio 
 low-intonation define-default-value set-comma-pause comma-intonation save-point-in-speech-space
 define-maximum-value exclamatory-stress define-synthesizer-code set-step-size multi-step-by
 initialize-total-space exit-block subclause-boundary  high-intonation
  re-initialize-speech-space current-value toggle-switch
 deactivate-sound-audio add-dimension high-low-intonation 
 global-set-state move-by get-step-size define-pronunciation move-to audio-prompt
 synchronize-and-play  exclamation dimension-range refresh
 set-final-scale-factor  step-by period-intonation define-unit-size play-once
 define-step-size compute-range with-pronunciation-mode initialize-speech-space
 list-of-speech-dimensions))

(defvar afl-external-vars
      '(*global-audio-state* richness *sound-priority* ursula *global-speech-state* kid
 *reader-comma-pause* *pronounce-ignore-case-in-text* voice right-volume quickness
 *current-audio-state* audio-move-to *always-dehyphenate* *global-total-audio-state* sound-name
 value breathiness harry paul smoothness *current-total-audio-state* pitch-range wendy
 id *global-pronunciation-mode* baseline-fall frank *lazy-set-state* *pronunciation-mode*
 *current-speech-state*  speech-rate port rita volume loudness
 average-pitch stress-rise dennis left-volume lax-breathiness
 *await-silence-when-using-stereo* named-block betty laryngilization head-size step-size
 *reader-period-pause* *sound-dir-name* *default-voice* hat-rise assertiveness))

;;; }
;;; {End Of File:

;;; local variables:
;;; folded-file: t
;;; end:

;;; }
