;;; Implement AFL Using Aural CSS (ACSS) 

(in-package :acss)
;;; {Externals from original AFL:

(setq afl-external-functions
      '(afl:minimum-value afl:scale-by afl:define-standard-voice afl:interrogative afl:multi-move-to
 afl:generalized-afl-operator afl:switch afl:make-audio-filename afl:define-minimum-value afl:maximum-value
 afl:with-surrounding-pause afl:select-sound afl:length-of-subinterval afl:set-period-pause afl:play-repeatedly
 afl:multi-scale-by afl:get-point-in-speech-space afl:reset-dectalk afl:paragraph-begin afl:new-block
 afl:define-final-scale-factor afl:with-lazy-set-state dectalk:send-text afl:initialize-audio-space afl:multi-move-by
 dectalk:speak-file afl:switch-off afl:interval afl:get-final-scale-factor dectalk:speak-number-string
 afl:current-pronunciation-mode afl:secondary-stress afl:switch-on afl:primary-stress afl:local-set-state
 afl:get-pronunciation dectalk:should-i-continue? afl:pause afl:activate-sound-audio dectalk:should-i-stop?
 afl:low-intonation afl:define-default-value afl:set-comma-pause afl:comma-intonation afl:save-point-in-speech-space
 afl:define-maximum-value afl:exclamatory-stress afl:define-synthesizer-code afl:set-step-size afl:multi-step-by
 afl:initialize-total-space afl:exit-block afl:subclause-boundary dectalk:send-space afl:high-intonation
 dectalk:with-file-instead-of-dectalk afl:re-initialize-speech-space afl:current-value afl:toggle-switch
 afl:deactivate-sound-audio afl:add-dimension afl:high-low-intonation dectalk:with-file-as-well-as-dectalk
 afl:global-set-state afl:move-by afl:get-step-size afl:define-pronunciation afl:move-to afl:audio-prompt
 afl:synchronize-and-play dectalk:await-silence afl:exclamation afl:dimension-range afl:refresh
 afl:set-final-scale-factor dectalk:force-speech afl:step-by afl:period-intonation afl:define-unit-size afl:play-once
 afl:define-step-size afl:compute-range afl:with-pronunciation-mode afl:initialize-speech-space
 afl:list-of-speech-dimensions))

(setq afl-external-vars
      '(afl:*global-audio-state* afl:richness afl:*sound-priority* afl:ursula afl:*global-speech-state* afl:kid
 afl:*reader-comma-pause* afl:*pronounce-ignore-case-in-text* afl:voice afl:right-volume afl:quickness
 afl:*current-audio-state* afl:audio-move-to afl:*always-dehyphenate* afl:*global-total-audio-state* afl:sound-name
 afl:value afl:breathiness afl:harry afl:paul afl:smoothness afl:*current-total-audio-state* afl:pitch-range afl:wendy
 afl:id afl:*global-pronunciation-mode* afl:baseline-fall afl:frank afl:*lazy-set-state* afl:*pronunciation-mode*
 afl:*current-speech-state* dectalk:*stream* afl:speech-rate afl:port afl:rita afl:volume afl:loudness
 afl:average-pitch afl:stress-rise afl:dennis afl:left-volume afl:lax-breathiness
 afl:*await-silence-when-using-stereo* afl:named-block afl:betty afl:laryngilization afl:head-size afl:step-size
 afl:*reader-period-pause* afl:*sound-dir-name* afl:*default-voice* afl:hat-rise afl:assertiveness))

;;; }
;;; {End Of File:

;;; local variables:
;;; folded-file: t
;;; end:

;;; }
