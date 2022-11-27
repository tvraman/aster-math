(in-package :aster)

(afl:tts-init)
  (unless afl:*current-speech-state*
    (afl:initialize-speech-space))
;; configure rules and  styles:
;;; active-styles is a stack:
;;; And earlier members take precedence
(setf *current-reading-style* '(descriptive use-special-pattern simple))


(activate-rule 'footnote 'float)
(activate-rule 'log 'read-base-first)

(setf *follow-cross-ref-wait* 0
      *get-label-wait* 0)
