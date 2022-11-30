(in-package :aster)
;; configure rules and  styles:
;;; active-styles is a stack:
;;; And earlier members take precedence
(setf *current-reading-style* '(descriptive use-special-pattern simple))


(activate-rule 'footnote 'float)
(activate-rule 'log 'read-base-first)
