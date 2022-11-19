;; configure rules and  styles:

(in-package :aster)
(activate-style  'use-special-pattern)
(activate-style 'descriptive)
(activate-style 'simple)
(activate-rule 'footnote 'float)
(activate-rule 'log 'read-base-first)

;(activate-rule 'induction 'default)
;(activate-rule 'overbrace 'default)
;(activate-rule 'stackrel 'default)
;(activate-rule 'underbrace 'default)

(setf *follow-cross-ref-wait* 0
      *get-label-wait* 0)
