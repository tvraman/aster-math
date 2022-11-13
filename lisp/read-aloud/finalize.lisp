;; configure rules and  styles:
(activate-rule 'stackrel 'default)
(activate-rule 'overbrace 'default)
(activate-rule 'underbrace 'default)
(activate-style 'simple)
(activate-style 'descriptive)
(activate-rule 'log 'read-base-first)
(activate-rule 'induction 'default)
(activate-rule 'footnote 'float)
(activate-style  'use-special-pattern)
(setf *follow-cross-ref-wait* 0
      *get-label-wait* 0)
