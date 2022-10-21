;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq EXT:APPEASE-CERRORS t)
(in-package :user)



;;; Fri Feb 19 09:38:37 EST 1993
;;; Someone's making dectalk use the :user package!
;;; Until I can find the offending code and delete it, (probably comes
;;; from the time when I did something without fully understanding the
;;; package system) just undoing the use here.
(unuse-package :user (find-package :dectalk))
;;; Thu Dec 24 14:46:45 EST 1992
;;; All that I need to execute after loading everything.
;;; Placed here to avoid typing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(afl:deactivate-sound-audio)
(mapc #'load-system *books*)
#+lucid 
(load
 (concatenate 'string
              *lisp-code-directory* "/"
              "read-aloud/temporary-tex-objects"))
#+clisp
(load
 (concatenate 'string
              *lisp-code-directory* "/"
              "read-aloud/temporary-tex-objects.lisp"))
(load-system :browse )
(when *announce* 
(afl:set-final-scale-factor 'afl:speech-rate 1.2)
(afl:set-final-scale-factor 'afl:right-volume .8))
(activate-rule 'paragraph 'interactive )
(activate-rule 'stackrel 'default)
(activate-rule 'overbrace 'default)
(activate-rule 'underbrace 'default)
(activate-style 'simple)
(activate-style 'descriptive)
(activate-rule 'log 'read-base-first)
(activate-rule 'aster 'dont-bark)
(setf *follow-cross-ref-wait* 0)
(activate-rule 'induction 'default)
(activate-rule 'footnote 'float)
(activate-style  'use-special-pattern)

 (setf *print-document-object-ignore-slots*
      (list 'next 'previous 'parent 'afl-state))
(setf *follow-cross-ref-wait* 0
*get-label-wait* 0)
#+lucid (precompile-generic-functions)
 

