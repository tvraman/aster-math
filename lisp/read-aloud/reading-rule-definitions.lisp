;;;   -*-   Mode: LISP -*-    ;;;
(in-package :aster)

;;; Modified: Mon Dec 21 09:39:21 EST 1992
;;; Reading rules that belong to different books being moved to their
;;; appropriate systems.
;;; Tue Dec  8 14:02:37 EST 1992
;;; Old version of this file is the directory  old-reading-rules.
;;; This file is now being modified and the reading rules are being
;;; converted to defmethod reading-rule.

;;;

;;; Wed Nov 11 15:13:25 EST 1992


;;; This file is being folded.
;;; Each fold will contain all the rules for a particular object
;;; The fold marker shows the object class and the rule names


;;{{{paragraph: interactive
(def-reading-rule (paragraph quiet)
  "do not read paragraph content. "
  )

(def-reading-rule (paragraph interactive)
  "Skip out at end of sentence if requested"
  (afl:new-block (afl:local-set-state :text)
    (with-slots  ((contents contents )) paragraph
      (loop for item in contents do
        (read-aloud  item)
        (when  (end-of-sentence? item)
          (afl:tts-force ))))))

(def-reading-rule (paragraph quick-interactive)
  "Skip out at end of sentence if requested"
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (afl:new-block (afl:local-set-state :text)
    (with-slots  ((contents contents )) paragraph
      (loop for item in contents do
        (read-aloud  item)
        (when  (end-of-sentence? item)
                                        ;(afl:should-i-stop? )
          ))))
  )

(def-reading-rule (paragraph non-interactive)
  " Non interactive: will not stop after each sentence. "
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (afl:new-block (afl:local-set-state :text)
    (with-slots  ((contents contents )) paragraph
      (loop for item in contents do
        (read-aloud  item ))))
  )

;;; Only read math:

(def-reading-rule (paragraph read-only-math)
  "Only read the math appearing in paragraphs"
  (let
      ((math-in-paragraph(remove nil
                                 (mapcar #'(lambda(object)
                                             (when (or (inline-math-p object)
                                                       (display-math-p
                                                        object ))
                                               object))
                                         (contents paragraph )))))
    (mapc #'read-aloud  math-in-paragraph)
    )
  )

(def-reading-rule (paragraph read-only-display-math)
  "Only read the math appearing in paragraphs"
  (let
      ((math-in-paragraph(remove nil
                                 (mapcar #'(lambda(object)
                                             (when
                                                 (or (math-equation-p object)
                                                     (display-math-p object))
                                               object))
                                         (contents paragraph )))))
    (mapc #'read-aloud  math-in-paragraph)
    )
  )

;;}}}

;;{{{verbatim: quiet verbose
(def-reading-rule (verbatim quiet)
  " quiet reading rule for object verbatim"
  )

(def-reading-rule (l-verbatim quiet)
  " quiet reading rule for object verbatim"
  )
(def-reading-rule (verbatim verbose)
  " verbose reading rule for object verbatim"
  (with-reading-state (reading-state 'verbatim-voice)
    (read-aloud(verbatim-contents verbatim )))
  )

;;}}}

;;{{{ fraction: fraction inference
(def-reading-rule (fraction fraction)
  " fraction reading rule for object fraction"
  (afl:new-block
    (afl:local-set-state
     (reading-state 'fraction)
     )
    (read-aloud "Fraction with numerator: ")
    (afl:new-block
      (afl:local-set-state
       (reading-state 'fraction-numerator))
      (read-aloud (numerator-of fraction))
      )
    (read-aloud "And denominator: ")
    (afl:new-block
      (afl:local-set-state
       (reading-state 'fraction-denominator))
      (read-aloud
       (denominator-of fraction))
      )
    (read-aloud  "end of fraction. ")
    )
  )
(def-reading-rule (fraction inference)
  " inference reading rule for object fraction"
  (read-aloud " from ")
  (read-aloud (numerator-of fraction))
  (read-aloud  " we infer ")
  (read-aloud (denominator-of fraction))
  )

;;}}}

;;{{{itemize: quiet default
(def-reading-rule (itemized-list quiet)
  " quiet reading rule for object itemized-list"
  nil)

(def-reading-rule (itemized-list default)
  (call-next-method))
;;}}}

;;{{{overbrace

(def-reading-rule (overbrace quiet)
  (read-attributes overbrace)
  )

;;}}}

;;{{{ slide
;;; read title only
;;}}}

(def-reading-rule (figure read-caption-only)
  "Only read the caption of a figure. "
  (read-aloud  "Figure captioned, ")
  (read-aloud (caption  figure ))
  (afl:tts-force))
