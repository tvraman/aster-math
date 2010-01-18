;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)


;;; Modified: Mon Dec 21 09:39:21 EST 1992
;;; Reading rules that belong to different books being moved to their
;;; appropriate systems. 
;;; Tue Dec  8 14:02:37 EST 1992
;;; Old version of this file is the directory  old-reading-rules.
;;; This file is now being modified and the reading rules are being
;;; converted to defmethod reading-rule.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

;;; Wed Nov 11 15:13:25 EST 1992

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file is being folded.
;;; Each fold will contain all the rules for a particular object
;;; The fold marker shows the object class and the rule names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; {paragraph: interactive
(def-reading-rule (paragraph quiet)
    "do not read paragraph content. "
  )

(def-reading-rule (paragraph interactive)
    "Skip out at end of sentence if requested"
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (afl:new-block (afl:local-set-state :text)
                 (with-slots  ((contents contents )) paragraph
                              (afl:paragraph-begin)
                              (afl:synchronize-and-play *paragraph-cue* :background-flag t)
                              (loop for item in contents do
                                    (read-aloud  item)
                                    (when  (end-of-sentence? item)
                                      (afl:await-silence )))))
  )

(defvar *mark-interactively-char* #\m
  "Char to be read when marking interactively. ")

(def-reading-rule (paragraph allow-interactive-marks) 
    "Skip out at end of sentence if requested.
In addition, allow the user to mark a position while reading. "
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (afl:new-block (afl:local-set-state :text)
                 (with-slots  ((contents contents )) paragraph
                              (afl:paragraph-begin)
                              (afl:synchronize-and-play *paragraph-cue* :background-flag t)
                              (loop for item in contents do
                                    (read-aloud  item)
                                    (when  (end-of-sentence? item)
                                      (afl:await-silence )
                                      (when  (char=
                                              (or (read-char-no-hang)
                                              #\space)
                                              *mark-interactively-char*)
                                        (mark-read-pointer))
                                      ))))
  )


(defvar *random-step-limit*  2.0 "Limit on random stepping. ")
(defvar *random-speech-interval*(1-  (length
                                  (afl:list-of-speech-dimensions)))
  "Number of speech dimensions. ")
(def-reading-rule (paragraph random-change) 
    "Introduce a random voice quality change after each sentence. "
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (flet (
         (random-dimension()
           (elt
            (afl:list-of-speech-dimensions)
            (1+ (random
                 *random-speech-interval* )))))
    (afl:new-block (afl:local-set-state :text)
                   (with-slots  ((contents contents )) paragraph
                                (afl:paragraph-begin)
                                (afl:synchronize-and-play *paragraph-cue* :background-flag t)
                                (loop for item in contents do
                                      (read-aloud  item)
                                      (when  (end-of-sentence? item)
                                        (afl:await-silence)
                                        (afl:local-set-state
                                         (afl:step-by
                                          afl:*current-speech-state*
                                          (elt
                                           (afl:list-of-speech-dimensions)
                                           (1+ (random
                                                *random-speech-interval* )))
                                          (random *random-step-limit*  )))
                                        )))))
  )
(def-reading-rule (paragraph random-stereo) 
    "Introduce a random stereo  change after each sentence. "
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (afl:new-block (afl:local-set-state :text)
                 (with-slots  ((contents contents )) paragraph
                              (afl:paragraph-begin)
                              (afl:synchronize-and-play *paragraph-cue* :background-flag t)
                              (loop for item in contents do
                                    (read-aloud  item)
                                    (when  (end-of-sentence? item)
                                      (afl:await-silence)
                                      (afl:local-set-state
                                       (afl:step-by
                                        afl:*current-speech-state*
                                        (if (zerop  (random 2))
                                            'afl:left-volume
                                            'afl:right-volume)
                                        (- (random 5.0) 2.5 ))
                                       ))))))









(def-reading-rule (paragraph quick-interactive)
    "Skip out at end of sentence if requested"
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (afl:new-block (afl:local-set-state :text)
                 (with-slots  ((contents contents )) paragraph
                              (afl:paragraph-begin)
                              (afl:synchronize-and-play *paragraph-cue* :background-flag t)
                              (loop for item in contents do
                                    (read-aloud  item)
                                    (when  (end-of-sentence? item)
                                      (afl:should-i-stop? )))))
  )

(def-reading-rule (paragraph non-interactive)
    " Non interactive: will not stop after each sentence. " 
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (afl:new-block (afl:local-set-state :text)
                 (with-slots  ((contents contents )) paragraph
                              (afl:paragraph-begin)
                              (afl:synchronize-and-play *paragraph-cue* :background-flag t)
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


(def-reading-rule (paragraph summary)
    "read first sentence"
  (let ((contents (contents paragraph )))
    (afl:new-block
     (afl:synchronize-and-play *paragraph-cue*)
     (loop for word in contents
           do
           (read-aloud word)
           (when (end-of-sentence? word)
             (return nil)
             (afl:force-speech )))))
  )

;;; }

;;; {verbatim: quiet verbose
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

;;; }

;;; { fraction: fraction inference
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
  (afl:synchronize-and-play *item-cue* :background-flag t)
  (read-aloud  " we infer ")
  (read-aloud (denominator-of fraction))
  )



;;; }

;;; {itemize: quiet default
(def-reading-rule (itemized-list quiet)
    " quiet reading rule for object itemized-list"
  nil)

(def-reading-rule (itemized-list default) 
    (call-next-method))
;;; }

;;; {overbrace

(def-reading-rule (overbrace quiet)
    (read-attributes overbrace)
  )

;;; }

;;; { slide
;;; read title only
(def-reading-rule (slide read-only-title)
    "only read titles of slides. "
  (flet
      ((extract-slide-title(paragraph)
         "extract slide title if present. "
         (when (paragraph-p paragraph)
           (find-if
            #'(lambda(object)  (typep object 'slide-title )) (contents paragraph)))
         ))
    (let
        ((title(mapcar #'extract-slide-title (contents slide ))))
      (read-aloud title )))
  )
;;; }


(def-reading-rule (figure read-caption-only)
    "Only read the caption of a figure. "
  (read-aloud  "Figure captioned, ")
  (read-aloud (caption  figure ))
  (afl:force-speech))
