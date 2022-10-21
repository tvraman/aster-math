;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Created: Sat Apr 11 19:20:05 EDT 1992
;;; Contains  read-aloud methods  and associated code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modified: Thu Aug 20 08:49:51 EDT 1992
;;;  Using AFL to express rendering rules.

;;; variables defining what sound cues to use:

;;; Variable: *ARTICLE-CUE*                                  Author: raman
;;; Created: Sun May  3 19:04:31 1992

(defvar *article-cue*
  "article"
  "Cue to use at the start and end of articles.")

;;; Variable: *ARTICLE-OPEN-CUE*                             Author: raman
;;; Created: Tue May 12 13:49:55 1992

(defvar *article-open-cue*
  "article-open"
  "open an article")

;;; Variable: *ARTICLE-CLOSE-CUE*                            Author: raman
;;; Created: Tue May 12 13:50:26 1992

(defvar *article-close-cue*
  "article-close"
  "close an article.")

;;; Variable: *ABSTRACT-CUE*                                 Author: raman
;;; Created: Thu May  7 13:00:16 1992

(defvar *abstract-cue*

  "abstract"
  "cue for abstract")

;;; Variable: *PARAGRAPH-CUE*                                Author: raman
;;; Created: Sun May  3 19:06:02 1992

(defvar *paragraph-cue*
  "paragraph"
  "cue to use at the start of a new paragraph.")

;;; Variable: *SECTION-CUE*                                  Author: raman
;;; Created: Sun May  3 19:06:33 1992

(defvar *section-cue*
  "section"
  "cue at the beginning of sections.")

;;; Variable: *ITEM-CUE*                                     Author: raman
;;; Created: Sun May  3 19:11:11 1992

(defvar *item-cue*
  "item"
  "Cue for items.")

;;; Variable: *NEWLINE-CUE*                                  Author: raman
;;; Created: Sun May  3 19:07:03 1992

(defvar *newline-cue*
  "newline"
  "Cue for newlines.")

  ;;; Parameter: *FIELD-SEPARATOR-CUE*                         Author: raman
  ;;; Created: Tue Sep 28 14:27:01 1993

(defparameter *field-separator-cue*
  "quiet-beep"
  "sound cue for field separator. ")

;;; Variable: *SLIDE-CUE*                                    Author: raman
;;; Created: Mon May  4 12:41:54 1992

(defvar *slide-cue*
  "slide"
  "cue for slides.")

;;; end of cues section.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generic: READ-ALOUD                                      Author: raman
;;; Created: Mon Apr 13 11:01:28 1992

(defgeneric read-aloud (object)
  #+CLOS (:documentation "read aloud an abject on the multivoice.")
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Sat Apr 11 21:10:50 1992

(defmethod read-aloud ( (token t))
  "default method to read aloud "
  (when token
    (afl:send-text token))
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Sat Apr 11 20:58:00 1992

(defmethod read-aloud ((document document))
  "read aloud a document, default method."
  (read-aloud (format nil   "Do not yet know how to read  ~a"
                      (type-of document)))
  (afl:force-speech)
  )

;;; Method: READ-ALOUD                                          Author: raman
;;; Created: Sat Apr 11 19:24:02 1992

(defmethod read-aloud ((abstract abstract))
  "read out abstract"
  (with-reading-state (reading-state 'annotation-voice)
    (read-aloud  " Abstract,   ")
    (afl:force-speech)
    )
  (tts:icon *abstract-cue*  )
  (afl:new-block
    (afl:send-text "[+]")
    (afl:local-set-state
     (reading-state 'abstract))
    (read-aloud
     (abstract-contents abstract))
    )
  )

;;; Variable: *PLAY-SIGNATURE-TUNE*                          Author: raman
;;; Created: Wed Apr 29 21:45:41 1992

(defvar *play-signature-tune* nil
  "If t play a tune before and after reading document.")

(defmethod read-aloud  :before ((article article ))
  "Activate audio player before beginning to read. "
  (afl:initialize-total-space)
  (reset-footnote-counter)
  (setf  (internal-time-to-read article)
         (get-universal-time))
  )

(defmethod read-aloud :after ((article article ))
  "Deactivate sound audio after reading. "
  (force-all-floats)
  (setf (internal-time-to-read article)
        (- (get-universal-time)
           (internal-time-to-read article ))))
;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon Apr 13 11:35:06 1992

(defmethod read-aloud ((article article))
  "read aloud an article"
  (afl:new-block
    (afl:initialize-speech-space)
    (when *play-signature-tune*(tts:icon *article-open-cue*))
    (when (article-title article)
      (with-reading-state (reading-state 'annotation-voice)
        (read-aloud   "Title. ")
        )
      (with-reading-state (reading-state 'title-voice)
        (read-aloud (article-title article ))))
    (when (article-author article)
      (with-reading-state (reading-state 'annotation-voice)
        (read-aloud  " By, "))
      (with-reading-state (reading-state 'title-voice)
        (read-aloud (article-author article ))))
    (when (article-date article)
      (with-reading-state (reading-state 'annotation-voice)
        (read-aloud "  Date, "))
      (with-reading-state (reading-state 'title-voice)
        (read-aloud (article-date article ))))
    (read-aloud (article-abstract article))
    (read-aloud (article-initial-body article))
    (read-aloud  (article-sectional-units article))
    (when *play-signature-tune*(tts:icon *article-close-cue*))
    (afl:force-speech) )
  )

;;; { punctuations

;;; Variable: *READ-ALL-TEXT*                                Author: raman
;;; Created: Wed Apr 29 08:40:46 1992

(defvar *read-all-text* nil  "If true, read all the text in a document.")

(setf *read-all-text* t )
;;; Variable: *PUNCTUATIONS*                                 Author: raman
;;; Created: Sat Apr 11 19:55:50 1992

(defvar *punctuations*
  (list
   "."
   ","
   "!"
   "?"
   ":"
   ";"
   "("
   ")"
   )
  "list of known punctuation characters.")

;;; Function: PUNCTUATION?                                   Author: raman
;;; Created: Sat Apr 11 19:54:58 1992

(defmethod  punctuation? ((punct string))
  "check if argument is a punctuation character"
  (find punct *punctuations*
        :test #'string=)
  )

  ;;; Method: PUNCTUATION?                                     Author: raman
  ;;; Created: Thu Dec 31 11:49:15 1992

(defmethod punctuation? ((word word))
  "Check for punctuation"
  (punctuation? (contents word ))
  )

;;; Variable: *END-OF-SENTENCE-MARKER*                       Author: raman
;;; Created: Mon Apr 13 11:16:00 1992

(defvar *end-of-sentence-marker*
  (list
   "."
   "?"
   "!"
   )
  "end of sentence markers")

(defmethod end-of-sentence? ((object t)) nil)

(defmethod punctuation? ((object t)) nil)
;;; Method: END-OF-SENTENCE?                               Author: raman
;;; Created: Mon Apr 13 11:16:46 1992

(defmethod  end-of-sentence? ((text string))
  "check if texts marks the  end of a sentence. "
  (find text *end-of-sentence-marker*
        :test #'string=)
  )

  ;;; Method: END-OF-SENTENCE?                                 Author: raman
  ;;; Created: Thu Dec 31 11:48:20 1992

(defmethod end-of-sentence? ((word word))
  "Check for end of sentence"
  (end-of-sentence? (contents word))
  )
;;; }

;;; Method: READ-ALOUD                                          Author: raman
;;; Created: Sat Apr 11 20:38:53 1992
;;; Modified: Sat Dec 26 08:18:44 EST 1992
;;; Updating after introducing word class.
;;; <(old version used should-i-continue, has been backed up. )>
;;; The same functionality now provided by the interactive reading
;;; rule for paragraphs  which can be activated if some primitive stop and
;;; skip desired.
;;; Activating  rules on builtin classes will not work,
;;; They are not subclasses of class document

(defmethod read-aloud ((list list ))
  "read out contents of a list"
  (mapc #'read-aloud list )
  )

  ;;; Method: READ-ALOUD                                       Author: raman
  ;;; Created: Sat Dec 26 07:21:06 1992
;;; Modified: Sat Dec 26 08:49:36 EST 1992
(defmethod read-aloud ((word word))
  "Read aloud a word. "
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (with-slots  ((contents contents )) word
    (cond
      ((punctuation? contents)
       (tts:queue (format nil "~a" (afl:get-pronunciation contents ))))
      (t (afl:send-space)
         (afl:send-text (afl:get-pronunciation contents )))
      ))
  )

;;; Method: READ-ALOUD                                          Author: raman
;;; Created: Sat Apr 11 20:43:20 1992

(defmethod read-aloud ((string string))
  "read out a string"
  (declare (optimize (compilation-speed 0) (safety  0) (speed 3)))
  (let
      ((pronounce (afl:get-pronunciation string )))
    (cond
      ((punctuation? string)
       (tts:queue (format nil "[_]~a" pronounce))
       (afl:force-speech))
      (t (afl:send-space)
         (afl:send-text pronounce  ))
      )
    )
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon Apr 13 11:41:28 1992

(defmethod read-aloud ((text-block text-block))
  "read aloud a text block,. "
  (afl:new-block
    (dolist
        (item (text-block-local-environment text-block) )
      (afl:local-set-state
       (funcall (retrieve-font-rule item) afl:*current-speech-state*)))
    (when (eql :math (afl:current-pronunciation-mode))
      (afl:local-set-state :text ))
    (read-aloud (text-block-contents text-block))
    ))

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon Apr 13 17:19:50 1992

(defmethod read-aloud ((paragraph paragraph))
  "read aloud a paragraph."
  (tts:icon *paragraph-cue*)
  (afl:send-text "[+] ")
  (read-aloud (paragraph-contents paragraph))
  )

;;; The following are read-aloud methods for some parent classes.
;;; Done as a quick way of getting the document spoken.
;;; Will be shadowed once more specific methods are implemented.

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon Apr 13 19:53:25 1992

(defmethod read-aloud ((list-environment list-environment))
  "read aloud a list environment."
  (afl:new-block
    (afl:local-set-state
     (reading-state 'list-environment-voice))
    (afl:paragraph-begin)
    (read-aloud (list-environment-items list-environment))
    )
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon Apr 13 19:59:49 1992

(defmethod read-aloud ((item item))
  "read aloud an item."
  (tts:icon *item-cue*)
  (when (item-marker item)
    (with-reading-state (reading-state 'annotation-voice)
      (read-aloud (item-marker item ))))
  (read-aloud (item-contents item))
  (afl:pause 5)
  (when (label item)
    (read-aloud (label-name (label item ))))
  (relabel-if-necessary (label item ))
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Fri May  1 09:40:21 1992

(defmethod read-aloud ((centered-text centered-text))
  "read aloud centered text."
  (afl:new-block
    (afl:local-set-state
     (reading-state 'center))
    (read-aloud (centered-text-contents centered-text )))
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon Apr 13 20:04:26 1992

(defmethod read-aloud ((quoted-text quoted-text))
  "read quoted text"
  (afl:new-block
    (afl:local-set-state
     (reading-state 'quotation-voice)
     )
    (read-aloud (quoted-text-contents quoted-text))
    )
  )

  ;;; Method: READ-ALOUD                                       Author: raman
  ;;; Created: Tue Dec 22 14:29:34 1992

(defmethod read-aloud ((text-number text-number))
  "Read aloud a number "
  (afl:speak-number-string (first  (contents text-number )))
  )
;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon Apr 13 20:05:25 1992

(defmethod read-aloud ((sectional-unit sectional-unit))
  "read sectional-unit"
  (with-reading-state (reading-state 'annotation-voice)
    (read-aloud
     (sectional-unit-name sectional-unit))
    (when (sectional-unit-number sectional-unit)
      (afl:speak-number-string
       (sectional-unit-number sectional-unit )))
    )
  (with-reading-state (reading-state 'title-voice)
    (read-aloud (sectional-unit-title sectional-unit )))
  (when (sectional-unit-body sectional-unit)
    (afl:new-block
      (read-aloud  (sectional-unit-body sectional-unit
                                        )))
    (afl:await-silence))
  (afl:new-block
    (read-aloud (sectional-unit-sectional-units sectional-unit )))
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Tue Apr 28 17:32:37 1992

(defmethod read-aloud ((section section))
  "read aloud a section"
  (with-reading-state (reading-state 'annotation-voice)
    (read-aloud
     (sectional-unit-name section))
    (when (sectional-unit-number section)
      (afl:speak-number-string
       (sectional-unit-number section)))
    )
  (tts:icon *section-cue* )
  (with-reading-state (reading-state 'title-voice)
    (read-aloud (sectional-unit-title section)))
  (when (sectional-unit-body section)
    (afl:new-block
      (read-aloud (sectional-unit-body section )))
    (afl:await-silence ))
  (afl:new-block
    (read-aloud (sectional-unit-sectional-units section )))
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Thu Apr 16 19:14:59 1992

(defmethod read-aloud ((comment comment))
  "read out comments in electronic source."
  ;; Do nothing.
  )

  ;;; Function: RELABEL-IF-NECESSARY                           Author: raman
  ;;; Created: Sat May  1 13:51:37 1993
(proclaim '(inline relabel-if-necessary))
(defun relabel-if-necessary (label)
  "Relabel this label object prompting user. "
  (when (typep label 'label)
    (let ((new-label (get-label-from-user )))
      (when new-label
        (setf (label-name label )new-label))
      (values)
      ))
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Thu Apr 16 19:29:26 1992

(defmethod read-aloud ((new-environment new-environment))
  "Read aloud a new environment"
  (if (label new-environment)
      (read-aloud (label-name (label new-environment )))
      (read-aloud (format nil "~a ~a. "
                          (if (eql 'new-environment
                                   (class-name (class-of
                                                new-environment )))
                              (new-environment-name new-environment )
                              (class-name (class-of new-environment )))
                          (number new-environment ))))
  (afl:pause 5)
  (read-aloud (new-environment-contents new-environment))
  (relabel-if-necessary (label new-environment ))
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Tue Apr 28 16:47:59 1992

(defmethod read-aloud ((tex-defined-macro tex-defined-macro))
  "read aloud a tex macro"
  (if    (tex-defined-macro-read-as tex-defined-macro)
         (read-aloud (tex-defined-macro-read-as tex-defined-macro))
         (read-aloud (tex-defined-macro-name tex-defined-macro))
         )
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Wed Apr 29 15:26:32 1992

(defmethod read-aloud ((citation citation))
  "read out citation."
  (afl:send-text "[_]" )
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Wed Apr 29 19:54:55 1992

(defmethod read-aloud ((footnote footnote))
  "read aloud a footnote."
  (afl:new-block
    (with-reading-state (reading-state 'footnote)
      (read-aloud (footnote-text footnote))))
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon Oct  5 11:55:28 1992

(defmethod read-aloud ((index-term index-term))
  "Read aloud method for index terms. "
  (afl:send-text  "[_]")
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Sun May  3 17:23:25 1992

(defmethod read-aloud ((newline (eql  'newline)))
  "read aloud new lines"
  (afl:force-speech)
  (tts:icon *newline-cue* )
  )

(defmethod read-aloud ((field-separator (eql  'field-separator)))
  "read aloud new lines"
  (tts:icon *field-separator-cue* )
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon May  4 12:34:18 1992

(defmethod read-aloud ((slide slide))
  "read aloud a slide"
  (tts:icon *slide-cue* )
  (read-aloud (slide-contents slide)))

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Fri Aug 28 15:04:12 1992

(defmethod read-aloud ((verbatim verbatim))
  "read aloud contents of verbatim"
  (afl:new-block
    (afl:local-set-state
     (reading-state 'verbatim-voice))
    (read-aloud (verbatim-contents verbatim))
    )
  )

;;; Function: OUTLINE                                        Author: raman
;;; Created: Mon May 11 10:53:37 1992
;;; No longer used.
(defun outline (art)
  "outline of art"
  (setf *read-all-text* nil)
  (read-aloud art)
  (toggle *read-all-text*)
  )

;;; Parameter: *MATH-CUE*                                    Author: raman
;;; Created: Mon Sep  7 13:24:46 1992

(defparameter *math-cue*
  "jazz_piano_beep"
  "cue math")

;;; Variable: *READ-MATH-ALOUD*                              Author: raman
;;; Created: Mon Sep 21 09:15:09 1992

(defvar *read-math-aloud* t "If t read math aloud")

;;; Variable: *SPEAK-MATH-IMMEDIATELY*                       Author: raman
;;; Created: Tue Sep 29 20:27:39 1992
(defvar *speak-math-immediately* nil
  "If t then dectalk speaks math immediately. Set this to nil when
reading full documents. ")

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon Sep  7 13:23:37 1992

(defmethod read-aloud ((display-math display-math))
  "default method for reading display-math: "
  (afl:with-pronunciation-mode (:mode :math)
    (afl:force-speech)
    (afl:with-surrounding-pause (* 3 *math-surround*)
      (afl:new-block
        (afl:local-set-state
         (reading-state 'math))
        (read-aloud ( display-math-contents display-math))
        ))
    (afl:force-speech)
    (afl:should-i-continue?))
  )

;;; Parameter: *MATH-SURROUND*                               Author: raman
;;; Created: Thu Nov 26 12:30:20 1992

(defparameter *math-surround* 50
  "Amount of pause around inline math in milliseconds. ")

;;; Fri Oct 30 11:12:01 EST 1992

(defmethod read-aloud ((inline-math inline-math))
  "default method for reading inline-math: "
  (let*
      ((object-weight (weight (inline-math-contents inline-math )))
       (pause-amount  (if  (> object-weight 1)
                           *math-surround*
                           0)))
    (afl:with-pronunciation-mode (:mode :math)
      (afl:with-surrounding-pause  pause-amount
        (afl:new-block
          (afl:local-set-state
           (reading-state 'math))
          (read-aloud ( inline-math-contents inline-math))
          ))
      )
    )
  )

;;; Variable: *ATTRIBUTES-READING-ORDER*                     Author: raman
;;; Created: Mon Oct 26 15:28:31 1992

(defvar *attributes-reading-order*
  '( subscript superscript underbar accent left-subscript
    left-superscript )
  " Sorted list of attributes")

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon Sep 21 08:47:02 1992

(defmethod read-aloud ((math-object math-object))
  "read aloud math object"
  (read-aloud (contents math-object))
  (afl:send-space)
  (when (attributes math-object)
    (mapcar #'read-aloud
            (sorted-attributes  (attributes math-object)))
    )
  (afl:send-space)
  (with-reading-state (reading-state 'children)
    (read-aloud (children math-object)))
  )

;;; Function: SORTED-ATTRIBUTES                              Author: raman
;;; Created: Mon Oct 26 15:31:17 1992

(proclaim '(inline sorted-attributes))
(defun sorted-attributes (attributes)
  "Return attributes sorted in order to be read"
  (loop for attribute in *attributes-reading-order*
        collect (find attribute
                      attributes
                      :key #'attribute-name))
  )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Mon Sep 21 08:49:38 1992

(defmethod read-aloud ((attribute attribute))
  "read aloud attribute"
  (afl:new-block
    (afl:local-set-state
     (reading-state (attribute-name attribute)))
    (read-aloud (attribute-value attribute))
    (afl:send-space)
    )
  )

;;; Function: TRANSPOSE-TABLE                                Author: raman
;;; Created: Wed Dec  2 11:17:14 1992

(defun transpose-table (table)
  "Transpose table assume nested lists of equal length"
  (when table
    (apply #'mapcar #'list table))
  )

;;; Variable: *TRANSPOSE-TABLE*                              Author: raman
;;; Created: Wed Dec  2 11:48:09 1992

(defvar *transpose-table* nil "If t read transposed tables")

;;; Variable: *column-CUE*                                      Author: raman
;;; Created: Sat Oct 31 11:16:32 1992

(defvar *column-cue*
  "column"
  "Column cue")

;;; Variable: *ROW-CUE*                                      Author: raman
;;; Created: Sat Oct 31 11:17:10 1992

(defvar *row-cue*
  "long-beep"
  "Row cue")
;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Fri Oct 30 12:00:29 1992

(defmethod read-aloud ((math-array math-array))
  "Read math array, not fully implemented"
  (afl:with-pronunciation-mode (:mode :math) 
    (let
        ((contents  (if *transpose-table*
                        (transpose-table (contents
                                          math-array))
                        (contents math-array ))))
      (loop for row in   contents 
            do
            (loop for column in row
                  and
                  col-index = 1 then (+ 1 col-index) 
                  do
                  (dotimes (i col-index) 
                    (afl:synchronize-and-play  *column-cue*))
                  (read-aloud  column))
            (afl:synchronize-and-play  *row-cue*)))))

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Thu Nov 19 15:30:15 1992

                                     (defmethod read-aloud ((tabular tabular))
                                       "Read aloud a table, not fully implemented"
                                       (let ((contents (if *transpose-table*
                                                           (transpose-table (contents tabular))
                                                           (contents tabular ))))
                                         (loop for row in  contents
                                               do
                                                  (loop for column in row
                                                        and
                                                          col-index = 1 then (+ 1 col-index)
                                                        do
                                                           (dotimes (i col-index)
                                                             (tts:icon  *column-cue*))
                                                           (read-aloud  column))
                                                  (tts:icon  *row-cue*)))
                                       )

                                     (defmethod read-aloud ((math-eqnarray math-eqnarray))
                                       "Read math array, not fully implemented"
                                       (loop for row in  (contents math-eqnarray)
                                             do
                                                (loop for column in row
                                                      and
                                                        col-index = 1 then (+ 1 col-index)
                                                      do
                                                         (dotimes (i col-index)
                                                           (tts:icon  *column-cue*))
                                                         (read-aloud  column))
                                                (tts:icon  *row-cue*))
                                       )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Tue Nov  3 12:48:25 1992

                                     (defmethod read-aloud ((integral-d integral-d))
                                       "Read aloud integral delimiter"
                                       (read-aloud "d ")
                                       (read-aloud (children integral-d))
                                       )

                                     (defmethod read-aloud ((math-subformula math-subformula))
                                       (read-aloud (contents math-subformula))
                                       (afl:send-space)
                                       (when (attributes math-subformula)
                                         (mapcar #'read-aloud
                                                 (sorted-attributes  (attributes
                                                                      math-subformula ))))
                                       )

;;; Method: READ-ALOUD                                       Author: raman
;;; Created: Tue Dec  1 11:50:14 1992

                                     (defmethod read-aloud ((delimited-expression delimited-expression))
                                       "Read aloud a delimited expression"
                                       (read-aloud(open-delimiter delimited-expression ))
                                       (with-reading-state (reading-state 'children)
                                         (loop for child in (children delimited-expression)
                                               do           (read-aloud child  )))
                                       (when (or  (attributes delimited-expression)
                                                  (eql 'mismatched-delimiters (delimited-expression-type delimited-expression)))
                                         (read-aloud (close-delimiter delimited-expression)))
                                       (when (attributes delimited-expression)
                                         (mapcar #'read-aloud
                                                 (sorted-attributes  (attributes
                                                                      delimited-expression ))))
                                       )

                                     (defmethod read-aloud ((table-element table-element ))
                                       "Just read out its contents"
                                       (declare (optimize (compilation-speed 0) (safety 0) (speed 3 )))
                                       (read-aloud (contents table-element ))
                                       (afl:force-speech)
                                       )

                                     (defmethod read-aloud ((factorial factorial ))
                                       "read aloud a factorial object"
                                       (read-aloud (contents factorial))
                                       (afl:subclause-boundary)
                                       (read-aloud "factorial,"))
