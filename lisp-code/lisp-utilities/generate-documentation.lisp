;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'user)


  ;;; Parameter: *READ-ALOUD-FILES*                            Author: raman
  ;;; Created: Thu Sep 23 21:20:43 1993

(defparameter *read-aloud-files*
(list
 "/home/raman/lisp-code/read-aloud/cardinal-numbers.lisp"
"/home/raman/lisp-code/read-aloud/complex-objects.lisp"
;"/home/raman/lisp-code/read-aloud/descriptive-math-readings.lisp"
;"/home/raman/lisp-code/read-aloud/font-afl-state-definitions.lisp"
;"/home/raman/lisp-code/read-aloud/get-user-feedback.lisp"
;"/home/raman/lisp-code/read-aloud/handling-different-fonts.lisp"
;"/home/raman/lisp-code/read-aloud/handling-reading-states.lisp"
;"/home/raman/lisp-code/read-aloud/literature.lisp"
;"/home/raman/lisp-code/read-aloud/macros-define-objects.lisp"
;"/home/raman/lisp-code/read-aloud/math-reader-aux.lisp"
;"/home/raman/lisp-code/read-aloud/math-reading-rules.lisp"
;"/home/raman/lisp-code/read-aloud/new-document-objects.lisp"
;"/home/raman/lisp-code/read-aloud/new-environment-definitions.lisp"
"/home/raman/lisp-code/read-aloud/read-aloud.lisp"
;"/home/raman/lisp-code/read-aloud/reading-rule-definitions.lisp"
;"/home/raman/lisp-code/read-aloud/reading-state-definitions.lisp"
"/home/raman/lisp-code/read-aloud/reading-styles-and-rules.lisp"
;"/home/raman/lisp-code/read-aloud/special-pattern-macros.lisp"
"/home/raman/lisp-code/read-aloud/special-patterns.lisp"
;"/home/raman/lisp-code/read-aloud/standard-math-objects.lisp"
"/home/raman/lisp-code/read-aloud/summary-style.lisp"
;"/home/raman/lisp-code/read-aloud/temporary-tex-objects.lisp"
;"/home/raman/lisp-code/read-aloud/trying-around-methods.lisp"
"/home/raman/lisp-code/read-aloud/var-subst.lisp")
  
  "Files in directory read-aloud ")


  ;;; Parameter: *AFL-FILE-LIST*                               Author: raman
  ;;; Created: Thu Sep 23 21:36:18 1993

(defparameter *afl-file-list*
(list "/home/raman/lisp-code/afl/01-speech-space.lisp"
"/home/raman/lisp-code/afl/02-user-definitions.lisp"
"/home/raman/lisp-code/afl/03-standard-voice-definitions.lisp"
"/home/raman/lisp-code/afl/04-moving-in-speech-space.lisp"
"/home/raman/lisp-code/afl/05-block-structure.lisp"
"/home/raman/lisp-code/afl/06-synthesizer-codes.lisp"
"/home/raman/lisp-code/afl/07-final-scaling.lisp"
"/home/raman/lisp-code/afl/dectalk-specific-code.lisp"
"/home/raman/lisp-code/afl/global-variables.lisp"
"/home/raman/lisp-code/afl/reference-variables.lisp"
"/home/raman/lisp-code/afl/test-assignments.lisp"
"/home/raman/lisp-code/afl/test-examples.lisp")
  "List of files in afl directory.")


  ;;; Parameter: *SOUND-AUDIO-FILE-LIST*                       Author: raman
  ;;; Created: Thu Sep 23 21:38:39 1993

(defparameter *sound-audio-file-list*
  (list "/home/raman/lisp-code/afl/sound-audio/audio-player.lisp"
        "/home/raman/lisp-code/afl/sound-audio/define-point-in-audio-space.lisp"
;        "/home/raman/lisp-code/afl/sound-audio/foreign-sparc-audio.lisp"
;        "/home/raman/lisp-code/afl/sound-audio/play-notes.lisp"
;        "/home/raman/lisp-code/afl/sound-audio/play-sound-file.lisp"
        "/home/raman/lisp-code/afl/sound-audio/point-in-audio-space.lisp")
  "List of files in sound audio component. ")

(defparameter *parser-files-to-document*
  (list
   "/home/raman/lisp-code/docs/doc-parser.lisp")
  "Document source in this file for parser. ")



  ;;; Function: DOCUMENT-CODE-IN-FILES                         Author: raman
  ;;; Created: Thu Sep 23 21:25:38 1993

(defun document-code-in-files (list-of-files output-file) 
  "Document the lisp code in list-of-files"
  (with-open-file  (o-stream output-file :direction :output)
    (mapc #'(lambda(file)
              (create-user-manual file
                                  :output-format 'latex
                                  :output-stream o-stream
                                  :purge-latex nil)) list-of-files))
  )

  ;;; Parameter: *TOTAL-AUDIO-FILES*                           Author: raman
  ;;; Created: Thu Sep 30 13:26:51 1993

(defparameter *total-audio-files*
  (list "/home/raman/lisp-code/afl/total-space/assignments.lisp"
"/home/raman/lisp-code/afl/total-space/initialize-total-space.lisp"
"/home/raman/lisp-code/afl/total-space/total-space-state.lisp"

)
   "Files making up the total audio space")


  ;;; Parameter: *BROWSER-FILES*                               Author: raman
  ;;; Created: Fri Oct 22 09:11:16 1993

(defparameter *browser-files*
  (list "/home/raman/lisp-code/browse/bookmark.lisp"
        "/home/raman/lisp-code/browse/browse-macros.lisp"
        "/home/raman/lisp-code/browse/cross-references.lisp"
        "/home/raman/lisp-code/browse/move-around.lisp"
        "/home/raman/lisp-code/browse/summarize.lisp")
  "List of files making up the browser.")
