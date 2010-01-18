;;;   -*- SYNTAX: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'user)
;;; This file contains the variables you need to set in order to
;;; configure AsTeR for a particular site.
;;; All of the definitions here are commented out.
;;; If you need to set one of these, just modify it and uncomment that
;;; particular setting. 
;;; { Comments: 

;;;
;;; Fri Jan  7 08:38:04 EST 1994
 ;;; Contains all external switches that configure AsTeR.
;;; All of these variables and parameters   were defined within  the
;;; source code. I am now moving them in here, so that it is easy to
;;; configure the system at another site etc.
;;;
;;; A simple way of configuring the system is to load the source code
;;; as is and then set the values of the necessary variables in this
;;; file.
;;; That is why everything here is a defparameter.
;;; Load this file after everything else is loaded, as loading this
;;; file is only for customizing the system for a specific site. 
;;; Definitions of these variables and parameters in the source will
;;; be marked by the comment ;;; external Parameter:
;;; This will help locate the related sources and documentation.

;;; }
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file is folded using folding mode.
;;; Each fold corresponds to a module of AsTeR.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; {System definition: installation
;
;;
;;;;; You need defsystem by Mark Kantrowitz available from the common
;;;;; lisp repository.
;;;;; The variables documented here are normally defined in my
;;;;; lisp-init.lisp, and  the various  system definition files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defvar *defsystem-directory*
;;  (concatenate 'string
;;               *lisp-code-directory* "/"  "lisp-utilities")
;;  "make for common lisp")
;;make parameters:
;;(setf make:*compile-during-load* t)
;;(setf make:*central-registry*
;;      (concatenate 'string *lisp-code-directory*
;;                   "/" "system-definitions"))
;;(defvar *lucid-version* 4.0)
;;
;;(defvar *lisp-code-directory*
;;  "/home/raman/lisp-code"
;;  "directory under which lisp code is organized")
;
;;
;;
;;
;;(defvar *afl-pathname*
;;  (concatenate 'string *lisp-code-directory*  "/" "afl")
;;  "source code resides here")
;;
;;
;;(defvar *announce* t  "If t announce actions on dectalk")
;;
;;;;; Variable: *DECTALK-PATHNAME*                             Author: raman
;;;;; Created: Mon Mar 30 20:13:56 1992
;;
;;(defvar *dectalk-pathname*
;;  (concatenate 'string  *lisp-code-directory* "/" "multivoice")
;;  "directory where multivoice code kept.")
;;
;;;;; Variable: *COMPATIBILITY-PATHNAME*                      Author: raman
;;;;; Created: Mon Mar 30 20:07:44 1992
;;
;;(defvar *compatibility-pathname*
;;  (concatenate 'string  *lisp-code-directory* "/" "compatibility" )
;;  "pathname where compatibility sources are kept.")
;;
;;          
;;(defparameter *books*
;;  '( rz-book vanloan-book vavasis-book cs611-notes
;;     gries-book norvig-book dennis-math-books)
;;  "List of books")
;
;(defvar *announce* t
;  "If t then announce progress of loading aster on the dectalk.
;If this is set to nil, then the Dectalk should be powered on for the
;system to load. ")
;;; }
;;; { Recognizer

;
;;; {Lexical analysis 

;;; Lexical analyser:
;
;;;; Parameter: *LEX-DIR*                                      Author: raman
;;;; Created: Fri Feb 21 09:14:15 1992
;
;(defparameter *lex-dir* "/home/raman/lisp-code/lexer"
;  "Directory where lexer resides. ")
;
;;;; Parameter: *LEX-PROGRAM*                                  Author: raman
;;;; Created: Fri Feb 21 09:15:19 1992
;
;(defparameter *lex-program* "lispify" 
;  "The program which does the lexical analysis")

;;; }
;;;; parsing: 
;;;; Parameter: *VALID-SECTIONAL-UNIT-NAMES*                   Author: raman
;;;; Created: Thu Apr  9 15:50:50 1992
;
;(defparameter *valid-sectional-unit-names* 
;  (list
;   'part
;   'chapter 
;   'section
;   'subsection
;   'subsubsection)
;  "list of valid sectioning units")
;
;  ;;; Parameter: *OBJECTS-THAT-CAN-BE-REFERRED*                 Author: raman
;  ;;; Created: Tue Dec 29 09:16:04 1992
;
;(defparameter *objects-that-can-be-referred*
;  '(new-environment  math-eqnarray
;    math-equation part chapter section subsection subsubsection enumerate item )
;  "These objects can be referred to by cross references")
;
;;;; Parameter: *SIGNAL-ERROR-ON-UNKNOWN-TEX-MACRO*            Author: raman
;;;; Created: Thu Jan 30 11:54:06 1992
;
;(defparameter *do-not-signal-error-on-unknown-tex-macro*   t 
;  "tell parser to signal error or continue when undefined tex macro
;  seen.")
;
;
;  ;;; Parameter: *CITATION-MUST-HAVE-A-BLOCK-ARGUMENT*         Author: raman
;  ;;; Created: Sun Oct 17 16:12:16 1993
;
;(defparameter *citation-must-have-a-block-argument* t
;  "If t, then \cite should be called with its argument in braces. Note:
;AMS bulletins violate this. ")
;
;
;  ;;; Parameter: *REF-MUST-HAVE-A-BLOCK-ARGUMENT*              Author: raman
;  ;;; Created: Sun Oct 17 16:14:04 1993
;
;(defparameter *ref-must-have-a-block-argument* t
;  "If T, then \ref must be called with its argument in braces. AMS
;bulletins violate this. ")
;

;;; }
;;; {Multivoice
;
;;;; The next two variables are defined and exported by the dectalk package
;(defparameter dectalk:*serial-unit-number* 
;  #+symbolics 2  #+lucid :B
;  "Unit number for Serial Port to dectalk.")
;
;(defparameter dectalk:*baud-rate* 9600)
;
;;*mung-serial-pathname* is defined in package serial.
;;;; It gives the pathname to the object file that is used to open the
;;;; serial line . 
;;; For some weird reason, load-foreign-files only gives correct result
;;; if passed a string arg.  a pathname does not work.
;(defvar *mung-serial-pathname*
;  (format nil "~A"
;          (make-pathname :name "mung-serial" :type "o"
;                         :directory'(:root "home"   "raman" "lisp-code" "compatibility")
;                         )))
;

;;; }
;;; { AFL

;
;;;; Note: Only those variables that would be changed by the user to
;;;; configure afl are listed here.
;;;; All of these variables are defined in the AFL package.
;;;; Unless mentioned otherwise, they are exported by package AFL
;;;; AFL program variables etc. are therefore not listed. 
;
;;;; Constant: *DEFAULT-VOICE*                                Author: raman
;;;; Created: Sun Aug 30 19:11:20 1992
;
;(defparameter *default-voice* 'paul  "default voice")
;
;
;;;; Parameter: *LAZY-SET-STATE*                               Author: raman
;;;; Created: Mon Aug 24 08:24:11 1992
;
;(defparameter *lazy-set-state*
;  nil
;  "If t set-speech-state sets all the dimensions, without checking if some
;dimension has actually been modified. Setting this to t will slow down
;the dectalk. This variable is internal to the AFL package.")
;
;
;;;; Parameter: *BEGIN-COMMAND*                                Author: raman
;;;; Created: Tue Aug 11 13:40:16 1992
;
;(defparameter *begin-command* "[" " string that begins a synthesizer command")
;
;;;; Parameter: *END-COMMAND*                                  Author: raman
;;;; Created: Tue Aug 11 13:40:44 1992
;
;(defparameter *end-command* "]" "string that ends a synthesizer command")
;
;;;; Parameter: *PRONOUNCE-IGNORE-CASE-IN-TEXT*                Author: raman
;;;; Created: Tue Nov 10 15:28:51 1992
;
;(defparameter *pronounce-ignore-case-in-text* t
;  "If t case ignore in text mode when choosing pronunciation")
;
;
;
;  ;;; Parameter: *ALWAYS-DEHYPHENATE*                           Author: raman
;  ;;; Created: Wed May  5 09:34:26 1993
;
;(defparameter *always-dehyphenate* t
;  "Always dehyphenate words. Avoids the dectalk spelling out things. ")
;
;
;  ;;; Parameter: *SOUND-PRIORITY*                               Author: raman
;  ;;; Created: Thu Jan  7 13:59:09 1993
;;;; *sound-priority* is assigned a high number. Lucid assigns a
;;;; default scheduling priority of 100 to processes, and the manual
;;;; looks confused as to whether high numbers mean high priority, but
;;;; simple tests indicate that high numbers mean lower priority.
;;;; Setting priority to a high number for sound players will allow the
;;;; reader to run without introducing irritating pauses in the speech.
;;;;
;(defparameter *sound-priority* 101  "Priority for scheduling sounds")
;
;
;;;; Parameter: *READER-PERIOD-PAUSE*                          Author: raman
;;;; Created: Mon Apr 13 10:01:35 1992
;
;(defparameter  *reader-period-pause*  -380  "period pause used by reader.")
;
;;;; Parameter: *READER-COMMA-PAUSE*                           Author: raman
;;;; Created: Mon Apr 13 10:04:31 1992
;
;(defparameter *reader-comma-pause*  -40  "comma pause used by
;reader.")

  ;;; Variable: *AWAIT-SILENCE-WHEN-USING-STEREO*              Author: raman
  ;;; Created: Sun Jan  9 12:20:50 1994

;(defvar *await-silence-when-using-stereo* nil 
;  "If T, then await silence before changing parameters that affect
;directional speech. ")

;;; }
;;; { Audio files

;
;;;; The following variables are defined and exported from the AFL package.
;
;  ;;; Parameter: *SOUND-DIR-NAME*                               Author: raman
;  ;;; Created: Fri Jan  8 13:21:15 1993
;
;(defparameter *sound-dir-name*
;  "/usr/u/raman/sounds/cues/"
;  "sound directory as a string. This is used by the function
;  make-audio-filename .")
;
;
;(defparameter *play-cmd* "play"  "play command")

;;; }
;;; { sound cues

;
;  ;;; Parameter: *FOOTNOTE-CUE*                                 Author: raman
;  ;;; Created: Mon Oct 25 16:50:59 1993
;
;(defparameter *footnote-cue*
;  (afl:make-audio-filename "footnote")
;  "Footnote cue. ")
;;;; Parameter: *ARTICLE-CUE*                                  Author: raman
;;;; Created: Sun May  3 19:04:31 1992
;
;(defparameter *article-cue*
;  (afl:make-audio-filename "article" )
;  "Cue to use at the start and end of articles.")
;;;; Parameter: *ARTICLE-OPEN-CUE*                             Author: raman
;;;; Created: Tue May 12 13:49:55 1992
;
;(defparameter *article-open-cue*
;  (afl:make-audio-filename "article-open")
;  "open an article")
;
;;;; Parameter: *ARTICLE-CLOSE-CUE*                            Author: raman
;;;; Created: Tue May 12 13:50:26 1992
;
;(defparameter *article-close-cue*
;  (afl:make-audio-filename "article-close" )
;  "close an article.")
;
;
;;;; Parameter: *ABSTRACT-CUE*                                 Author: raman
;;;; Created: Thu May  7 13:00:16 1992
;
;(defparameter *abstract-cue*
;
;  (afl:make-audio-filename "abstract")
;  "cue for abstract")
;
;
;;;; Parameter: *PARAGRAPH-CUE*                                Author: raman
;;;; Created: Sun May  3 19:06:02 1992
;
;(defparameter *paragraph-cue*
;  (afl:make-audio-filename "paragraph")
;  "cue to use at the start of a new paragraph.")
;;;; Parameter: *SECTION-CUE*                                  Author: raman
;;;; Created: Sun May  3 19:06:33 1992
;
;(defparameter *section-cue*
;  (afl:make-audio-filename "section")
;  "cue at the beginning of sections.")
;;;; Parameter: *ITEM-CUE*                                     Author: raman
;;;; Created: Sun May  3 19:11:11 1992
;
;(defparameter *item-cue*
;  (afl:make-audio-filename "item")
;  "Cue for items.")
;
;;;; Parameter: *NEWLINE-CUE*                                  Author: raman
;;;; Created: Sun May  3 19:07:03 1992
;
;(defparameter *newline-cue*
;  (afl:make-audio-filename "newline")
;  "Cue for newlines.")
;
;;;; Parameter: *SLIDE-CUE*                                    Author: raman
;;;; Created: Mon May  4 12:41:54 1992
;
;(defparameter *slide-cue*
;  (afl:make-audio-filename "slide")
;  "cue for slides.")
;
;
;  ;;; Parameter: *PROMPT-CUE*                                   Author: raman
;  ;;; Created: Sat May  1 13:06:02 1993
;
;(defparameter *prompt-cue*
;  (afl:make-audio-filename "arcade_beep")
;  "Sound to play when prompting. ")
;  ;;; Parameter: *CROSS-REF-CUE*                               Author: raman
;  ;;; Created: Sat May  1 15:21:47 1993
;
;(defparameter *cross-ref-cue*
;  (afl:make-audio-filename "multi_beep")
;  "Cue cross reference. ")

;;; }
;;; {Reading rules 

;
;;;; Parameter: *PLAY-SIGNATURE-TUNE*                          Author: raman
;;;; Created: Wed Apr 29 21:45:41 1992
;
;(defparameter *play-signature-tune* nil
;  "If t play a tune before and after reading document.")
;
;
;
; ;;; Function: READ-AS-INFIX                                  Author: raman
; ;;; Created: Tue Nov 24 15:41:34 1992
;
;(defparameter *use-comma-intonation-for-lists* nil
;  "if t use comma intonation when reading long lists")
;;;; Parameter: *PUNCTUATIONS*                                 Author: raman
;;;; Created: Sat Apr 11 19:55:50 1992
;
;(defparameter *punctuations*
;  (list
;   "."
;   ","
;   "!"
;   "?"
;   ":"
;   ";"
;   "("
;   ")"
;   )
;  "list of known punctuation characters.")
;
;
;;;; Parameter: *END-OF-SENTENCE-MARKER*                       Author: raman
;;;; Created: Mon Apr 13 11:16:00 1992
;
;(defparameter *end-of-sentence-marker*
;(list
;       "."
;       "?"
;       "!"
;       )
;  "end of sentence markers")
;
;
;;;; Parameter: *SPEAK-MATH-IMMEDIATELY*                       Author: raman
;;;; Created: Tue Sep 29 20:27:39 1992
;(defparameter *speak-math-immediately* nil
;  "If t then dectalk speaks math immediately. Set this to nil when
;reading full documents. ")
;
;;;; Parameter: *ATTRIBUTES-READING-ORDER*                     Author: raman
;;;; Created: Mon Oct 26 15:28:31 1992
;
;(defparameter *attributes-reading-order*
;  '( subscript superscript underbar accent left-subscript
;    left-superscript )
;  " Sorted list of attributes")
;
;  ;;; Parameter: *STEP-THROUGH-MATH-READINGS*                            Author: raman
;  ;;; Created: Thu Nov 11 12:15:24 1993
;
;(defparameter *step-through-math-readings* nil "Switch to determine if we step
;through a math reading. ")
;
;;;; This threshold determines if this object is complex enough to be
;;;; stepped through:
;
;  ;;; Parameter: *MATH-STEP-THRESHOLD*                          Author: raman
;  ;;; Created: Thu Nov 11 12:16:12 1993
;
;(defparameter *math-step-threshold* 2 "Threshold value for stepping through math objects. Compare weight of a
;math object against this threshold. ")
;
;
;
;  ;;; Parameter: *GET-LABEL-WAIT*                              Author: raman
;  ;;; Created: Sat May  1 12:55:39 1993
;
;(defparameter *get-label-wait* 1
;  "Seconds to wait to see if user wants to enter a label")
;
;
; ;;; Parameter: *PAUSE-AROUND-CHILD*                            Author: raman
; ;;; Created: Thu Nov 26 15:52:33 1992
;
;(defparameter *pause-around-child*  5
;  "Pause  surrounding reading of child")
;
;
;;;; Parameter: *MATH-SURROUND*                               Author: raman
;;;; Created: Thu Nov 26 12:30:20 1992
;
;(defparameter *math-surround* 50 
;  "Amount of pause around inline math in milliseconds. ")
;
;
;  ;;; Parameter: *PARAGRAPH-SUMMARY-LENGTH*                    Author: raman
;  ;;; Created: Sat Apr 17 13:36:29 1993
;
;(defparameter *paragraph-summary-length*  'sentence
;  "Read so many words to
;summarize a paragraph.
; If 'sentence, read first sentence. If 'clause, read first clause. ")
;
;  ;;; Parameter: *WAIT-BEFORE-READING-SUBSTITUTIONS*           Author: raman
;  ;;; Created: Sun Sep 26 10:51:33 1993
;
;(defparameter *wait-before-reading-substitutions* t
;  "If t, wait before speaking the substitutions when using the variable
;substitution reading style.
;If set to an integer, this is used as a timeout, i.e. reading
;continues if y is not  pressed.")
;
;  ;;; Parameter: *ATTR-WEIGHT-FACTOR*                          Author: raman
;  ;;; Created: Wed May 26 18:09:25 1993
;
;(defparameter *attr-weight-factor* 2.5 "Scale weight of attributes")
;
;  ;;; Parameter: *ABSOLUTE-COMPLEXITY-THRESHOLD*               Author: raman
;  ;;; Created: Wed May 26 07:07:11 1993
;
;(defparameter *absolute-complexity-threshold* 5
;  "Absolute threshold beyond which substitution applied ")
;
;  ;;; Parameter: *PROPORTIONAL-COMPLEXITY-THRESHOLD*           Author: raman
;  ;;; Created: Tue May 25 12:38:22 1993
;
;(defparameter *proportional-complexity-threshold* (/ 1 7)
;  "Complexity threshold ratio. ")

;;; }
;;; {Browser 

;
;(defparameter *follow-cross-ref-wait* 1
;  "Follow cross reference behaviour. ")
;
(load  (concatenate 'string
                    *lisp-code-directory*
                    "system-definitions/aster-configure.lisp"))
;
;

;;; }

