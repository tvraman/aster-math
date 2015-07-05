(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; TTS Calls used by  read-aloud.

(export
 '(synchronize-and-play should-i-stop? should-i-continue?
   audio-prompt send-space
   high-intonation low-intonation high-low-intonation
   comma-intonation period-intonation paragraph-begin
   set-period-pause set-comma-pause
   pause await-silence
   interrogative exclamation
   primary-stress secondary-stress exclamatory-stress
   with-surrounding-pause force-speech
   send-text speak-number-string subclause-boundary))
(defmacro with-surrounding-pause (pause-amount &body body)
  "Execute body with surrounding pause specified by pause-amount"
  `(progn
     (tts:pause ,pause-amount)
     ,@body
     (tts:pause ,pause-amount)))

(defun should-i-stop? (&rest ignore)
"stub"
nil)
(defun should-i-continue? (&rest ignore)
"stub"
nil)

(defun synchronize-and-play (name  &rest ignore)
  "Stub"
  (tts:icon name)
  (tts:force))

;;; The functions  below will be auto-generated based on engine in  use.

;;; Stubbed for now
(defun  make-audio-filename (name)
  "Stubbed for now"
  name)

(defun send-space ()
  "Send a space to TTS."
  (tts:queue " "))

(defun set-period-pause (msec)
  (send-text  (format nil "[:pp ~a]" msec)))

(defun set-comma-pause(msec)
  (send-text (format nil "[:cp ~a]" msec)))

(defun pause (milliseconds)
  "Pause for so many milliseconds"
  (assert (typep milliseconds 'fixnum) nil
          "Milliseconds = ~a, which is not a number" milliseconds)
  (when (> milliseconds 0)
    (tts:pause  milliseconds)))

(defun force-speech ()
  (tts:force))

(defun send-text (text)
  (tts:queue text))

(defun speak-number-string (number-string)
  (tts:queue number-string))
;;; no-op
(defun await-silence () t)

;;;not implemented

(defun subclause-boundary ()
  "Insert a subclause boundary"
  (tts:queue " "))

(defun high-intonation ()
  "Generate H*"
  (tts:queue " "))

(defun low-intonation ()
  "Generate L*"
  (tts:queue " "))

(defun high-low-intonation ()
  "Generate Hl*"
  (tts:queue " "))

(defun comma-intonation ()
  "Generate a comma intonation"
  (tts:queue " "))

(defun period-intonation ()
  "Generate a period intonation"
  (tts:queue " "))

(defun paragraph-begin  ()
  "Begin a paragraph"
  (tts:queue " "))

(defun exclamation  ()
  "Send an exclamation. "
  (tts:queue " "))

(defun interrogative  ()
  "Send an interrogative. "
  (tts:queue " "))

(defun primary-stress  ()
  "Send a  primary-stress. "
  (tts:queue " "))

(defun secondary-stress  ()
  "Send a  secondary-stress. "
  (tts:queue " "))

(defun exclamatory-stress  ()
  "Send an   exclamatory-stress. "
  (tts:queue " "))

(defun audio-prompt(control-string &rest arguments)
  "Send a prompt to the speech device and return T. "
  (tts:speak (format nil control-string arguments ))
  t)
