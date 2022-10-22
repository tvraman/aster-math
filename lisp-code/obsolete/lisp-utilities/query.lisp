;;; Tue Mar 23 17:39:14 1993 by Mark Kantrowitz <mkant@GLINDA.OZ.CS.CMU.EDU>
;;; query.lisp -- 11797 bytes

;;; ********************************
;;; QUERY Package ******************
;;; ********************************
;;;
;;; The QUERY package provides 5 functions:
;;;    Y-OR-N-P, YES-OR-NO-P, Y-OR-N-P-WAIT, YES-OR-NO-P-WAIT, and QUERY
;;; The latter is similar in concept to the Symbolics FQUERY, but there are
;;; several major differences, including the ability to timeout with a 
;;; default value. The Y-OR-N-P-WAIT and YES-OR-NO-P-WAIT illustrate this
;;; ability. All five functions are built upon the same substrate.
;;;
;;;
;;; To Do:
;;;    Add wildcard choices (e.g., for "otherwise" clause )?
;;;    What to do if called within gnu-emacs as opposed to xterm? Needs some
;;;    general CBREAK-mode style fix. 
;;;
;;; *** Change Log:
;;; 23-MAR-93 mk    Created.
;;; 30-MAR-93 mk    Listing the choices now does (y or n )and (y, n, or h )
;;;                 instead of (y, n )and (y, n, h ).



(defpackage "QUERY" (:use "LISP" )
    (:shadow y-or-n-p yes-or-no-p )
    (:export query y-or-n-p yes-or-no-p y-or-n-p-wait yes-or-no-p-wait ))

(in-package "QUERY" )

;;; ********************************
;;; Global Variables ***************
;;; ********************************

(defparameter *y-or-n-p-choices*
    '(((T "Yes" )#\y #\t #\space )
      ((NIL "No" )#\n )
      ((:help "Help" )#\h #\? )))
(defparameter *yes-or-no-p-choices*
    '((T "Yes" )
      (NIL "No" )
      (:help "Help" "h" "?" )))

(defparameter *timeout* 20 )
(defparameter *y-or-n-p-wait-default* #\n )
(defparameter *yes-or-no-p-wait-default* "No" )

;;; ********************************
;;; Reading with Timeouts **********
;;; ********************************
;;;
;;; Lots of Lisps, especially those that run on top of UNIX, do not get
;;; their input one character at a time, but a whole line at a time because
;;; of the buffering done by the UNIX system. This causes y-or-n-p-wait
;;; to not always work as expected. 
;;;
;;; I wish lisp did all its own buffering (turning off UNIX input line
;;; buffering by putting the UNIX into CBREAK mode ). Of course, this means
;;; that we lose input editing, but why can't the lisp implement this? 
;;;
;;; Note that CMU CL's time functions cons too much, causing GC to thrash 
;;; itself to death. Thus the -WAIT functions really aren't practical
;;; in CMU CL because of their busy-waiting.
;;;
;;; We might get better results if we change this to use fixnums instead
;;; of floats.

(defun internal-real-time-in-seconds ( )
  (float (/ (get-internal-real-time )
	    internal-time-units-per-second )))

(defun read-char-wait (&optional (timeout 20 )input-stream &aux peek )
  (do ((start (internal-real-time-in-seconds )))
      ((or (setq peek (listen input-stream ))
	   (< (+ start timeout )(internal-real-time-in-seconds )))
       (when peek
	 ;; was read-char-no-hang
	 (read-char input-stream )))))

(defun read-line-wait (&optional (timeout 20 )input-stream &aux peek )
  (do ((start (internal-real-time-in-seconds )))
      ((or (setq peek (listen input-stream ))
	   (< (+ start timeout )(internal-real-time-in-seconds )))
       (when peek
	 (read-line input-stream )))))

;;; ********************************
;;; Query Functions ****************
;;; ********************************
(defun QUERY (options &optional format-string &rest format-arguments )
  "QUERY is used for asking questions of the user. OPTIONS is a property-list
   containing the following keywords:
      :TYPE              :char or :line (how *query-io* is read )
      :CHOICES           A list containing entries of the form 
                         (<value> . <key>* )or ((<value> <print> ). <key>* ), 
                         where <value> is the value to be returned, <key> is
                         a character or string, as the case may be, that is
                         used to select the <value>, and <print> is printed
                         when the value is selected.
      :BEEP              Specifies whether to ring the bell.
      :CLEAR-INPUT       If T, does a clear-input before querying.
      :FRESH-LINE        If T, starts on a fresh line before printing
                         the prompt.
      :LIST-CHOICES      If T, prints a list of the choices after the prompt.
      :HELP-STRING       A string to be printed if the user asks for help.
      :TIMEOUT           The number of seconds to wait, if using timeouts.
      :TIMEOUT-DEFAULT   The default value to use if the query times out.
      :STREAM            The stream to use. Defaults to *query-io*
      :SIGNAL-CONDITION  If T, will signal a condition of type QUERY-ERROR
                         before proceeding. If non-NIL but not T, uses the
                         value as the name of the condition. "
  (apply #'query-internal 
	 :format-string format-string 
	 :format-arguments format-arguments
	 options ))

(defun y-or-n-p (&optional format-string &rest format-arguments )
  "Y-OR-N-P prints the message, if supplied, and reads a character
   from *QUERY-IO* until the user types a Y or an N, returning T and
   NIL, respectively. Repeats the request if the user typed anything
   else. Also, if a H or ? is enterred, prints a brief help message.
   If you want a question mark at the end of the message, you must
   put it there yourself; Y-OR-N-P will not add it."
  (apply #'query
	 `(:type :char :choices ,*y-or-n-p-choices*
		 :help-string "Type \"y\" for yes or \"n\" for no. " )
	 format-string format-arguments ))

(defun yes-or-no-p (&optional format-string &rest format-arguments )
  "YES-OR-NO-P prints the message, if supplied, rings the bell, and
   reads a line from *QUERY-IO* (ignoring whitespace )until the user
   types YES or NO, returning T and NIL, respectively. Repeats the
   request if the user typed anything else. Also, if HELP, H or ? is 
   enterred, prints a brief help message. If you want a question mark
   at the end of the message, you must put it there yourself;
   YES-OR-NO-P will not add it."
  (apply #'query
	 `(:type :line :choices ,*yes-or-no-p-choices*
		 :clear-input t :beep t
		 :help-string "Type \"yes\" for yes or \"no\" for no. " )
	 format-string format-arguments ))

(defun y-or-n-p-wait (&optional (default *y-or-n-p-wait-default* )
				(timeout *timeout* )
				format-string &rest format-arguments )
  "Y-OR-N-P-WAIT is like Y-OR-N-P, but will timeout after TIMEOUT seconds
   with DEFAULT as the default value."
  (apply #'query
	 `(:type :char :choices ,*y-or-n-p-choices*
		 :help-string "Type \"y\" for yes or \"n\" for no. "
		 :timeout ,timeout
		 :timeout-default ,default )
	 format-string format-arguments ))

(defun yes-or-no-p-wait (&optional (default *yes-or-no-p-wait-default* )
				   (timeout *timeout* )
				   format-string &rest format-arguments )
  "YES-OR-NO-P-WAIT is like YES-OR-NO-P, but will timeout after TIMEOUT seconds
   with DEFAULT as the default value."
  (apply #'query
	 `(:type :line :choices ,*yes-or-no-p-choices*
		 :clear-input t :beep t
		 :help-string "Type \"yes\" for yes or \"no\" for no. "
		 :timeout ,timeout
		 :timeout-default ,default )
	 format-string format-arguments ))

(defun query-internal (&key format-string format-arguments
			    (type :char )
			    (choices *y-or-n-p-choices* )
			    (beep nil )
			    (clear-input nil )
			    (fresh-line nil fresh-line-p )
			    (list-choices t )
			    (help-string nil )
			    timeout
			    timeout-default
			    (stream *query-io* )
			    (signal-condition nil ))
  ;; Default :FRESH-LINE appropriately if the user didn't specify it as 
  ;; a keyword.
  (unless fresh-line-p
    ;; If QUERY is called with no format arguments, don't do a FRESH-LINE.
    ;; We assume that any message has been printed by other means.
    ;; If QUERY is called with format arguments, does do a FRESH-LINE.
    (if (null format-string )
	(setq fresh-line nil )
	(setq fresh-line t )))
  ;; Signal a condition, giving the program a chance to handle it.
  (when signal-condition
    ;; If SIGNAL-CONDITION was specified as T, signals an error of type
    ;; 'QUERY-ERROR. Otherwise it uses SIGNAL-CONDITION as the error.
    (signal (if (eq signal-condition t )
		'query-error
		signal-condition )
	    :format-string format-string
	    :format-arguments format-arguments
	    :choices choices ))
  ;; Clear-input, then beep, then freshline. Is the order correct?
  (loop
    ;; Clear the input stream and beep, if necessary.
    (when clear-input (clear-input stream ))
    (when beep        (write-char #\bell stream )(finish-output stream ))
    ;; format the prompt
    (when format-string
      (when fresh-line (fresh-line stream ))
      (apply #'format stream format-string format-arguments )
      ;; We use FINISH-OUTPUT here to force it to print the message
      ;; before reading the reply. In CMU CL, for example, streams
      ;; are buffered, so using FORCE-OUTPUT could result in the
      ;; prompt being printed after the user types his or her reply.
      (finish-output stream ))
    ;; list choices
    (when list-choices
      (let ((choices (mapcar #'cadr choices )))
	(when choices
	  ;; Changed it so that it does (y or n )and (y, n, or h )instead
	  ;; of (y, n )and (y, n, h ). Ain't FORMAT wonderful?
	  (format stream " (~{~#[~;~A~;~A or ~A~:;~@{~#[~;or ~]~A~^, ~}~]~} )"
		  choices )
	  ;; (format stream " (~A~{, ~A~} )" (first choices )(rest choices ))
	  (finish-output stream ))))
    ;; choose
    (let* ((input (case type
		    (:char 
		     (if timeout
			 (or (read-char-wait timeout stream )
			     (when timeout-default
			       (format stream "~A~%" timeout-default )
			       (finish-output stream )
			       timeout-default ))
			 (read-char stream )))
		    (:line
		     ;; Note that #\space is allowed in :char mode, but
		     ;; not in :line mode.
		     (string-trim '(#\space #\tab #\newline #\return )
				  (if timeout
				      (or (read-line-wait timeout stream )
					  (when timeout-default
					    (format stream "~A~%"
						    timeout-default )
					    (finish-output stream )
					    timeout-default ))
				      (read-line stream ))))))
	   (choice (find input choices
			 :test (ecase type
				 (:char #'(lambda (x y )
					    (find x y :test #'char-equal )))
				 (:line #'(lambda (x y )
					    (find x y :test #'string-equal ))))
			 :key #'cdr )))
      (cond ((or (and (stringp input )
		      (string= input "" ); user typed nothing
		      help-string )
		 (and choice
		      (if (listp (car choice ))
			  (eq (car (car choice )):help )
			  (eq (car choice ):help ))))
	     ;; If help is requested, or the user typed nothing and we have
	     ;; a help string, print some help.
	     (when help-string
	       (fresh-line stream )
	       (format stream help-string )
	       (finish-output stream ))
	     ;; Don't beep if (s )he only asked for help.
	     (setq beep nil clear-input t fresh-line t list-choices t ))
	    (choice
	     ;; It found a choice, and will return it
	     (setq choice (car choice ))
	     (when (listp choice )
	       ;; or write-string/write-char
	       (format stream "~A" (second choice ))
	       (finish-output stream )
	       (setq choice (car choice )))
	     (return-from query-internal choice ))
	    (t
	     ;; Otherwise, we're going around the loop again, but
	     ;; we're definitely going to beep, clear-input, fresh-line,
	     ;; and list-choices.
	     (setq beep t clear-input t fresh-line t list-choices t ))))))


;;; ********************************
;;; Examples and Test Cases ********
;;; ********************************

#|
;;; In the first example, we timed out.
USER(313 ): (query:y-or-n-p-wait #\y 3 "Do you want it?" )
Do you want it? (y, n, h )y
Yes
T
USER(314 ): USER(314 ): (query:y-or-n-p-wait #\y 3 "Do you want it?" )
Do you want it? (y, n, h )n
No
NIL
USER(315 ): USER(315 ): (query:y-or-n-p-wait #\y 3 "Do you want it?" )
Do you want it? (y, n, h )h
Type "y" for yes or "n" for no. 
Do you want it? (y, n, h )y
Yes
T
USER(316 ): USER(316 ): (query:y-or-n-p-wait #\y 3 "Do you want it?" )
Do you want it? (y, n, h )h
Type "y" for yes or "n" for no. 
Do you want it? (y, n, h )n
No
NIL
|#

;;; *EOF*
