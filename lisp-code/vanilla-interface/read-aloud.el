;;; Wed Sep  2 11:15:28 EDT 1992
;;; Author: T. V.  Raman
;;; Grab region from a tex file,
;;; put a tex header and trailer around it,
;;; and write it to a file.
;;; Then call read aloud and parser in lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar read-aloud-directory
  "/tmp/"
"Where lispify looks for the file. ")

(defvar temp-file "temp-read")

(defvar document-preamble
  "\\begin{document}"
  "preamble for reading")

(defvar document-postamble
  "\\end{document}"
  "postamble for reading ")

(defvar reader-relative-pathnames-flag t 
  "if t then reader uses relative pathnames for include files,
otherwise expect absolute pathnames")

(defvar file-containing-region (concat read-aloud-directory
                                         temp-file))


(defvar inferior-lisp-buffer "*inferior-lisp*"
  "Name of inferior lisp interaction buffer.")

(defun read-aloud-region(start end )
   "Render this region using AsTeR. "
  (interactive "r")
(declare (special inferior-lisp-buffer))
  (let*
      ((dir-name(when buffer-file-name
                  (file-name-directory buffer-file-name )))
       (dir-command (when(and dir-name  reader-relative-pathnames-flag)
                      (format "(cd \"%s\") " dir-name ))))
    (prepare-region-for-reading start end)
    (message  "Reading region: ")
    (let
        ((command-string
          (concat
"(progn"
           dir-command 
           "(read-aloud-file "
           (format "\"%s\") " file-containing-region )
           ")\n"
           )
          )
         )
      (process-send-string
       (get-buffer-process inferior-lisp-buffer) 
       command-string)
                                        ; Move point to the end of the lisp  buffer
      (switch-to-buffer   inferior-lisp-buffer) 
      (goto-char (point-max )))
    )
  )

(defun aster-dump-region(start end )
  "read region. parsed document left in lucid *temp*" 
  (interactive "r")
  (let*
      ((dir-name(when buffer-file-name
                  (file-name-directory buffer-file-name )))
       (dir-command (when(and dir-name  reader-relative-pathnames-flag)
                      (format "(cd \"%s\") " dir-name )))
       (output-file (read-from-minibuffer  "Enter name of dtk file: "))
       )
    (prepare-region-for-reading start end)
    (message  "Dumping  region: ")
    (let
        ((command-string
          (concat
           "(unless (equal \"USER\"  (package-name *package*))
(in-package 'user))\n"
           dir-command 
           "(progn"
           "(setf *temp*  (parse-article "
           (format "\"%s\" " file-containing-region )
           "))"
           "(type-of (setf *read-pointer* *temp* ))"
           "(weight *temp*)"
"(afl:with-file-instead-of-dectalk (:filename  "
(format "\"%s\"" output-file )
")"
           "(read-aloud *read-pointer*)"
"))\n\n"
           )
           )
)
      (process-send-string
(get-buffer-process inferior-lisp-buffer)
command-string)
                                        ; Move point to the end of the lucid buffer
      (switch-to-buffer   inferior-lisp-buffer) 
      (goto-char (point-max )))
    )
  )


(defun read-it-again()
  "Read aloud once again avoiding parsing"
  (interactive)
(declare (special inferior-lisp-buffer))
  (let ((command-string "(read-aloud *document*)"))
    (process-send-string
     (get-buffer-process inferior-lisp-buffer ) command-string)
    )
  )

(defun open-temp-file-for-reading()
  "open temp file, and delete contents if necessary"
  (let 
      ((filename 
        (concat
         read-aloud-directory
         temp-file )))
    (find-file-noselect filename)
    )
  )

(defun prepare-region-for-reading(start end)
  (interactive "r")
  (let(
       (string (buffer-substring start end ))
       (buffer (open-temp-file-for-reading))
       )
    (save-excursion
      (set-buffer buffer)
(erase-buffer) 
(insert document-preamble)
      (insert string)
       (insert document-postamble)
      (clean-up-ctrl-m)
      (save-buffer)
      (kill-buffer  buffer)
      )
    )
  )
;;; clean up control m
(defun clean-up-ctrl-m()
  "Replace ctrl-m with ctrl-j"
(goto-char (point-min))
  (replace-string "\015" "\012")
  )


;;; reading math strings that have no math bracket aroundthem:
;;; Useful for reading calc output.

(defun math-buffer()
  "Return a buffer for placing math to be read. "
  (get-buffer-create "*math to be read*")
  )

(defun read-math-string(math-string)
  "Prepare math string to be read"
  (save-excursion
    (set-buffer (math-buffer))
    (let ((position (point )))
      (insert (format
               "$%s$\n"
               math-string))
      (read-aloud-region position (point))
      ))
  )

(defun read-aloud-region-as-math(start end) 
  (interactive "r")
  "Read contents of region as math: "
  (read-math-string
   (buffer-substring
    start end) )
  )


;;; select reading rule
;;; Updated to handle new version of reading rules, ie: defmethod
;;; reading rule. Old version of elisp code in the directory where the
;;; old lisp sources are kept.  <(old-reading-rules)>
(defun select-reading-rule(arg)
  "Select reading rule prompting for rule name and class.
With prefix arg, deactivate rule instead. "
  (interactive "P")
  (declare (special inferior-lisp-buffer))
  (let*  ((rule-name (read-minibuffer
                      (format "%s  rule named: "
                              (if arg "Deactivate" "Activate" ))))
          (class-name (read-minibuffer "For class: "))
          (command-string  (format
                            "(%s-rule   '%s \
 '%s )\n "
                            (if arg "deactivate" "activate")
                            class-name rule-name )))
    (process-send-string
     (get-buffer-process inferior-lisp-buffer )
     command-string)
    )
  )

(defun activate-reading-style(arg)
  "Activate reading style prompting for style name. With prefix arg,
  deactivate style. "
  (interactive "P")
(declare (special inferior-lisp-buffer))
  (let*  ((style-name (read-minibuffer
                       (format "%s style:"
                               (if arg "deactivate " "activate "))))
          (command-string
           (format "(%s-style '%s )\n"
                   (if arg "deactivate" "activate")
                   style-name )))
    (process-send-string
     (get-buffer-process inferior-lisp-buffer)
     command-string)
    )
  )

(defun read-previous-calc-answer()
  "Read aloud previous answer from calc"
  (interactive)
  (let ((start nil )
        (end  (point )))
    (save-excursion
      (when 
          (search-backward "1:"  nil nil )
        (forward-char 2)
        (setq start (point))
        (read-aloud-region-as-math start   end )))
    (switch-to-buffer "*Calculator*")
    )
  )



(provide 'read-aloud)
