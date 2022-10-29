;;; Wed Sep  2 11:15:28 EDT 1992
;;; Author: T. V.  Raman
;;; Grab region from a tex file,
;;; put a tex header and trailer around it,
;;; and write it to a file.
;;; Then call read aloud and parser in lucid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'read-aloud)

(defvar *read-aloud-directory*
  "/tmp/")

(defvar *temp-file* "temp-read")

(defvar *document-preamble*
  (expand-file-name "~/.read-aloud/preamble.tex")
  "preamble for reading")

(defvar *document-postamble*
  (expand-file-name "~/.read-aloud/postamble.tex")
  "postamble for reading ")

(defvar *file-containing-region* (concat *read-aloud-directory* *temp-file*))
(defun read-aloud-region(start end )
  "read region. parsed document left in lucid *temp*" 
  (interactive "r")
  (prepare-region-for-reading start end)
  (message  "Reading region: ")
  (let
      ((command-string
        (concat
         "(in-package 'user)"
         "(progn"
 "(setf *temp*  (parse-article "
(format "\"%s\" " *file-containing-region* )
"))"
"(read-aloud *temp*))"
)
)
)
;call as ilisp-send (string &optional message status and-go handler)
;l(ilisp-send command-string "reading region " t 'dispatch  nil)
(process-send-string (ilisp-process) command-string)
)
)

(defun read-it-again()
  (interactive)
  "Read aloud once again avoiding parsing"
  (let ((command-string "(read-aloud *temp*)"))
(process-send-string (ilisp-process) command-string)
)
  )

(defun open-temp-file-for-reading()
  "open temp file, and delete contents if necessary"
  (let 
  ((filename 
   (concat
    *read-aloud-directory*
    *temp-file* )))
    (when (file-exists-p filename)
      (delete-file filename))
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
    (insert-file-contents *document-postamble*)
  (save-excursion (insert string))
  (insert-file-contents *document-preamble*)
  (clean-up-ctrl-m)
(save-buffer)
(kill-buffer  buffer)
)
)
)
;;; clean up control m
(defun clean-up-ctrl-m()
  "Replace ctrl-m with ctrl-j"
  (replace-string  "" "
" )
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
(defun select-reading-rule()
  (interactive)
  "Select reading rule prompting for rule name and class"
  (let*  ((rule-name (read-minibuffer "Select rule named: "))
          (class-name (read-minibuffer "For class: "))
          (command-string  (format
                            "(select-reading-rule :name %s \
:class %s ) "
                            rule-name class-name )))
    (ilisp-send command-string "Selecting rule  " t 'dispatch  nil)
    )
  )

(defun toggle-lucid-variable()
  (interactive)
  (let
      ((variable (read-minibuffer "Toggle variable: ")))
    (process-send-string (ilisp-process)
                         (format "(toggle %s )"
                                 variable))
    (message "Toggled  %s " variable)
    )
  )
