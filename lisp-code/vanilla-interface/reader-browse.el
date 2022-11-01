;;;   doing a complete cleanup
;;; Sat May 20 14:49:38 EDT 1995
;;; Mon Jan 11 17:42:33 EST 1993
;;; Simple elisp interface to browser.
;;; Use C-b as a prefix key.
;;; send command to lisp and move point to the end of the buffer in
;;; each command.
(defvar reader-browse-keymap nil
  "Keymap used by the browser for AsTeR. ")

(setq reader-browse-keymap (copy-keymap global-map ))
(defvar reader-browser-prefix "\C-b"
  "prefix key to use for browser ")

(global-set-key reader-browser-prefix reader-browse-keymap)

(define-key reader-browse-keymap "a" 'reader-read-above)
(define-key reader-browse-keymap "d" 'reader-read-below)
(define-key reader-browse-keymap "f"
            'reader-follow-bookmark)
(define-key reader-browse-keymap "m"
            'reader-mark-read-pointer)
(define-key reader-browse-keymap "p"
            'reader-read-previous)
(define-key reader-browse-keymap "n" 'reader-read-next)
(define-key reader-browse-keymap "P" 'reader-read-parent )
(define-key reader-browse-keymap "c" 'reader-read-rest)
(define-key reader-browse-keymap " " 'reader-read-current)
(define-key reader-browse-keymap "k" 'reader-move-up)
(define-key reader-browse-keymap "C" 'reader-read-children)
(define-key reader-browse-keymap "j" 'reader-move-to-children )
(define-key reader-browse-keymap "\C-a" 'reader-move-to-abstract)
(define-key reader-browse-keymap "h"
            'reader-move-back)
(define-key reader-browse-keymap "l" 'reader-move-forward)
(define-key reader-browse-keymap '[left] 'reader-move-back)
(define-key reader-browse-keymap '[right] 'reader-move-forward)
(define-key reader-browse-keymap '[up] 'reader-move-up)
(define-key reader-browse-keymap '[down] 'reader-move-to-children )
(define-key reader-browse-keymap "i" 'reader-move-to-contents)
(define-key reader-browse-keymap "A" 'reader-move-to-attributes)
(define-key reader-browse-keymap "^" 'reader-move-to-superscript )
(define-key reader-browse-keymap "_" 'reader-move-to-subscript)
(define-key reader-browse-keymap "t" 'reader-to-top)
(define-key reader-browse-keymap "r" 'reader-read-just-this-node)
(define-key reader-browse-keymap "q" 'reader-quit-reading)
(define-key reader-browse-keymap "s" 'reader-stop-reading)
(define-key reader-browse-keymap "g" 'reader-read-follow-cross-ref)
(define-key reader-browse-keymap   reader-browser-prefix
            'switch-to-browse-keymap)
(define-key reader-browse-keymap "\C-d" 'read-aloud-region)
;;; now for the functions:
(defun move-to-end-of-lisp-buffer()
  "Move point to end of inferior lisp buffer"
  (save-excursion
    (set-buffer  inferior-lisp-buffer)
    (goto-char (point-max ))))

(defun reader-move-back (arg)
  "Move the selection   to the previous sibling.
Numeric prefix arg specifies how much to move,
default is to move by 1. "
  (interactive "p")
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(move-back %s )\n"
                                (or arg 1)))
  (move-to-end-of-lisp-buffer))

(defun reader-move-forward (arg)
  "Move the selection   to the next  sibling.
Numeric prefix arg specifies how much to move,
default is to move by 1. "
  (interactive "p")
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(move-forward %s )\n"
                                (or arg 1)))
  (move-to-end-of-lisp-buffer))

(defun reader-move-up (arg)
  "Move the selection   to the parent.
Numeric prefix arg specifies how much to move,
default is to move by 1. "
  (interactive "p")
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(move-up %s )\n"
                                (or arg 1)))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-move-to-attributes ()
  "Move the selection to the attributes of the current node. "
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(move-to-attributes )\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-move-to-abstract ()
  "Move the selection to the abstract "
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(move-to-abstract )\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-move-to-children ()
  "Move the selection to the first child of the current node. "
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(move-to-children )\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-move-to-subscript  ()
  "Move  the current selection to the subscript "
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(move-to-subscript )\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-move-to-superscript  ()
  "Move the current selection to the superscript. "
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(move-to-superscript )\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-move-to-contents  ()
  "Move the current selection  to the contents of the current node. "
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(move-to-contents )\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-follow-cross-ref  (arg)
  "Follow closest cross ref and read it.
With prefix arg look back. "
  (interactive "P")
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(read-follow-cross-ref %s )\n"
                                (if arg t nil )))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-rest ()
  "Read the remainder of the document starting at current selection. "
  (interactive )
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(read-rest  *read-pointer* )\n"))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-next (arg)
  "Move selection to the next sibling, and read it.
Prefix numeric arg specifies how much to move, default is 1."
  (interactive "p")
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(read-next %s )\n"
                                (or arg 1)))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-previous (arg)
  "Move selection to the previous sibling, and read it.
Prefix numeric arg specifies how much to move, default is 1."
  (interactive "p")
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(read-previous %s )\n"
                                (or arg 1)))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-parent (arg)
  "Move selection to the parent , and read it.
Prefix numeric arg specifies how much to move, default is 1."
  (interactive "p")
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(read-parent %s)\n"
                                (or arg 1)))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-above ()
  "If in a matrix or other tabular structure, move the current selection to the element above it,
and read it. "
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(read-above)\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-below ()
  "If in a matrix or other tabular structure,
 move the current selection to the element below it, and read it."
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(read-below)\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-children ()
  "Read all the chldren of the current selection. "
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(read-children)\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-follow-bookmark (arg)
  "read bookmark
With prefix arg, move the current selection as well."
  (interactive "P")
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format " %s \n"
                                (if arg "(goto-bookmark )"
                                  "(follow-bookmark )" )))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-mark-read-pointer()
  "Mark current selection"
  (interactive)
  (process-send-string
   (get-buffer-process inferior-lisp-buffer)
   (format " (mark-read-pointer) \n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-next-in-order()
  "Read next in order"
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(read-next-in-order
 )\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-previous-in-order()
  "Read previous  in order"
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(read-previous-in-order
 )\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-move-to-next-in-order()
  "Move to next in order"
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(move-to-next-in-order
 )\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-move-to-previous-in-order()
  "Move to previous  in order"
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(move-to-previous-in-order
 )\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-just-this-node ()
  "Just speak the node that is the current selection, rather than the entire subtree rooted at this node.
Useful when traversing complex math expressions. "
  (interactive)
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (format "(read-just-the-node)\n" ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-read-current (arg)
  "Read current node. With prefix arg, read it relative to its
  position in the entire document. "
  (interactive "P")
  (process-send-string  (get-buffer-process inferior-lisp-buffer)
                        (if arg
                            (format "(read-current-relatively) \n")
                          (format "(read-current )\n")
                          ))
  (move-to-end-of-lisp-buffer)
  )

(defun reader-to-top(arg)
  "Move the selection  to root of document
with prefix arg move to top of math. "
  (interactive "P")
  (process-send-string (get-buffer-process inferior-lisp-buffer)
                       (if arg
                           (format "(move-to-top-of-math )\n")
                         (format "(progn
(afl:refresh)
(type-of (setf *read-pointer* *document*)) \n
(summarize *read-pointer*) \n
(afl:force-speech)\n )\n")
                         )
                       ))

(defvar aster-browse-minor-mode nil "if t browse keymap is on")
(make-variable-buffer-local 'aster-browse-minor-mode)

(defun get-inferior-lisp-keymap ()
  "get inferior  lisp keymap"
  (save-excursion
    (set-buffer inferior-lisp-buffer)
    (current-local-map))
  )

(defvar inferior-lisp-buffer-keymap  (get-inferior-lisp-keymap))

(defun switch-to-browse-keymap()
  (interactive)
  "Toggle between inferior   lisp  local keymap and browse keymap"
  (declare (special aster-browse-minor-mode))
  (switch-to-buffer inferior-lisp-buffer)
  (cond
   (aster-browse-minor-mode
    (setq aster-browse-minor-mode nil)
    (use-local-map inferior-lisp-buffer-keymap)
    (message "Exitting Aster's browsing mode. Use %s to re-enter this mode. "
             (key-description (substitute-command-keys "\\[switch-to-browse-keymap]"))))
   (t (setq aster-browse-minor-mode t)
      (use-local-map reader-browse-keymap )
      (message "Entering Aster's browse mode. Use  %s to return to normal. "
               (key-description (substitute-command-keys "\\[switch-to-browse-keymap]")))))
  )

(defun reader-quit-reading()
  "Quit reading. The current selection is left where reading commenced."
  (interactive)
  (set-buffer inferior-lisp-buffer)
  (comint-interrupt-subjob)
  (progn
    (sleep-for 1)
    (process-send-string (get-buffer-process inferior-lisp-buffer) "abort\n")
    (message "Quit reading")
    ))

(defun reader-stop-reading()
  "Stop reading.
The current selection is left on the last object that was spoken."
  (interactive)
  (set-buffer inferior-lisp-buffer)
  (comint-interrupt-subjob)
  (message "Stopped reading" )
  )

(provide 'reader-browse)
