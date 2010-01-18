
;;; load this to use run-lisp  with clisp 

(require 'cl)
(require 'define-text-objects)
(require  'read-aloud)
(require 'clisp-template)
(setq lisp-mode-hook
      (function (lambda ()
		  (local-set-key "\M-a" 'beginning-of-defun)
		  (local-set-key "\C-c=" 'clisp-make-template)
		  (local-set-key "\C-c\:"
				 'generate-read-aloud-method)
		  (local-set-key "\C-c'"
				 'generate-define-text-object)
		  (local-set-key
		   "\C-c"
		   'lisp-insert-modification-time))))


(defvar clisp "clisp" 
  "Where your Lisp is installed. ")

(defvar aster-init 
  (expand-file-name "~/emacs/lisp/aster/clisp-init.lisp"))


(defvar aster-lisp  
  (format "%s -on-error appease -i -q  %s" 
	  clisp aster-init )
  "How to start up Aster")

(defun aster()
  "Starts up Aster. "
  (interactive)
  (declare (special aster-lisp))
  (run-lisp aster-lisp)
  (load-library "reader-browse"))

