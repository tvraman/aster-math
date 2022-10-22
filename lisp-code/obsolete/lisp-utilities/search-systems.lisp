;;;-*- Mode: Lisp; Package: CCL -*-
;*********************************************************************
;*                                                                   *
;*    PROGRAM      S E A R C H   S Y S T E M S                       *
;*                                                                   *
;*********************************************************************
   ;* Author:     Alex Repenning, ralex@cs.colorado.edu              *
   ;*             Copyright (c) 1992 Alex Repenning                  *
   ;* Address:    Computer Science Department                        *
   ;*             University of Colorado at Boulder                  *
   ;*             Boulder, CO 80309-0430                             *
   ;*                                                                *
   ;* Filename:   search-systems.lisp                                *
   ;* Update:     12/8/92                                            *
   ;* Version:                                                       *
   ;*   1.0  12/8/92                                                 *
   ;* System:     Macintosh II, MCL 2.0p1                            *
   ;* Abstract:   Search extension of Mark Kantrowitz's DEFSYSTEM.   *
   ;* Features:                                                      *
   ;*   - efficient Boyer Moore string search over many files.       *
   ;* Status: just started                                           *
   ;*                                                                *
   ;******************************************************************

(in-package :ccl)

;; (require "boyer-moore")

;---------------------------------------
;  DEFSYSTEM Search Operation           |
;------------------------------------------------------------------------------
; A system can be searched for a string:                                       |
;   (oos <system-name> :search :force <search-string>).                        |
; Unlike other system operations SEARCH requires a <search-string> argument.   |
; For the moment being the :force keyword is (mis)used for this purpose.       |
;------------------------------------------------------------------------------

(make::component-operation :search 'search-operation)
(make::component-operation 'search 'search-operation)

(defun SEARCH-OPERATION (Component Force) "
  in: Component {Component}, Force {string}.
  Every :file component will be called with this function.
  Force is misused to hold the search string."
  (make::with-tell-user ("Searching source" component :source)
    (do-dialog-file-search (make::component-full-pathname Component :source) Force))
  nil)


(defun GET-LIST-OF-SYSTEMS-FROM-USER () "
  out: Systems {list of: {keyword}}.
  Prompts the user for systems, which it returns. "
  (mapcar
   #'(lambda (Name) (read-from-string (format nil ":~A" Name)))
   (select-item-from-list
    (mapcar #'make::component-name (make:defined-systems))
    :window-title "Select Systems:"
    :selection-type :disjoint)))


(defun BM-DO-DIALOG-SYSTEM-SEARCH (System String) "
  in: System {keyword}, String {string}.
  Search through all source files in <System> for <String>.
  Hacked version of Bill's BOYER-MORE.LISP in MCL:Examples"
  (let ((files (make:files-in-system System)))
    (unless files
      (return-from BM-DO-DIALOG-SYSTEM-SEARCH
        (message-dialog (format nil "No files in system ~A" System))))
    (let* ((dialog (select-item-from-list
                    nil
                    :window-title (format nil "Files of system ~A containing ~s" System string)
                    :modeless t
                    :action-function
                    #'(lambda (list)
                        (ccl::maybe-start-isearch (ed (car list)) string))))
           (sequence (car (subviews dialog 'sequence-dialog-item)))
           (button (default-button dialog)))
      (set-cell-font sequence #@(0 0) :italic)
      (set-table-sequence sequence (list (car files)))
      (flet ((f (file index)
               (cond ((null file)
                      (set-cell-font sequence #@(0 0) nil)
                      (set-table-sequence sequence (cdr (table-sequence sequence))))
                     ((eq index t)
                      (setf (car (table-sequence sequence)) file)
                      (redraw-cell sequence #@(0 0)))
                     (index
                      (let ((sel (ccl::first-selected-cell sequence)))
                        (when sel
                          (cell-deselect sequence sel))
                        (set-table-sequence 
                         sequence (nconc (table-sequence sequence) (list file)))
                        (cell-select sequence (or sel #@(0 1))))
                      (dialog-item-enable button)
                      nil)
                     (t nil))))
        (declare (dynamic-extent f))
        (ccl::bm-find-string-in-files string files #'f)))))


(defun SEARCH-SYSTEMS (String &optional (Systems (get-list-of-systems-from-user))) "
  in: String {string}, 
      &optional Systems {list of: {keyword}} default (get-list-of-systems-from-user).
  Search all files in all <Systems> for <String>."
  (dolist (System Systems)
    (bm-do-dialog-system-search System String)))


;;; insert "Search Systems" menu item after "Search Files" menu item in "Tools" menu
(let* ((File-Menu (find-menu "Tools"))
       (Search-File-Menu-Item (find-menu-item File-Menu "Search Files")))
  (unless (find-menu-item File-Menu "Search Systems…")
    (let ((New-Items nil) (Old-Items (menu-items File-Menu)))
      (apply #'remove-menu-items File-Menu (menu-items File-Menu))
      (apply #'add-menu-items File-Menu
             (dolist (Item Old-Items (reverse New-Items))
               (push Item New-Items)
               (when (equal Item Search-File-Menu-Item)
                 (push 
                  (make-instance
                    'menu-item
                    :menu-item-title "Search Systems…"     
                    :menu-item-action 
                    #'(lambda () (eval-enqueue 
                                  '(search-systems 
                                    (get-string-from-user "System Search String:")))))
                  New-Items)))))))


#| Examples:
(search-systems "make-instance")
|#