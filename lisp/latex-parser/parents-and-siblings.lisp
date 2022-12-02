;;;   -*-   Mode: LISP -*-    ;;;
 
 

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;


(in-package :aster)

;;; Thu Dec 24 08:33:36 EST 1992
;;; Parents and siblings for document objects
 
;;; Method: LINK-CHILDREN-TO-PARENT Author: raman
;;; Created: Tue Dec 22 17:26:12 1992

(defmethod link-children-to-parent ((current  math-object))
  "Create parent links"
  (let
      ((children (children current ))
       (attributes (attributes current ))
       (contents (contents current ))
       (parent (parent current )))
    (unless    (equal 'root parent )
      (unless parent                    ;mark as linked
        (setf (parent current) 'root ))
      (when (math-array-p contents)
        (setf (parent contents) current)
        (link-children-to-parent contents ))
      (mapc #'link-children-to-parent attributes )
      (link-siblings  children)
      (loop for child in children
            do
               (unless (equal  'undefined (parent child ))
                 (setf (parent child)   current)
                 (link-children-to-parent child)
                 ))
      )
    current)
  )

(defmethod link-children-to-parent ((current factorial))
  "Link up factorial objects."
  (let ((contents (contents current )))
    (setf (parent contents) current)
    (link-children-to-parent contents )
    (call-next-method))
  )
;;; math-subformula has contents no children.
;;; Hence needs a different method for linking

(defmethod link-children-to-parent ((current  math-subformula))
  "Create parent links"
  (let
      ((contents  (contents current ))
       (attributes (attributes current ))
       (parent (parent current )))
    (unless    (equal 'root parent )
      (unless parent                    ;mark as linked
        (setf (parent current) 'root ))
      (mapc #'link-children-to-parent attributes )  
      (unless (equal  'undefined (parent contents ))
        (setf (parent contents)   current)
        (link-children-to-parent contents)
        ))
    current)
  )


  ;;; Method: LINK-CHILDREN-TO-PARENT Author: raman
  ;;; Created: Wed Dec 23 09:21:27 1992

(defmethod link-children-to-parent ((attribute attribute))
  "Do not need to link parents for attributes, just link children"
  (link-children-to-parent (attribute-value attribute ))
  (setf (parent (attribute-value attribute )) attribute )
  attribute)

  ;;; Method: link-children-to-parent Author: raman
  ;;; Created: Fri Dec 25 10:37:39 1992

(defmethod link-children-to-parent ((article article))
  "Handle abstract and initial body correctly"
  (let
      ((abstract (abstract article ))
       (parent (parent article ))
       (contents (contents article ))
       (sections (children article )))
    (unless    (equal 'root parent )
      (unless parent                    ;mark as linked
        (setf (parent article) 'root ))
      (when abstract 
        (setf  (parent abstract) article)
        (link-children-to-parent abstract))
      (link-siblings contents)
      (loop for block  in contents
            do
            (unless (equal  'undefined (parent block ))
              (setf (parent block ) article)
              (link-children-to-parent block )))
      (link-siblings sections)
      (loop for section in sections
            do
            (setf (parent section) article)
            (link-children-to-parent section )))
    article)
  )


  ;;; Method: LINK-CHILDREN-TO-PARENT                          Author: raman
  ;;; Created: Fri Dec 25 10:50:56 1992

(defmethod link-children-to-parent ((abstract abstract))
  "Link in children for abstract"
  (let ((contents (contents abstract )))
    (link-siblings contents)
    (loop for block in contents
          do
          (unless (equal  'undefined (parent block ))
            (setf (parent block) abstract)
            (link-children-to-parent block )))
    abstract    )
  )

(defmethod link-children-to-parent ((current  document))
  "Create parent links"
  (let
      ((children (children current ))
       (contents (contents current))
       (parent (parent current )))
    (unless    (equal 'root parent )
      (unless parent                    ;mark as linked
        (setf (parent current) 'root ))
      (unless (equal 'undefined contents )
        (when (listp contents)
          (link-siblings contents )
          (loop for block in contents
                do
                   (unless (equal  'undefined (parent block ))
                     (setf (parent block) current)
                       (link-children-to-parent block )))))
      (unless (equal 'undefined children ) 
        (link-siblings children)
        (loop for child in children
              do
                 (unless (equal  'undefined (parent child ))
                   (setf (parent child)   current)
                   (link-children-to-parent child)))))
    current))

(defmethod link-children-to-parent :after ((footnote footnote ))
  "Clean up links around footnote"
  (setf (parent footnote)  (previous footnote ))
  )



  ;;; Method: LINK-CHILDREN-TO-PARENT                          Author: raman
  ;;; Created: Tue Jan 12 14:00:53 1993

(defmethod link-children-to-parent ((table table-mix-in ))
  "Link elements of a table"
  (let ((contents (contents table )))
    (loop for row in contents do
          (loop for table-element  in row do
                (setf (parent  table-element) table )
                (link-children-to-parent table-element)
                ))                      ; parent  linked
    (loop for row in contents  do
                                        ; link left and right, ie
                                        ; previous  and next
          (link-siblings row ))
                                        ; Now link above and below
    (flet
        ((link-above-and-below (column)
           (when  (every #'table-element-p column )
                                        ; link only if it is safe
             (let
                 ((top  (first column )))
               (loop for current in (rest column) 
                     do
                     (setf (table-element-below  top) current)
                     (setf (table-element-above  current) top )
                     (setf top current))
               column   ))
           )                            ; end local function
         (extract-column (nested-list column-position )
           (mapcar #'(lambda(local-row)
                       (when (<= column-position (length local-row ))
                         (elt local-row
                              (- column-position 1 ))))
                   nested-list))        ; end local function
         )
      (let ((row-length (length (first contents ))))
        (when (every #'(lambda(row)
                         (= row-length (length row ))) contents )
                                        ; link only if well formed array
          (loop for i from 1 to row-length
                do
                (link-above-and-below (extract-column contents i )))
          )
        )
      )
    table     )
  )

(defmethod link-children-to-parent ((current table-element ))
  "Link contents of table element "
  (let ((contents (contents current )))
    (unless (equal  'undefined (parent contents ))
      (setf (parent contents ) current )
      (link-children-to-parent contents ))
    current )
  )

  ;;; Method: LINK-SIBLINGS                                    Author: raman
  ;;; Created: Fri Dec 25 13:43:31 1992

(defmethod link-siblings ((children  list))
  "Link siblings "
  (when (next-and-previous-defined-p children) 
    (let
        ((previous (first children )))
      (loop for current in (rest children) 
            do
            (setf (next previous) current)
            (setf (previous current) previous )
            (setf previous current))
      children   ))
  )


  ;;; Function: NEXT-AND-PREVIOUS-DEFINED-P                    Author: raman
  ;;; Created: Fri Dec 25 13:57:25 1992

(defun next-and-previous-defined-p (list) 
  "Validate items of list"
  (notany #'(lambda(item)
              (or (equal 'undefined (next item ))
                  (equal 'undefined (previous item ))))
          list)
  )

;;{{{ default parent and sibling methods catch all: 

  ;;; Method: PARENT                                           Author: raman
  ;;; Created: Wed Dec 23 09:04:08 1992

(defmethod parent ((object t))
  "Default parent method"
  'undefined
  )

(defmethod children ((object t))
  'undefined
  )

(defmethod contents ((object t))
  'undefined
  )

(defmethod previous ((object t))
  'undefined )

(defmethod next ((object  t))
  'undefined )

;;}}}
