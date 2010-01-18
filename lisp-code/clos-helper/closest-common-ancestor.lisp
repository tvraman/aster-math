;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)
(use-package :clos)

;;; Sun Apr  4 19:11:55 EDT 1993
;;; Compute the closest common ancestor of a list of objects.
;;; <(solution obtained in response to posting on comp.lang.clos)>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun closest-common-superclass (list-o-instances)
  (loop with candidates = (reduce #'intersection
                                  (mapcar #'(lambda (i)
                                              (class-precedence-list
                                               (class-of i)))
                                          list-o-instances))
        ;; Too bad INTERSECTION doesn't maintain order
        with best-candidate = (find-class 'T)
        for (current-candidate . remaining-candidates) on candidates
        when (and (subtypep current-candidate best-candidate)
                  (every #'(lambda (other-candidate)
                             (subtypep current-candidate other-candidate))
                         remaining-candidates))
          do (setf best-candidate current-candidate)
        finally (return best-candidate )))

;Examples of use:
;
;(least-common-ancestor '(2))
;  => #<BUILT-IN-CLASS INTEGER>
;
;(least-common-ancestor '(2 3/4))
;  => #<BUILT-IN-CLASS RATIONAL>
;
;(least-common-ancestor '(2 3/4 1.0))
;  => #<BUILT-IN-CLASS REAL>
;
;(least-common-ancestor '(2 3/4 1.0 #C(5 6)))
;  => #<BUILT-IN-CLASS NUMBER>
;
;(least-common-ancestor '(2 3/4 1.0 #C(5 6) SEVEN))
;  => #<BUILT-IN-CLASS T>
;
;There are more efficient, but less obvious solutions, especially if
;you known something special about the shape of the relevant class
;lattice (maybe that's why you included a specific set of classes as an
;example).
;
;John Burger
;john@mitre.org
;
;
;
