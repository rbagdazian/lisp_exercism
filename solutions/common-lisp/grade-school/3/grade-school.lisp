(defpackage :grade-school
  (:use :cl)
  (:export :make-school :add :roster :grade))

(in-package :grade-school)

;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read
;;   https://portacle.github.io or *portacle-help*
;; To report problems and to get help with issues,  please visit
;;   https://github.com/portacle/portacle/issues
;; 
;; You can use this buffer for notes and tinkering with code.

(defun make-school ()
  (make-array '(12) :initial-element nil))
(defun add (school name grade)
  (when (not (member name (roster school) :test #'string=))
    (push name (aref school (1- grade)))))
(defun roster (school)
  (loop for i from 1 to 12
        append (grade school i)))
(defun grade (school n)
  (sort (aref school (1- n)) #'string<))


