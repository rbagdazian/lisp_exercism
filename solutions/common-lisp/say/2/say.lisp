(defpackage :say
  (:use :cl)
  (:export :say))

(in-package :say)

(defvar *thash* (make-hash-table))

(defun say (number)
  (when (and (>= number 0)
             (< number 1000000000000))
    (format nil "~r" number)))
