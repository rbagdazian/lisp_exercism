(defpackage :two-fer
  (:use :cl)
  (:export :twofer))
(in-package :two-fer)

(defun twofer (&optional (name nil))
  (format nil "One for ~A, one for me." 
    (if (null name) 
      "you"
      name)))