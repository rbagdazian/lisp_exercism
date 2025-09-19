(defpackage :lucys-magnificent-mapper
  (:use :cl)
  (:export :make-magnificent-maybe :only-the-best))

(in-package :lucys-magnificent-mapper)

;; Define make-magnificent-maybe function
(defun make-magnificent-maybe (func seq)
  (mapcar func seq))

;; Define only-the-best function
(defun only-the-best (func seq)
  (let ((tmp nil)) 
    (setf tmp (remove-if func seq))
    (remove 1 tmp)))


