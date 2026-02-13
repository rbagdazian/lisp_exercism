(defpackage :darts
  (:use :cl)
  (:export :score))

(in-package :darts)

(defun score (x y)
  (let ((range (sqrt (+ (* x x) (* y y))) ))
       (cond ((> range 10) 0)
         ((> range 5) 1)
         ((> range 1) 5)
         (t 10))))
