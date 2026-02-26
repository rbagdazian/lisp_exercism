(defpackage :resistor-color-duo
  (:use :cl)
  (:export :value))

(in-package :resistor-color-duo)

(defparameter *colors*
         (list '("black" . 0) '("brown" . 1) '("red" . 2) '("orange" . 3) '("yellow" . 4)
           '("green" . 5) '("blue" . 6) '("violet" . 7) '("grey" . 8) '("white" . 9)))

(defun value (colors)
  (+ (* (cdr (assoc (car colors)  *colors* :test #'string-equal)) 10)
         (cdr (assoc (cadr colors) *colors* :test #'string-equal)) ))
