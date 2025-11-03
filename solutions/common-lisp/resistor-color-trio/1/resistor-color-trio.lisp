(defpackage :resistor-color-trio
  (:use :cl)
  (:export :label))

(in-package :resistor-color-trio)


(defparameter *colors*
         (list '("black" . 0) '("brown" . 1) '("red" . 2) '("orange" . 3) '("yellow" . 4)
           '("green" . 5) '("blue" . 6) '("violet" . 7) '("grey" . 8) '("white" . 9)))

(defun value (colors)
  (+ (* (cdr (assoc (car colors)  *colors* :test #'string-equal)) 10)
         (cdr (assoc (cadr colors) *colors* :test #'string-equal)) ))

(defun label (colors)
  (let ((v (* (value colors) (expt 10 (cdr (assoc (caddr colors) *colors* :test #'string-equal))))))
  (cond ((and (>= v 1000) (< v 1000000)) (format nil "~A kiloohm~:p" (/ v 1000)))
    ((and (>= v 1000000) (< v 1000000000)) (format nil "~A megaohm~:p" (/ v 1000000)))
    ((>= v 1000000000) (format nil "~A gigaohm~:p" (/ v 1000000000)))    
    (t (format nil "~A ohm~:p" v)))))
