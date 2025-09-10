(defpackage :collatz-conjecture
  (:use :cl)
  (:export :collatz))

(in-package :collatz-conjecture)

(defun iterateCollatz (n)
  (cond ((= (car n) 1) n)
        ((evenp (car n)) (iterateCollatz (cons (/ (car n) 2) n)))
        (T (iterateCollatz (cons (+ (* (car n) 3) 1) n)))))

(defun collatz (n)
  (cond ((<= n 0) nil)
      (T (- (length (iterateCollatz (list n))) 1))))
  
