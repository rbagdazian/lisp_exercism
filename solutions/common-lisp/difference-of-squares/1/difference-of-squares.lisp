(defpackage :difference-of-squares
  (:use :cl)
  (:export :sum-of-squares
           :square-of-sum
           :difference))

(in-package :difference-of-squares)

(defun sumn (n)
  (cond ((zerop n) n)
        (T (+ n (sumn (- n 1))))))

(defun sumsq (n)
  (cond ((zerop n) n)
        (T (+ (expt n 2) (sumsq (- n 1))))))


(defun square-of-sum (n)
  "Calculates the square of the sum for a given number."
  (expt (sumn n) 2))
  

(defun sum-of-squares (n)
  "Calculates the sum of squares for a given number."
      (sumsq n)
  )

(defun difference (n)
  "Finds the diff. between the square of the sum and the sum of the squares."
  (- (square-of-sum n) (sum-of-squares n)))
  
