(defpackage :queen-attack
  (:use :cl)
  (:export :valid-position-p
           :attackp))

(in-package :queen-attack)

(defun valid-position-p (coordinates)
 (and (<= 0 (car coordinates) 7) (<= 0 (cdr coordinates) 7)))

(defun attackp (white-queen black-queen)
 (cond ((or 
        (= (car white-queen) (car black-queen))
        (= (cdr white-queen) (cdr black-queen))) t)
        ((= (abs (- (car white-queen) (car black-queen)))
         (abs (- (cdr white-queen) (cdr black-queen)))) t)
        (t nil)))
   
