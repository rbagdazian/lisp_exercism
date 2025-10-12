(defpackage :triangle
  (:use :cl)
  (:export :triangle-type-p))

(in-package :triangle)

(defun triangle-type-p (type a b c)
  "Deterimines if a triangle (given by side lengths A, B, C) is of the given TYPE"
  (if (= a b c 0) nil
  (if (and
       (>= (+ a b) c)
       (>= (+ b c) a)
       (>= (+ a c) b))
      (cond ((and (eql type :equilateral) (= a b c) ) t)
            ((and (or (= a b) (= b c) (= a c)) (eql type :isosceles)) t)
            ((and (/= a b) (/= b c) (/= a c) (eql type :scalene)) t)
            ( t nil)))))
