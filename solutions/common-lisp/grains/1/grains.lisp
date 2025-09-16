(defpackage :grains
  (:use :cl)
  (:export :square :total))
(in-package :grains)

(defun square (n) 
  (cond ((> n 64) nil)
        ((<= n 0) nil)
        (T (expt 2 (- n 1)))))

(defun total () 
  (do ((x 1 (+ x 1))
       (sumv 0 (+ sumv (square x))))
       ((> x 64) sumv)
    ))

  
