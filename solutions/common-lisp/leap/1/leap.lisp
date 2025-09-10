(defpackage :leap
  (:use :cl)
  (:export :leap-year-p))

(in-package :leap)

(defun leap-year-p (year)
  (cond ((= (nth-value 0 (mod year 400 )) 0) T)
        ((= (nth-value 0 (mod year 100 )) 0) nil)
        ((= (nth-value 0 (mod year 4 )) 0) T)))

