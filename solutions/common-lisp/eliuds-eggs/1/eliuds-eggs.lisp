(defpackage :eliuds-eggs
  (:use :cl)
  (:export :egg-count))

(in-package :eliuds-eggs)

(defun egg-count (number)
  (cond ((zerop number) 0)
        ((= (logand number 1) 1)
         (+ 1 (egg-count (floor (/ number 2)))))
        (t (egg-count (floor (/ number 2))))))

