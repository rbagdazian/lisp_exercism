(defpackage :palindrome-products
  (:use :cl)
  (:export :smallest
           :largest))

(in-package :palindrome-products)


(defun smallest (min max)
  (when (< min max)
         (loop :for i :from (expt min 2) :to (expt max 2)
               :when (palindromep i min max)
               :return (values i (factors i min max)))))

(defun largest (min max)
  (when (< min max)
         (loop :for i :from (expt max 2) :downto (expt min 2)
               :when (palindromep i min max)
               :return (values i (factors i min max)))))

(defun palindromep (num min max)
  (let ((numstr (format nil "~A" num)))
    (when (string= numstr (reverse numstr))
      (loop :for factor :from min to max
            :when (multiple-value-bind (q r) (floor num factor)
                     (and (zerop r) (<= min q) (<= q max) ))
            :return t))))

(defun factors (n min max)
  (loop for factor from min to (isqrt n) with q and r
        do (multiple-value-setq (q r) (floor n factor))
        when (and (zerop r) (<= min q) (<= q max))
        collect (list factor q)))





       
  



