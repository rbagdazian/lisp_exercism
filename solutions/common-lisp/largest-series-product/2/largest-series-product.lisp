(defpackage :largest-series-product
  (:use :cl)
  (:export :largest-product))

(in-package :largest-series-product)

(defun string-to-product (seq)
  (apply #'* (map 'list #'digit-char-p seq)))

(defun only-digits-p (s)
  "Returns T if string S contains only digit characters, NIL otherwise."
  (loop for char across s
        always (digit-char-p char)))

(defun largest-product (digits span &aux (diglist '()) (prodlist '()) (maxv nil)) 
  " parse the digits into a list"
  (when (only-digits-p digits)
    (if (or (> span (length digits)) (< span 1)) 
    (setf maxv nil)
    (when (and (>= (length digits) span) (> span 0))
      (setf maxv 0)
      (loop for i from 0 to (- (length digits) span)  do       
         (setf maxv (max maxv (string-to-product (subseq digits i (+ i span))))))      
    ))  
    maxv)
)
