(defpackage :largest-series-product
  (:use :cl)
  (:export :largest-product))

(in-package :largest-series-product)

(defun getprod (str &aux (prod 1) (v nil))
  (if (> (length str) 0)
      (loop for i from 0 to (1- (length str)) do
        (when (null (setf v (digit-char-p (char str i))))
          (setf prod nil) (return))          
        (setf prod (* prod v))
        )
       (setf prod 0))
  prod
  )

(defun maxprod (lst)
  (if (null (car lst)) 
  nil
     (if (= (length lst) 1) (car lst)
        (reduce #'max lst))
  ))

(defun largest-product (digits span &aux (diglist '()) (prodlist '()) (maxv nil)) 
  " parse the digits into a list"
  (when (and (>= (length digits) span) (> span 0))
    (loop for i from 0 to (- (length digits) span)  do
       (setf diglist (push (subseq digits i (+ i span)) diglist))
       (setf prodlist (push (getprod (car diglist)) prodlist))
      (if (null (car prodlist)) (return))
    )
    (setf maxv (maxprod prodlist))
    )
  
  maxv
)
