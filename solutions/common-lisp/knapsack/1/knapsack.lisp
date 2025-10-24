(defpackage :knapsack
  (:use :cl)
  (:export :maximum-value))

(in-package :knapsack)

(defun decimal-to-binary-list (n)
  "Converts a positive decimal integer N to a list of its binary digits (1s and 0s)."
  (cond ((zerop n) '(0)) ; Base case: if n is 0, return a list containing 0
        ((= n 1) '(1))   ; Base case: if n is 1, return a list containing 1
        (t (cons (rem n 2) ; Cons the remainder of n divided by 2
                 (decimal-to-binary-list (floor n 2))))))

(defun select-items (combo items res)  
  (cond ((null combo) res)
        ((= (car combo) 1) (setf res (append res (list (car items)))) 
                           (select-items (cdr combo) (cdr items) res))
        (t (select-items (cdr combo) (cdr items) res))
    )) 

(defun get-load (setx maxwt)
   (let ((wsum 0) (vsum 0))
        (loop for pair in setx do
              (setf vsum (+ vsum (cdr (assoc :value pair))))
              (setf wsum (+ wsum (cdr (assoc :weight pair))))
        )
        (when (<= wsum maxwt)
          (list wsum vsum)
          )
        ) 
  )
  
(defun maximum-value (maximum-weight items &aux combos sets (load nil) (maxwt 0) (maxval 0))
  (if (null items) 0
      (progn
        (setf combos (loop for i from 1 to (1- (expt 2 (length items))) 
                           collect (decimal-to-binary-list i )))
        (setf sets (loop for i from 0 to (1- (length combos)) collect
          (select-items (nth i combos) items '())))
        (loop for i from 0 to (1- (length sets)) do
              (setf load (get-load (nth i sets) maximum-weight))
              (when (and load (> (cadr load) maxval))                                            
                  (setf maxwt (car load))
                  (setf maxval (cadr load))
                  ))        
        maxval
        ))
  )
        